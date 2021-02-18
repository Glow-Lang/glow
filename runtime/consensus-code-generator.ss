(export #t)

(import
  :std/sort :std/srfi/1 :std/misc/hash :std/misc/list :std/misc/number
  :clan/base :clan/number :clan/syntax
  :clan/poo/io :clan/poo/object :clan/poo/brace :clan/poo/debug
  :mukn/ethereum/ethereum :mukn/ethereum/assembly :mukn/ethereum/contract-runtime :mukn/ethereum/signing
  :mukn/ethereum/assets :mukn/ethereum/types
  ./program)

(define-type ConsensusCodeGenerator
  (.+
    (Record
      program: [Any] ; Program
      name: [Symbol]
      variable-offsets: [(OrFalse (Map Nat <- Symbol))]
      labels: [(OrFalse (Map Nat <- Symbol))]
      bytes: [(OrFalse Bytes)]
      params-end: [(OrFalse Nat)])
    {.make:
      (lambda (some-program some-name some-timeout)
        {program: some-program
         name: some-name
         variable-offsets: #f
         labels: #f
         bytes: #f
         params-end: #f
         timeout: some-timeout})
     .generate:
      (lambda (self)
        (parameterize ((brk-start (box params-start@)))
          (.set! self variable-offsets (make-hash-table))
          (.set! self params-end params-start@)
          ;; NB: Scratch space begins at the memory location after the maximum size of all the medium
          ;; functions / code block frames. Therefore, medium functions need to be defined before small
          ;; functions, since small functions use the scratch space to store temporary variables.
          (def compiled-medium-functions (&define-medium-functions self))
          (def compiled-small-functions (&define-small-functions self (@ (.@ self program) small-functions)))
          ;; NB: you can use #t below to debug with remix.ethereum.org. Do NOT commit that!
          ;; TODO: maybe we should have some more formal debugging mode parameter?
          (def debug #t)
          (defvalues (bytes-value labels-value)
            (assemble
              (&begin
                (&simple-contract-prelude)
                &define-tail-call
                &define-simple-logging
                (&define-check-participant-or-timeout debug: debug)
                (&define-end-contract debug: debug)
                compiled-small-functions
                compiled-medium-functions
                [&label 'brk-start@ (unbox (brk-start))])))
          (.set! self bytes bytes-value)
          (.set! self labels labels-value)))
    }))

;; Directives to generate bytecode for small functions, usually defined in the header.
;; Directive <- ConsensusCodeGenerator (Map SmallFunction <- Symbol)
(def (&define-small-functions self small-functions)
  (&begin*
    (map (match <>
            ([function-name . definition]
              (hash-put! (.@ self variable-offsets) function-name (make-hash-table))
              (def store-instructions
                (map (λ (argument-name)
                        (def type (lookup-type (.@ self program) argument-name))
                        (def len (and type (param-length type)))
                        (def offset (add-temporary-variable-offset self function-name argument-name))
                        (&mstoreat offset len))
                  (.@ definition arguments)))
              (def compiled-statements
                (map (λ (statement) (compile-consensus-statement self function-name statement))
                      (.@ definition body)))
              (def body-bytes
                (&begin*
                  (append
                    (flatten1 store-instructions)
                    (flatten1 compiled-statements))))
              (&define-small-function function-name body-bytes)))
         (hash->list/sort small-functions symbol<?))))

;; Directives to generate the entire bytecode for the contract (minus header / footer)
;; Directive <- ConsensusCodeGenerator
(def (&define-medium-functions self)
  (def consensus-interaction (get-interaction (.@ self program) (.@ self name) #f))
  (&begin*
    (append-map (match <> ([code-block-label . code-block]
                          (generate-consensus-code-block self code-block-label code-block)))
                (hash->list/sort consensus-interaction symbol<?))))

;; Directives from a code block
;; (List Directive) <- ConsensusCodeGenerator Symbol CodeBlock
(def (generate-consensus-code-block self code-block-label code-block)
  (def checkpoint-statements (code-block-statements code-block))
  (def frame-size (compute-variable-offsets self code-block-label))
  (def code-block-directives
    [[&jumpdest (make-checkpoint-label (.@ self name) code-block-label)]
      (&check-timeout! timeout: (.@ self timeout))
      (append-map (λ (statement) (compile-consensus-statement self code-block-label statement))
                checkpoint-statements)...])
  (register-frame-size frame-size)
  (def end-code-block-directive
    (if (equal? code-block-label (get-last-code-block-label (.@ self program) (.@ self name)))
      &end-contract!
      (setup-tail-call self code-block-label code-block)))
  (snoc end-code-block-directive code-block-directives))

;; : Symbol <- Symbol Symbol
(def (make-checkpoint-label name checkpoint)
  (unless (symbol? name)
    (error 'make-checkpoint-label "expected a symbol, given" name))
  (symbolify name "--" checkpoint))

;; (List Directive) <- ConsensusCodeGenerator Symbol Sexp
(def (compile-consensus-statement self function-name statement)

  (match statement
    (['set-participant new-participant]
      ;; TODO: support more than two participants
      (let (other-participant (find-other-participant self new-participant))
        [(&check-participant-or-timeout!
          must-act: (lookup-variable-offset self function-name new-participant)
          or-end-in-favor-of: (lookup-variable-offset self function-name other-participant))]))

    ;; TODO: support the fact that the "immediate continuation" for an expression
    ;; may be not just def, but also ignore or return
    (['def variable-name expression]
      (add-local-variable-to-frame self function-name variable-name)
      (def type (lookup-type (.@ self program) variable-name))
      (def len (and type (param-length type)))
      (match expression
        (['expect-published published-variable-name]
          [len (lookup-variable-offset self function-name variable-name) &read-published-data-to-mem])
        (else
          (append
            (compile-consensus-expression self function-name expression)
            [(&mstoreat (lookup-variable-offset self function-name variable-name) len)]))))

    (['require! variable-name]
      [(load-immediate-variable self function-name variable-name Bool) &require!])

    (['expect-deposited amount]
      [(load-immediate-variable self function-name amount Ether) &deposit!])

    (['consensus:withdraw participant amount]
      [(load-immediate-variable self function-name amount Ether)
       (load-immediate-variable self function-name participant Address)
       &withdraw!])

   (['return ['@tuple]]
      [])

    (['return expression]
      (compile-consensus-expression self function-name expression))

    (['@label _]
      [])
    (['@debug-label _]
      [])

    (['switch value cases ...]
      (let*
        ((comparison-value (trivial-expression self function-name value))
        (interpreted-cases (map (λ (case)
                                  (let (interpreted-statements (map (λ (case-statement)
                                                                    (compile-consensus-statement self function-name case-statement)) (cdr case)))
                                  [(car case) (flatten1 interpreted-statements)])) cases)))
      [(&switch comparison-value interpreted-cases)]))

    (else
      (error "Runtime does not recognize consensus statement: " statement))))

;; ASSUMING a two-participant contract, find the other participant for use in timeouts.
;; Symbol <- ConsensusCodeGenerator Symbol
(def (find-other-participant self participant)
  (find
    (λ (p) (and (not (equal? #f p)) (not (equal? p participant))))
    (hash-keys (@ (.@ self program) interactions))))

(def (typed-directive<-trivial-expr self function-name expr)
  (def program (.@ self program))
  (cons (lookup-type program expr) (trivial-expression self function-name expr)))

;; Directive <- ConsensusCodeGenerator Symbol Symbol Any
(def (compile-consensus-expression self function-name expression)
  (def (binary-operator op a b)
    [(trivial-expression self function-name b)
     (trivial-expression self function-name a)
     op])

  (match expression

    (['@app 'isValidSignature participant digest signature]
      [(load-immediate-variable self function-name participant Address)
       (load-immediate-variable self function-name digest Digest)
        ;; signatures are passed by reference, not by value
       (lookup-variable-offset self function-name signature)
       &isValidSignature])

    (['digest . exprs]
      [(&digest<-tvps (map (cut typed-directive<-trivial-expr self function-name <>) exprs))])

    (['== a b]
      (binary-operator EQ a b))

    ;; TODO: Make sure contract and runtime handle overflows in the same way.
    (['@app '< a b]
      (binary-operator LT a b))

    (['@app '> a b]
      (binary-operator GT a b))

    (['@app '+ a b]
      (binary-operator ADD a b))

    (['@app '- a b]
      (binary-operator SUB a b))

    (['@app '* a b]
      (binary-operator MUL a b))

    (['@app '/ a b]
      (binary-operator DIV a b))

    (['@app 'bitwise-xor a b]
      (binary-operator XOR a b))

    (['@app 'bitwise-and a b]
      (binary-operator AND a b))

    (['@app 'mod a b]
      (binary-operator MOD a b))

    ;; WARNING: This does not support re-entrancy!
    ;; TODO: Handle re-entrancy.
    (['@app inner-function-name argument-names ...]
      (unless (hash-get (@ (.@ self program) small-functions) inner-function-name)
        (error "Unknown function " inner-function-name))
      (let (arguments (map (λ (argument-name) (trivial-expression self function-name argument-name)) argument-names))
        [(apply &call inner-function-name arguments)]))

    (['input _ _]
      [REVERT])

    (else
      [(trivial-expression self function-name expression)])))

;; Offset <- ConsensusCodeGenerator Symbol Symbol
(def (lookup-variable-offset self function-name variable-name)
  (hash-get (hash-get (.@ self variable-offsets) function-name) variable-name))

;; Assembly directives to load an immediate variable (i.e. for unboxed type) onto the stack
;; : Directives <- ConsensusCodeGenerator Symbol Symbol Type
(def (load-immediate-variable self function-name variable-name variable-type)
  (def offset (lookup-variable-offset self function-name variable-name))
  (cond
    ((integer? offset)
      (&mloadat offset (param-length variable-type)))
    (else
      (error "Unknown offset for " variable-name " in function " function-name))))

;; TODO: params-end should be the MAX of each frame's params-end.
;; Updates variable offsets to account for new local variable, and increments params-end
;; <- ConsensusCodeGenerator Symbol Symbol
(def (add-local-variable-to-frame self function-name variable-name)
  (def type (lookup-type (.@ self program) variable-name))
  (def argument-length (param-length type))
  (def function-variable-offsets (hash-get (.@ self variable-offsets) function-name))
  ;; The same variable can be bound in several branches of an if or match expression, and
  ;; because of the ANF transformation we can assume the previously assigned offset is
  ;; correct already.
  (unless (hash-key? function-variable-offsets variable-name)
    (hash-put! function-variable-offsets variable-name
      (post-increment! (.@ self params-end) argument-length))))

;; Offset <- ConsensusCodeGenerator Symbol Symbol
(def (add-temporary-variable-offset self function-name variable-name)
  (def function-offsets (hash-get (.@ self variable-offsets) function-name))
  (def type (lookup-type (.@ self program) variable-name))
  (def argument-length (param-length type))
  (def offset (post-increment! (.@ self params-end) argument-length))
  (hash-put! (hash-get (.@ self variable-offsets) function-name) variable-name offset)
  offset)

;; Directives to load onto stack a representation for a trivial expression
;; use load-immediate-variable for types passed by value
;; use lookup-variable-offset for types passed by reference (Signature)
;; otherwise place a constant on the stack
;; : Directives <- ConsensusCodeGenerator Symbol Any
(def (trivial-expression self function-name expr)
  (def type (lookup-type (.@ self program) expr))
  (def len (param-length type))
  (cond
    ((zero? len) 0) ;; TODO: have a more general notion of singleton/unit type, not only 0-valued?
    ((<= len 32) ;; TODO: have a more general notion of immediate vs boxed type?
    (if (symbol? expr)
      (load-immediate-variable self function-name expr type) ;; reading a variable
      (nat<-bytes (bytes<- type expr)))) ;; constant
    (else
    (if (symbol? expr)
      (lookup-variable-offset self function-name expr) ;; referring to a variable by offset
      ;; TODO: store the data in a variable (temporary, if needed) --- do that in ANF after typesetting.
      (error "trivial-expression: oversize constant" (.@ type sexp) expr)))))

;; Directive <- ConsensusCodeGenerator Symbol CodeBlock
(def (setup-tail-call self code-block-label code-block)
  (let* ((next-code-block-label (code-block-exit code-block))
         (next-code-block-live-variables (sort (lookup-live-variables (.@ self program) (.@ self name) next-code-block-label) symbol<?))
         (next-code-block-frame-size (+ (param-length UInt16) (param-length Block)))) ;; 2 for program counter, 2 for timer start,
    (&begin
      (&begin*
        (map
          (λ (lv)
            ;; TODO: Make sure this works for non-immediate variables.
            (def variable-size (param-length (lookup-type (.@ self program) lv)))
            (&mloadat
              (lookup-variable-offset self code-block-label lv)
              variable-size))
          (reverse next-code-block-live-variables)))
      ;; TODO: If other guy, put NUMBER, otherwise, reuse current timer-start.
      NUMBER
      ;; TODO: Define only once for every callee.
      ;; [&jumpdest (symbolify 'tail-call-into- next-code-block-label)]
      (make-checkpoint-label (.@ self name) next-code-block-label) pc-set!
      timer-start-set!
      (&begin*
        (map
          (λ (lv)
            ;; TODO: Make sure this works for non-immediate variables.
            (def variable-size (param-length (lookup-type (.@ self program) lv)))
            (set! next-code-block-frame-size (+ next-code-block-frame-size variable-size))
            (&mstoreat
              (lookup-variable-offset self next-code-block-label lv)
              variable-size))
          next-code-block-live-variables))
      next-code-block-frame-size
      'tail-call JUMP)))

;; Offset <- ConsensusCodeGenerator Symbol
(def (compute-variable-offsets self code-block-label)
  (def frame-variables (make-hash-table))
  ;; Initial offset computed by global registers, see :mukn/ethereum/contract-runtime
  (def frame-size params-start@)
  (def live-variables (lookup-live-variables (.@ self program) (.@ self name) code-block-label))
  (for-each
    (λ (live-variable)
      (let* ((type (lookup-type (.@ self program) live-variable))
             (parameter-length (param-length type)))
        (hash-put! frame-variables live-variable (post-increment! frame-size parameter-length))))
    (sort live-variables symbol<?))
  (hash-put! (.@ self variable-offsets) code-block-label frame-variables)
  (when (or (not (.@ self params-end)) (> frame-size (.@ self params-end)))
    (.set! self params-end frame-size))
  frame-size)

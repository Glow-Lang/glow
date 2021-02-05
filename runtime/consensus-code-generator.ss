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
      variable-offsets: [(OrFalse (Map Nat <- Symbol))]
      labels: [(OrFalse (Map Nat <- Symbol))]
      bytes: [(OrFalse Bytes)]
      params-end: [(OrFalse Nat)])
    {.make:
      (lambda (some-program some-timeout)
        {program: some-program
         variable-offsets: #f
         labels: #f
         bytes: #f
         params-end: #f
         timeout: some-timeout})
     .generate:
      (lambda (self)
        (parameterize ((brk-start (box params-start@)))
          (def consensus-code (generate-consensus-code self))
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
                (&define-small-functions self (@ (.@ self program) small-functions))
                consensus-code
                [&label 'brk-start@ (unbox (brk-start))])))
          (.set! self bytes bytes-value)
          (.set! self labels labels-value)))
    }))

(def (&define-small-functions self small-functions)
  (&begin*
    (map (λ (sf) (match sf
          ([name . definition]
            (def body-bytes
              (&begin*
                (map (λ (statement) (compile-consensus-statement self name statement))
                     (.@ definition body))))
            (&define-small-function name body-bytes))))
         (hash->list/sort small-functions symbol<?))))

;; Directives to generate the entire bytecode for the contract (minus header / footer)
;; Directive <- ConsensusCodeGenerator
(def (generate-consensus-code self)
  (def consensus-interaction (get-interaction (.@ self program) #f))
  (.set! self variable-offsets (make-hash-table))
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
    [[&jumpdest (make-checkpoint-label (.@ self program) code-block-label)]
      (&check-timeout! timeout: (.@ self timeout))
      (append-map (λ (statement) (compile-consensus-statement self code-block-label statement))
                checkpoint-statements)...])
  (register-frame-size frame-size)
  (def end-code-block-directive
    (if (equal? code-block-label (get-last-code-block-label (.@ self program)))
      &end-contract!
      (setup-tail-call self code-block-label code-block)))
  (snoc end-code-block-directive code-block-directives))

;; : Symbol <- Program Symbol
(def (make-checkpoint-label program checkpoint)
  (symbolify (@ program name) "--" checkpoint))

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
      (compile-consensus-expression self function-name variable-name expression))

    (['require! variable-name]
      [(load-immediate-variable self function-name variable-name Bool) &require!])

    (['expect-deposited amount]
      [(load-immediate-variable self function-name amount Ether) &deposit!])

    (['consensus:withdraw participant amount]
      [(load-immediate-variable self function-name amount Ether)
       (load-immediate-variable self function-name participant Address)
       &withdraw!])

    (['return _]
      [])

    (['@label _]
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
(def (compile-consensus-expression self function-name variable-name expression)
  (def type (lookup-type (.@ self program) variable-name))
  (def len (and type (param-length type)))
  (def (binary-operator op a b)
    [(trivial-expression self function-name b)
     (trivial-expression self function-name a)
     op
     (&mstoreat (lookup-variable-offset self function-name variable-name) len)])
  (match expression
    (['expect-published published-variable-name]
      [len (lookup-variable-offset self function-name variable-name) &read-published-data-to-mem])
    (['@app 'isValidSignature participant digest signature]
      [(load-immediate-variable self function-name participant Address)
       (load-immediate-variable self function-name digest Digest)
        ;; signatures are passed by reference, not by value
       (lookup-variable-offset self function-name signature)
       &isValidSignature
       (&mstoreat (lookup-variable-offset self function-name variable-name) 1)])
    (['digest . exprs]
      [(&digest<-tvps (map (cut typed-directive<-trivial-expr self function-name <>) exprs))
       (&mstoreat (lookup-variable-offset self function-name variable-name) len)])
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
    (['input _ _]
      [REVERT])))

;; Offset <- ConsensusCodeGenerator Symbol Symbol
(def (lookup-temporary-variable-offset self function-name variable-name)
  (def function-offsets (hash-get (.@ self variable-offsets) function-name))
  (let* ((type (lookup-type (.@ self program) variable-name))
         (computed-offset (.@ self params-end))
         (argument-length (param-length type)))
    (hash-put! (hash-get (.@ self variable-offsets) function-name) variable-name
      (post-increment! (.@ self params-end) argument-length))
    computed-offset))

(def (lookup-variable-offset self function-name variable-name)
  (or (hash-get (hash-get (.@ self variable-offsets) function-name) variable-name)
      (lookup-temporary-variable-offset self function-name variable-name)))

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
  (def code-block-variable-offsets (hash-get (.@ self variable-offsets) function-name))
  ;; The same variable can be bound in several branches of an if or match expression, and
  ;; because of the ANF transformation we can assume the previously assigned offset is
  ;; correct already.
  (unless (hash-key? code-block-variable-offsets variable-name)
    (hash-put! code-block-variable-offsets variable-name
      (post-increment! (.@ self params-end) argument-length))))

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
         (next-code-block-live-variables (sort (lookup-live-variables (.@ self program) next-code-block-label) symbol<?))
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
      (make-checkpoint-label (.@ self program) next-code-block-label) pc-set!
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
  (def live-variables (lookup-live-variables (.@ self program) code-block-label))
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

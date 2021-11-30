(export #t)

(import
  :std/iter :std/sort :std/sugar :std/assert :std/srfi/1 :std/misc/hash :std/misc/list :std/misc/number
  :clan/base :clan/number :clan/syntax
  :clan/poo/io :clan/poo/object :clan/poo/brace :clan/poo/debug
  :mukn/ethereum/ethereum :mukn/ethereum/assembly :mukn/ethereum/evm-runtime
  :mukn/ethereum/assets :mukn/ethereum/types
  (only-in :mukn/glow/compiler/common hash-kref)
  ./program)

(define-type ConsensusCodeGenerator
  (.+
    (Record
      program: [Program]
      name: [Symbol]
      static-block-ctx: [StaticBlockCtx]
      variable-offsets: [(OrFalse (Map (Map Nat <- Symbol) <- Symbol))]
      labels: [(OrFalse (Map Nat <- Symbol))]
      bytes: [(OrFalse Bytes)]

      ; TODO: rename params-end; its actual meaning is all statically-allocated
      ; space in the executable, incl. globals, so the name should reflect that.
      params-end: [(OrFalse Nat)]
      timeout: [Nat])
    {.make:
      (lambda (some-program some-name some-timeout assets)
        {program: some-program
         name: some-name
         static-block-ctx: (.call StaticBlockCtx .make some-program some-name assets)
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
          ;; NB: the order in which we build medium and small functions is important, because they mutate
          ;; brk-start in place. &define-medium-functions will allocate the maximum space needed by all
          ;; medium functions, and &define-small-functions will allocate the *sum* of the space needed
          ;; by all small functions, since the latter can be live at the same time (but so far we do
          ;; not support recursion, so we don't need to support more than one frame of the same function).
          ;; When computing the medium frames, we do this by setting params-end to the max of its current
          ;; value and what is needed for the frame we're working on, so if we did small functions first
          ;; this could go awry.
          ;; TODO(cleanup): change the way this works internally so this isn't so delecate.
          (def compiled-medium-functions (&define-medium-functions self))
          (def compiled-small-functions (&define-small-functions self (.@ self program small-functions)))
          ;; After the above have run, params-end will be set to the end of the space they need, so
          ;; update brk-start.
          ;; TODO: add space for global variables
          (set-box! (brk-start) (.@ self params-end))

          (def sbc (.@ self static-block-ctx))
          (def assets-and-vars
             (map (lambda (kv)
                    (cons (.call StaticBlockCtx .get-asset sbc (car kv))
                          (cdr kv)))
                  (hash->list/sort (.@ sbc balance-vars) symbol<?)))

          (defvalues (bytes-value labels-value)
            (assemble
              (&begin
                (&simple-contract-prelude)
                &define-tail-call
                (&define-commit-contract-call/simple self)
                (&define-check-participant-or-timeout assets-and-vars timeout: (.@ self timeout))
                (&define-end-contract assets-and-vars)
                compiled-small-functions
                compiled-medium-functions
                [&label 'brk-start@ (unbox (brk-start))])))
          (.set! self bytes bytes-value)
          (.set! self labels labels-value)))
    }))

;; TODO: rename this; it has nothing to do with blocks.
(define-type StaticBlockCtx
  {
    ;; .deposit-var : [StaticBlockCtx Symbol -> StaticVar]
    .deposit-var:
      (lambda (sbc sym)
        (hash-kref (.@ sbc deposit-vars) sym))
    .balance-var:
      (lambda (sbc sym)
        (hash-kref (.@ sbc balance-vars) sym))
    ;; .withdraw-var : [StaticBlockCtx Symbol Symbol -> StaticVar]
    .withdraw-var:
      (lambda (sbc asset-sym participant)
        (hash-kref (.@ sbc withdraw-vars) [asset-sym participant]))

    ;; .get-asset-names : [StaticBlockCtx -> [Listof Symbol]]
    .get-asset-names: (lambda (sbc) (.@ sbc asset-names))
    ;; .get-participant-names : [StaticBlockCtx -> [Listof Symbol]]
    .get-participant-names: (lambda (sbc) (.@ sbc participant-names))
    ;; .get-asset : [StaticBlockCtx Symbol -> Asset]
    .get-asset: (lambda (sbc sym) (.ref (.@ sbc assets) sym))
    ;; .&get-deposit : [StaticBlockCtx Symbol -> EvmThunk]
    .&get-deposit: (lambda (sbc sym) (.@ (.call StaticBlockCtx .deposit-var sbc sym) get))
    ;; .&add-deposit! : [StaticBlockCtx Symbol -> EvmThunk]
    .&add-deposit!: (lambda (sbc sym)
                     (&add-var! (.call StaticBlockCtx .deposit-var sbc sym)))
    ;; .&get-withdraw : [StaticBlockCtx Symbol Symbol -> EvmThunk]
    .&get-withdraw: (lambda (sbc asset-sym participant)
                     (.@ (.call StaticBlockCtx .withdraw-var sbc asset-sym participant)
                         get))
    ;; .&add-withdraw! : [StaticBlockCtx Symbol Symbol -> EvmThunk]
    .&add-withdraw!: (lambda (sbc asset-sym participant)
                      (&add-var! (.call StaticBlockCtx .withdraw-var sbc asset-sym participant)))
    .make: (lambda (program name assets)
             (def inter (hash-kref (.@ program interactions) name))
             (def asset-names (.@ inter asset-names))
             (def participant-names (.@ inter participant-names))
             (unless (length<=n? asset-names MAX_ASSETS)
               (error "too many assets"))
             (unless (length<=n? participant-names MAX_PARTICIPANTS)
               (error "too many participants"))
             (def asset-participant-names
               (cartesian-product asset-names participant-names))
             (def my-deposit-vars
               (list->hash-table-eq
                (for/collect ((n asset-names)
                              (d deposit-vars))
                  [n . d])))
             (def my-balance-vars
               (list->hash-table-eq
                (for/collect ((n asset-names)
                              (b balance-vars)
                              (i (iota (length balance-vars))))
                  (let
                    ((v (.mix b)))
                      ;; Keep track of the balance variable's
                      ;; index, so we get the memory layout right
                      ;; elsewhere:
                      (set! (.@ v index) i)
                      [n . v]))))
             (def my-withdraw-vars
               (list->hash-table
                (for/collect ((np asset-participant-names)
                              (w withdraw-vars))
                  (cons np w))))
             { asset-names participant-names assets
               deposit-vars: my-deposit-vars
               balance-vars: my-balance-vars
               withdraw-vars: my-withdraw-vars }) })

;; Logging the data, simple version, optimal for messages less than 6000 bytes of data.
;; TESTING STATUS: Used by buy-sig.
(def (&define-commit-contract-call/simple self)
  (def sbc (.@ self static-block-ctx))
  (def assets (.call StaticBlockCtx .get-asset-names sbc))
  (def participants (.call StaticBlockCtx .get-participant-names sbc))
  (def initial-label
    (.@ (hash-kref (.@ self program interactions) (.@ self name))
        initial-code-block-label))
  (&begin
   [&jumpdest 'commit-contract-call] ;; -- return-address
   ;; First, check deposits
   (&begin*
    (for/collect ((an assets))
     (def a (.call StaticBlockCtx .get-asset sbc an))
     (def amount (.call StaticBlockCtx .&get-deposit sbc an))
     (&begin
       (.call a .commit-deposit! amount)
       amount
       (&add-var! (.call StaticBlockCtx .balance-var sbc an)))))
   ;; For each participant, commit the withdrawls
   (&begin*
    (flatten1
     (for/collect ((an assets))
      (for/collect ((pn participants))
        (def a (.call StaticBlockCtx .get-asset sbc an))
        (def p (load-immediate-variable self initial-label pn Address))
        (.call a .commit-withdraw!
               p
               (.call StaticBlockCtx .&get-withdraw sbc an pn)
               (.call StaticBlockCtx .balance-var sbc an))))))
   calldatanew DUP1 CALLDATASIZE SUB ;; -- logsz cdn ret
   SWAP1 ;; -- cdn logsz ret
   DUP2 ;; logsz cdn logsz ret
   0 SWAP2 ;; -- cdn logsz 0 logsz ret
   DUP3 ;; -- 0 cdn logsz 0 logsz ret
   CALLDATACOPY ;; -- 0 logsz ret
   LOG0 JUMP))

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
  (def checkpoint-statements (.@ code-block statements))
  (def frame-size (compute-variable-offsets self code-block-label))
  (def code-block-directives
    [[&jumpdest (make-checkpoint-label (.@ self name) code-block-label)]
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
        ;; timeout argument is passed into `&define-check-participant-or-timeout`, not here
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

    (['expect-deposited ['@record [asset-symbol amount]]]
      (def asset (.call StaticBlockCtx .get-asset (.@ self static-block-ctx) asset-symbol))
      (def &add-deposit (.call StaticBlockCtx .&add-deposit! (.@ self static-block-ctx) asset-symbol))
      [(load-immediate-variable self function-name amount asset)
       (.call StaticBlockCtx .&add-deposit! (.@ self static-block-ctx) asset-symbol)])

    (['consensus:withdraw participant ['@record [asset-symbol amount]]]
      (def asset (.call StaticBlockCtx .get-asset (.@ self static-block-ctx) asset-symbol))
      [(load-immediate-variable self function-name amount asset)
       (.call StaticBlockCtx .&add-withdraw! (.@ self static-block-ctx) asset-symbol participant)])

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
  (let*
    ((interactions (.@ self program interactions))
     (entry (hash-kref interactions (.@ self name))))
    (find
      (λ (p) (and (not (equal? #f p)) (not (equal? p participant))))
      (hash-keys (.@ entry specific-interactions)))))

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
      (unless (hash-get (.@ self program small-functions) inner-function-name)
        (error "Unknown function " inner-function-name))
      (let (arguments (map (λ (argument-name) (trivial-expression self function-name argument-name)) argument-names))
        [(apply &call inner-function-name arguments)]))

    (['input _ _]
      [REVERT])

    (else
      [(trivial-expression self function-name expression)])))

;; Offset <- ConsensusCodeGenerator Symbol Symbol
(def (lookup-variable-offset self function-name variable-name)
  (hash-kref (hash-kref (.@ self variable-offsets) function-name) variable-name))

;; Assembly directives to load an immediate variable (i.e. for unboxed type) onto the stack
;; : Directives <- ConsensusCodeGenerator Symbol Symbol Type
(def (load-immediate-variable self function-name variable-name variable-type)
  (def offset (lookup-variable-offset self function-name variable-name))
  (cond
    ((integer? offset)
      (&mloadat offset (param-length variable-type)))
    (else
      (error "Unknown offset for " variable-name " in function " function-name))))

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
  (let* ((next-code-block-label (.@ code-block exit))
         (_ (assert! (symbol? next-code-block-label)))
         (next-code-block-live-variables (sort (lookup-live-variables (.@ self program) (.@ self name) next-code-block-label) symbol<?))
         (next-code-block-frame-size (- params-start@ frame@)))
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
  (assert! (symbol? code-block-label))
  (def frame-variables (make-hash-table))
  ;; Initial offset computed by global registers, see :mukn/ethereum/evm-runtime
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

;; cartesian-product : [Listof A] ... -> [Listof [List A ...]]
(def (cartesian-product . lol) (cartesian-product* lol))
;; TODO : move cartesian-product into gerbil-utils or gerbil, or find a suitable
;; replacement if one exists (e.g. in an SRFI).

;; cartesian-product* : [List [Listof A] ...] -> [Listof [List A ...]]
(def (cartesian-product* lol)
  (match lol
    ([] [[]])
    ([[] . _] [])
    ([l . rst]
     (let (rst* (cartesian-product* rst))
       (flatten1
        (for/collect (e l)
          (for/collect (r rst*)
            (cons e r))))))))

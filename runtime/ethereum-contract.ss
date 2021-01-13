(export #t)

(import
  :std/iter :std/sugar :std/misc/hash :std/misc/list :std/misc/number :std/srfi/1 :std/sort
  :clan/number :clan/syntax :clan/poo/io
  :mukn/ethereum/hex :mukn/ethereum/types :mukn/ethereum/ethereum :mukn/ethereum/signing
  :mukn/ethereum/assembly :mukn/ethereum/contract-runtime :mukn/ethereum/assets
  ./program
  ../compiler/project/runtime-2)

;; (deftype DependentPair (Pair t:Type Value:t))

;; TODO: key variable-offsets with (Pair frame:Symbol variable:Symbol), or move it to CodeBlock!
(defclass Contract
  (program ;; : Program ;; from program.ss
   participants ;; : (Table Address <- Symbol)
   arguments ;; : (Table DependentPair <- Symbol)
   variable-offsets ;; : (Table (Table Offset <- Symbol) <- Symbol)
   params-end) ;; : Offset
  transparent: #t)

;; (deftype Frame (List DependentPair))

;; : Frame <- Contract Block (Table Offset <- Symbol) Symbol
(defmethod {create-frame-variables Contract}
  (λ (self timer-start contract-runtime-labels code-block-label code-block-participant)
    (def checkpoint-location
      (hash-get contract-runtime-labels {make-checkpoint-label self code-block-label}))
    (def active-participant-offset
      {lookup-variable-offset self code-block-label code-block-participant})
    ;; TODO: ensure keys are sorted in both hash-values
    [[UInt16 . checkpoint-location]
     [Block . timer-start]
     ;; [UInt16 . active-participant-offset]
     ;; TODO: designate participant addresses as global variables that are stored outside of frames
     (map (λ (kv) (cons Address (cdr kv))) (hash->list/sort (@ self participants) symbol<?))...
     (map cdr (hash->list/sort (@ self arguments) symbol<?))...]))

;; Block <- Frame
(def (timer-start<-frame-variables frame-variables)
  (cdadr frame-variables))

;; TODO: use [t . v] everywhere instead of [v t] ? and unify with sexp<-state in ethereum-runtime
;; Sexp <- Frame
(def (sexp<-frame-variables frame-variables)
  `(list ,@(map (match <> ([v t] `(list ,(sexp<- t v) ,(sexp<- Type t)))) frame-variables)))


;; TODO: Exclude checkpoints that have already been executed by the first active
;; participant.
;; : Bytes (Table Offset <- Symbol) <- Contract
(defmethod {generate-consensus-runtime Contract}
  (λ (self timeout)
    (parameterize ((brk-start (box params-start@)))
      (def consensus-code {generate-consensus-code self timeout})
      (assemble
        (&begin
        (&simple-contract-prelude)
        &define-simple-logging
        (&define-check-participant-or-timeout)
        ;; NB: you can use #t below to debug with remix.ethereum.org. Do NOT commit that!
        ;; TODO: maybe we should have some more formal debugging mode parameter?
        (&define-end-contract debug: #f)
        consensus-code
        [&label 'brk-start@ (unbox (brk-start))])))))

;; : Bytes (Table Offset <- Symbol) <- Contract
(defmethod {make-checkpoint-label Contract}
  (λ (self checkpoint)
    (symbolify (@ self program name) "--" checkpoint)))

;; <- Contract
(defmethod {compute-variable-offsets Contract}
  (λ (self code-block-label)
    (def frame-variables (make-hash-table))
    ;; Initial offset computed by global registers, see :mukn/ethereum/contract-runtime
    (def start params-start@)
    (for-each (match <>
                ([role . address]
                 (let (parameter-length (param-length Address))
                   (hash-put! frame-variables
                              role (post-increment! start parameter-length)))))
              (hash->list/sort (@ self participants) symbol<?))
    (def live-variables (sort {lookup-live-variables (@ self program) code-block-label} symbol<?))
    (for-each
      (λ (live-variable)
        (let (type {lookup-type (@ self program) live-variable})
          (when (and type (not (hash-get frame-variables live-variable)))
            (let (parameter-length (param-length type))
              (hash-put! frame-variables live-variable (post-increment! start parameter-length))))))
      live-variables)
    (hash-put! (@ self variable-offsets) code-block-label frame-variables)
    (set! (@ self params-end) start)))

;; Offset <- Contract Symbol
(defmethod {lookup-variable-offset Contract}
  (λ (self code-block-label variable-name)
    (def offset
      (hash-get (hash-get (@ self variable-offsets) code-block-label) variable-name))
    (if offset
      offset
      (error "No offset for variable: " variable-name))))

;; Assembly directives to load an immediate variable (i.e. for unboxed type) onto the stack
;; : Directives <- Contract Symbol Type
(defmethod {load-immediate-variable Contract}
  (λ (self code-block-label variable-name variable-type)
    (&mloadat
      {lookup-variable-offset self code-block-label variable-name}
      (param-length variable-type))))

;; Directives to load onto stack a representation for a trivial expression
;; use load-immediate-variable for types passed by value
;; use lookup-variable-offset for types passed by reference (Signature)
;; otherwise place a constant on the stack
;; : Directives <- Contract
(defmethod {trivial-expression Contract}
  (λ (self code-block-label expr)
    (def type {lookup-type (@ self program) expr})
    (def len (param-length type))
    (cond
     ((zero? len) 0) ;; TODO: have a more general notion of singleton/unit type, not only 0-valued?
     ((<= len 32) ;; TODO: have a more general notion of immediate vs boxed type?
      (if (symbol? expr)
        {load-immediate-variable self code-block-label expr type} ;; reading a variable
        (nat<-bytes (bytes<- type expr)))) ;; constant
     (else
      (if (symbol? expr)
        {lookup-variable-offset self code-block-label expr} ;; referring to a variable by offset
        ;; TODO: store the data in a variable (temporary, if needed) --- do that in ANF after typesetting.
        (error "trivial-expression: oversize constant" (.@ type sexp) expr))))))

;; TODO: params-end should be the MAX of each frame's params-end.
;; Updates variable offsets to account for new local variable, and increments params-end
;; <- Contract Symbol
(defmethod {add-local-variable-to-frame Contract}
  (λ (self code-block-label variable-name)
    (def type {lookup-type (@ self program) variable-name})
    (def argument-length (param-length type))
    (def code-block-variable-offsets (hash-get (@ self variable-offsets) code-block-label))
    ;; The same variable can be bound in several branches of an if or match expression, and
    ;; because of the ANF transformation we can assume the previously assigned offset is
    ;; correct already.
    (unless (hash-key? code-block-variable-offsets variable-name)
      (hash-put! code-block-variable-offsets variable-name
        (post-increment! (@ self params-end) argument-length)))))

;; Directives to generate the entire bytecode for the contract (minus header / footer)
;; Directive <- Contract
(defmethod {generate-consensus-code Contract}
  (λ (self timeout)
    (def consensus-interaction {get-interaction (@ self program) #f})
    (set! (@ self variable-offsets) (make-hash-table))
    (&begin*
     (append-map (match <> ([code-block-label . code-block]
                            {generate-consensus-code-block self code-block-label code-block timeout}))
                 (hash->list/sort consensus-interaction symbol<?)))))

;; Directives from a code block
;; (List Directive) <- Contract Symbol CodeBlock
(defmethod {generate-consensus-code-block Contract}
  (λ (self code-block-label code-block timeout)
    (def checkpoint-statements (code-block-statements code-block))
    (set! (@ self params-end) #f)
    {compute-variable-offsets self code-block-label}
    (def code-block-directives
      [[&jumpdest {make-checkpoint-label self code-block-label}]
       (&check-timeout! timeout: timeout)
       (append-map (λ (statement) {interpret-consensus-statement self code-block-label statement})
                 checkpoint-statements)...])
    (register-frame-size (@ self params-end))
    (def end-code-block-directive
      (if (equal? code-block-label {get-last-code-block-label (@ self program)})
        &end-contract!
        (&begin
          &start-timer!
          ;; TODO: Store call frame in storage before committing. See targets defined in contract-runtime/&define-tail-call
          STOP)))
    (snoc end-code-block-directive code-block-directives)))

;; ASSUMING a two-participant contract, find the other participant for use in timeouts.
;; Symbol <- Contract Symbol
(defmethod {find-other-participant Contract}
  (λ (self participant)
    (find
      (λ (p) (not (equal? p participant)))
      (hash-keys (@ self participants)))))

;; (List Directive) <- Contract Sexp
(defmethod {interpret-consensus-statement Contract}
  (λ (self code-block-label statement)
    (match statement
      (['set-participant new-participant]
       ;; TODO: support more than two participants
       (let (other-participant {find-other-participant self new-participant})
         [(&check-participant-or-timeout!
           must-act: {lookup-variable-offset self code-block-label new-participant}
           or-end-in-favor-of: {lookup-variable-offset self code-block-label other-participant})]))

      ;; TODO: support the fact that the "immediate continuation" for an expression
      ;; may be not just def, but also ignore or return
      (['def variable-name expression]
       {add-local-variable-to-frame self code-block-label variable-name}
       {interpret-consensus-expression self code-block-label variable-name expression})

      (['require! variable-name]
       [{load-immediate-variable self code-block-label variable-name Bool} &require!])

      (['expect-deposited amount]
       [{load-immediate-variable self code-block-label amount Ether} &deposit!])

      (['consensus:withdraw participant amount]
       [{load-immediate-variable self code-block-label amount Ether}
        {load-immediate-variable self code-block-label participant Address}
        &withdraw!])

      (['return _]
        [])

      (['@label _]
        [])

      (['switch value cases ...]
        (let*
          ((comparison-value {trivial-expression self code-block-label value})
           (interpreted-cases (map (λ (case)
             (let (interpreted-statements (map (λ (case-statement)
                    {interpret-consensus-statement self code-block-label case-statement}) (cdr case)))
              [(car case) (flatten1 interpreted-statements)])) cases)))
        [(&switch comparison-value interpreted-cases)]))

      (else
       (error "Contract does not recognize consensus statement: " statement)))))

(def (typed-directive<-trivial-expr Contract code-block-label expr)
  (def program (@ Contract program))
  (cons {lookup-type program expr} {trivial-expression Contract code-block-label expr}))

(defmethod {interpret-consensus-expression Contract}
  (λ (self code-block-label variable-name expression)
    (def type {lookup-type (@ self program) variable-name})
    (def len (and type (param-length type)))
    (def (binary-operator op a b)
      [{trivial-expression self code-block-label b}
       {trivial-expression self code-block-label a}
       op])
    (match expression
      (['expect-published published-variable-name]
        [len {lookup-variable-offset self code-block-label variable-name} &read-published-data-to-mem])
      (['@app 'isValidSignature participant digest signature]
        [{load-immediate-variable self code-block-label participant Address}
          {load-immediate-variable self code-block-label digest Digest}
          ;; signatures are passed by reference, not by value
          {lookup-variable-offset self code-block-label signature}
          &isValidSignature
          (&mstoreat {lookup-variable-offset self code-block-label variable-name} 1)])
      (['digest . exprs]
        [(&digest<-tvps (map (cut typed-directive<-trivial-expr self code-block-label <>) exprs))])
      (['== a b]
        (binary-operator EQ a b))
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
        (binary-operator AND a b)))))

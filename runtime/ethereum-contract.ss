(export #t)

(import
  :std/iter :std/sugar :std/misc/hash :std/misc/list :std/misc/number :std/srfi/1
  :clan/syntax
  :mukn/ethereum/assembly :mukn/ethereum/ethereum :mukn/ethereum/assets
  :mukn/ethereum/signing :mukn/ethereum/contract-runtime
  ./program
  ../compiler/project/runtime-2)

;; (deftype DependentPair (Pair t:Type Value:t))

(defclass Contract
  (program ;; : Program ;; from program.ss
   participants ;; : (Table Address <- Symbol)
   arguments ;; : (Table DependentPair <- Symbol)
   variable-offsets ;; : (Table Offset <- Symbol)
   params-end ;; : Offset
   initial-timer-start ;; : Block
   timeout) ;; : Block
  transparent: #t)

;; (deftype Frame (List DependentPair))

;; : Frame <- Contract Block (Table Offset <- Symbol) Symbol
(defmethod {create-frame-variables Contract}
  (λ (self timer-start contract-runtime-labels checkpoint)
    (def checkpoint-location
      (hash-get contract-runtime-labels {make-checkpoint-label self checkpoint}))
    ;; TODO: ensure keys are sorted in both hash-values
    [[UInt16 . checkpoint-location]
     [Block . timer-start]
     (map (lambda (kv) (cons Address (cdr kv))) (hash->list/sort (@ self participants) symbol<?))...
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
  (λ (self)
    (set! (@ self params-end) #f)
    {compute-parameter-offsets self}
    (parameterize ((brk-start (box params-start@)))
      (assemble
        (&begin
         &simple-contract-prelude
         &define-simple-logging
         (&define-check-participant-or-timeout)
         (&define-end-contract)
         {generate-consensus-code self}
         [&label 'brk-start@ (unbox (brk-start))])))))

;; : Bytes (Table Offset <- Symbol) <- Contract
(defmethod {make-checkpoint-label Contract}
  (λ (self checkpoint)
    (symbolify (@ self program name) "--" checkpoint)))

;; <- Contract
(defmethod {compute-parameter-offsets Contract}
  (λ (self)
    (def frame-variables (make-hash-table))
    ;; Initial offset computed by global registers, see :mukn/ethereum/contract-runtime
    (def start params-start@)
    (for-each (match <>
                ([role . address]
                 (let (parameter-length (param-length Address))
                   (hash-put! frame-variables
                              role (post-increment! start parameter-length)))))
              (hash->list/sort (@ self participants) symbol<?))
    (for-each (match <>
                ([variable type . value]
                 (let (argument-length (param-length type))
                   (hash-put! frame-variables
                              variable (post-increment! start argument-length)))))
              (hash->list/sort (@ self arguments) symbol<?))
    (set! (@ self variable-offsets) frame-variables)
    (set! (@ self params-end) start)))

;; Offset <- Contract Symbol
(defmethod {lookup-variable-offset Contract}
  (λ (self variable-name)
    (def offset
      (hash-get (@ self variable-offsets) variable-name))
    (if offset
      offset
      (error "No offset for variable: " variable-name))))

;; Assembly directives to load an immediate variable (i.e. for unboxed type) onto the stack
;; : Directives <- Contract Symbol Type
(defmethod {load-immediate-variable Contract}
  (λ (self variable-name variable-type)
    (&mloadat
      {lookup-variable-offset self variable-name}
      (param-length variable-type))))

;; Directives to load onto stack a representation for a trivial expression
;; use load-immediate-variable for types passed by value
;; use lookup-variable-offset for types passed by reference (Signature)
;; otherwise place a constant on the stack
;; : Directives <- Contract
(defmethod {trivial-expression Contract}
  (λ (self expr)
    (def type {lookup-type (@ self program) expr})
    (cond
      ((symbol? expr)
       (cond ((member (.@ type sexp) '(Signature (Tuple UInt256 UInt256)))
              {lookup-variable-offset self expr type})
             (else
              {load-immediate-variable self expr type})))
      ((integer? expr)
       expr)
      (else
       (error 'trivial-expression "unknown" expr type)))))

;; TODO: params-end should be the MAX of each frame's params-end.
;; Updates variable offsets to account for new local variable, and increments params-end
;; <- Contract Symbol
(defmethod {add-local-variable-to-frame Contract}
  (λ (self variable-name)
    (def type {lookup-type (@ self program) variable-name})
    (def argument-length (param-length type))
    (when (hash-key? (@ self variable-offsets) variable-name)
      (error "variable already added!" variable-name))
    (hash-put! (@ self variable-offsets)
      variable-name (post-increment! (@ self params-end) argument-length))))

;; Directives to generate the entire bytecode for the contract (minus header / footer)
;; Directive <- Contract
(defmethod {generate-consensus-code Contract}
  (λ (self)
    (def consensus-interaction {get-interaction (@ self program) #f})
    (&begin*
     (append-map (match <> ([checkpoint . code-block]
                            {generate-consensus-code-block self checkpoint code-block}))
                 (hash->list/sort consensus-interaction symbol<?)))))

;; Directives from a code block
;; (List Directive) <- Contract Symbol CodeBlock
(defmethod {generate-consensus-code-block Contract}
  (λ (self checkpoint code-block)
    (def checkpoint-statements (code-block-statements code-block))
    [[&jumpdest {make-checkpoint-label self checkpoint}]
     (append-map (λ (statement) {interpret-consensus-statement self statement})
                 checkpoint-statements)...]))

;; ASSUMING a two-participant contract, find the other participant for use in timeouts.
;; Symbol <- Contract Symbol
(defmethod {find-other-participant Contract}
  (λ (self participant)
    (find
      (λ (p) (not (equal? p participant)))
      (hash-keys (@ self participants)))))

;; (List Directive) <- Contract Sexp
(defmethod {interpret-consensus-statement Contract}
  (λ (self statement)
    (match statement
      (['set-participant new-participant]
       ;; TODO: support more than two participants
       (let (other-participant {find-other-participant self new-participant})
         [(&check-participant-or-timeout!
           must-act: {lookup-variable-offset self new-participant}
           or-end-in-favor-of: {lookup-variable-offset self other-participant})]))

      (['def variable-name expression]
       {add-local-variable-to-frame self variable-name}
       (match expression
         (['expect-published published-variable-name]
          [{lookup-variable-offset self variable-name} &read-published-data-to-mem])
         ;; TODO: digest
         (['@app 'isValidSignature participant digest signature]
          [{load-immediate-variable self participant Address}
           {load-immediate-variable self digest Digest}
           ;; signatures are passed by reference, not by value
           {lookup-variable-offset self signature}
           &isValidSignature])
         (['@app '< a b]
          [{trivial-expression self a}
           {trivial-expression self b}
           LT])))

      (['require! variable-name]
       [{load-immediate-variable self variable-name Bool} &require!])

      (['expect-deposited amount]
       ;; TODO: check that this amount was deposited by the active participant
       [])

      (['consensus:withdraw participant amount]
       [{load-immediate-variable self amount Ether}
        {load-immediate-variable self participant Address}
        &withdraw!])

      (['expect-deposited amount]
       ;; TODO: implement
       [])

      (['@label 'end0]
       [&end-contract!])

      (['return ['@tuple]]
       [])

      (else
       (error "Contract does not recognize consensus statement: " statement)))))

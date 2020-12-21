(export #t)

(import
  :std/iter :std/sugar :std/misc/list :std/misc/number
  :mukn/ethereum/assembly :mukn/ethereum/ethereum
  :mukn/ethereum/signing :mukn/ethereum/contract-runtime
  ./program
  ../compiler/project/runtime-2)

(defclass Contract (program participants arguments variable-offsets params-end timeout)
  transparent: #t)

(defmethod {create-frame-variables Contract}
  (λ (self initial-block contract-runtime-labels checkpoint)
    (def checkpoint-location
      (hash-get contract-runtime-labels {make-checkpoint-label self checkpoint}))
    (flatten1
       [[[UInt16 . checkpoint-location]]
        [[Block . initial-block]]
        (map (λ (participant) [Address . participant]) (hash-values (@ self participants)))
        (hash-values (@ self arguments))])))

(def (sexp<-frame-variables frame-variables)
  `(list ,@(map (match <> ([v t] `(list ,(sexp<- t v) ,(sexp<- Type t)))) frame-variables)))

;; TODO: Exclude checkpoints that have already been executed by the first active
;; participant.
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

(defmethod {make-checkpoint-label Contract}
  (λ (self checkpoint)
    (string->symbol (string-append
      (symbol->string (@ (@ self program) name))
      (string-append "--" (symbol->string checkpoint))))))

(defmethod {compute-parameter-offsets Contract}
  (λ (self)
    (def frame-variables (make-hash-table))
    ;; Initial offset computed by global registers, see :mukn/ethereum/contract-runtime
    (def start params-start@)
    (for ((values variable value) (in-hash (@ self participants)))
      (def parameter-length (param-length Address))
      (hash-put! frame-variables
        variable (post-increment! start parameter-length)))
    (for ((values variable dependent-pair) (in-hash (@ self arguments)))
      (def argument-length (param-length (car dependent-pair)))
      (hash-put! frame-variables
        variable (post-increment! start argument-length)))
    (set! (@ self variable-offsets) frame-variables)
    (set! (@ self params-end) start)))

(defmethod {lookup-variable-offset Contract}
  (λ (self variable-name)
    (def offset
      (hash-get (@ self variable-offsets) variable-name))
    (if offset
      offset
      (error "No offset for variable: " variable-name))))

(defmethod {load-variable Contract}
  (λ (self variable-name variable-type)
    (&mloadat
      {lookup-variable-offset self variable-name}
      (param-length variable-type))))

;; use load-variable for types passed by value
;; use lookup-variable-offset for types passed by reference (Signature)
;; otherwise place a constant on the stack
(defmethod {trivial-expression Contract}
  (λ (self expr)
    (def type {lookup-type (@ self program) expr})
    (cond
      ((symbol? expr)
       (cond ((member (.@ type sexp) '(Signature (Tuple UInt256 UInt256)))
              {lookup-variable-offset self expr type})
             (else
              {load-variable self expr type})))
      ((integer? expr)
       expr)
      (else
       (error 'trivial-expression "unknown" expr type)))))

(defmethod {add-local-variable-to-frame Contract}
  (λ (self variable-name)
    (def type {lookup-type (@ self program) variable-name})
    (def argument-length (param-length type))
    (hash-put! (@ self variable-offsets)
      variable-name (post-increment! (@ self params-end) argument-length))))

(defmethod {generate-consensus-code Contract}
  (λ (self)
    (def consensus-interaction {get-interaction (@ self program) #f})
    {compute-parameter-offsets self}
    (&begin*
      (flatten1 (hash-map (λ (checkpoint code-block)
        {generate-consensus-code-block self checkpoint code-block})
        consensus-interaction)))))

(defmethod {generate-consensus-code-block Contract}
  (λ (self checkpoint code-block)
    (def checkpoint-statements (code-block-statements code-block))
    (cons
      [&jumpdest {make-checkpoint-label self checkpoint}]
      (flatten1 (map (λ (statement)
        {interpret-consensus-statement self statement}) checkpoint-statements)))))

(defmethod {find-other-participant Contract}
  (λ (self participant)
    (find
      (λ (p) (not (equal? p participant)))
      (hash-keys (@ self participants)))))

(defmethod {interpret-consensus-statement Contract}
  (λ (self statement)
    (match statement
      (['set-participant new-participant]
        ; TODO: support more than two participants
        (let (other-participant {find-other-participant self new-participant})
        [(&check-participant-or-timeout!
          must-act: {lookup-variable-offset self new-participant}
          or-end-in-favor-of: {lookup-variable-offset self other-participant})]))

      (['def variable-name expression]
        {add-local-variable-to-frame self variable-name}
        (match expression
          (['expect-published published-variable-name]
            [{lookup-variable-offset self variable-name} &read-published-data-to-mem])
          (['@app 'isValidSignature participant digest signature]
            [{load-variable self participant Address}
             {load-variable self digest Digest}
             ; signatures are passed by reference, not by value
             {lookup-variable-offset self signature}
             &isValidSignature])
          (['@app '< a b]
            [{trivial-expression self a}
             {trivial-expression self b}
             LT])))

      (['require! variable-name]
        [{load-variable self variable-name Bool} &require!])

      (['expect-deposited amount]
        (void))
        ; TODO: check that this amount was deposited by the active participant

      (['consensus:withdraw participant amount]
        (void))
        ; TODO: uncomment once issue with EVM bytecode is fixed
        ; [{load-variable self amount Ether}
        ;  {load-variable self participant Address}
        ;  &withdraw!])

      (['expect-deposited amount]
        ;; TODO: implement
        (void))

      (['@label 'end0]
        [&end-contract!])

      (['return ['@tuple]]
        (void))

      (else
       (error "Contract does not recognize consensus statement: " statement)))))

(export #t)

(import
  :std/iter :std/sugar :std/misc/list :std/misc/number
  :mukn/ethereum/assembly :mukn/ethereum/ethereum
  :mukn/ethereum/signing :mukn/ethereum/contract-runtime
  ./program
  ../compiler/project/runtime-2)

(defclass Contract (program participants arguments variable-offsets params-end)
  transparent: #t)

(defmethod {create-frame-variables Contract}
  (λ (self initial-block contract-runtime-labels)
    (def checkpoint-location
      (hash-get contract-runtime-labels {make-checkpoint-label self}))
    (flatten1
       [[[UInt16 . checkpoint-location]]
        [[Block . initial-block]]
        (map (λ (participant) [Address . participant]) (hash-values (@ self participants)))
        (hash-values (@ self arguments))])))

(def (sexp<-frame-variables frame-variables)
  `(list ,@(map (match <> ([v t] `(list ,(sexp<- t v) ,(sexp<- Type t)))) frame-variables)))

(defmethod {generate-consensus-runtime Contract}
  (λ (self)
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

;; TODO: increment counter of checkpoints
(defmethod {make-checkpoint-label Contract}
  (λ (self)
    (def checkpoint-number 0)
    (string->symbol (string-append
      (symbol->string (@ (@ self program) name))
      (string-append "--cp" (number->string checkpoint-number))))))

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

(defmethod {add-local-variable-to-frame Contract}
  (λ (self variable-name)
    (def type {lookup-type (@ self program) variable-name})
    (def argument-length (param-length type))
    (hash-put! (@ self variable-offsets)
      variable-name (post-increment! (@ self params-end) argument-length))))

(defmethod {generate-consensus-code Contract}
  (λ (self)
    (def consensus-interaction
      {get-interaction (@ self program) #f})
    ;; TODO: don't hardcode the checkpoint label
    (def cp0-statements
      (code-block-statements (hash-get consensus-interaction 'cp0)))
    {compute-parameter-offsets self}
    (&begin*
      (cons
        [&jumpdest {make-checkpoint-label self}]
        (flatten1 (map (λ (statement)
          {interpret-consensus-statement self statement}) cp0-statements))))))

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
             &isValidSignature])))

      (['require! variable-name]
        [{load-variable self variable-name Bool} &require!])

      (['consensus:withdraw participant amount]
        (void))
        ; TODO: uncomment once issue with EVM bytecode is fixed
        ; [{load-variable self amount Ether}
        ;  {load-variable self participant Address}
        ;  &withdraw!])

      (['@label 'end0]
        [&end-contract!])

      (['return ['@tuple]]
        (void))

      (else
       (error "Contract does not recognize consensus statement: " statement)))))
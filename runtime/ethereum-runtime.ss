(export #t)

(import
  :gerbil/gambit/bytes
  :std/iter :std/sugar
  :clan/exception :clan/json :clan/poo/io :clan/pure/dict/assq :clan/persist/content-addressing
  :mukn/ethereum/ethereum :mukn/ethereum/network-config  :mukn/ethereum/json-rpc
  :mukn/ethereum/transaction :mukn/ethereum/tx-tracker
  :mukn/ethereum/contract-runtime :mukn/ethereum/contract-config
  ./program
  ./ethereum-contract
  ../compiler/method-resolve/method-resolve
  ../compiler/project/runtime-2)

;; PARTICIPANT RUNTIME
;; TODO: add separate field for most recent transaction seen
(defclass Runtime (contract role contract-state current-code-block current-label environment message)
  constructor: :init!
  transparent: #t)

(defmethod {:init! Runtime}
  (λ (self contract role (cs #f) (ccb 'begin0) (cl 'begin) (e (make-hash-table)) (m (make-Message)))
    (set! (@ self contract) contract)
    (set! (@ self role) role)
    (set! (@ self contract-state) cs)
    (set! (@ self current-code-block) ccb)
    (set! (@ self current-label) cl)
    (set! (@ self environment) e)
    (set! (@ self message) m)
    {initialize-environment self}))

(defmethod {execute Runtime}
  (λ (self)
    (with-logged-exceptions ()
      (def code-block-label (@ self current-code-block))
      (displayln "executing code block: " code-block-label)

      (def code-block {get-current-code-block self})

      {prepare self}
      (for ((statement (code-block-statements code-block)))
        {interpret-participant-statement self statement})
      {commit self}

      (match (code-block-exit code-block)
        (#f
          (void)) ; contract finished
        (exit
          (set! (@ self current-code-block ) exit)
          ; TODO: recurse after seeing new transaction
          ; {execute self}
          )))))

(defmethod {watch Runtime}
  (λ (self)
    ;; TODO: watch blockchain for new transactions against contract
    (void)))

(defmethod {prepare Runtime}
  (λ (self)
    (def contract-state (@ self contract-state))
    ;; TODO: create a sum type for all the contract states
    ;; (NotYetDeployed, DeployedByOtherParticipant, ContractHandshake, Active TransactionReceipt, Complete)
    ;(when )
    ;  {watch self})
    (cond
      ((element? ContractHandshake contract-state)
        (displayln "verifying contract ...")
        (let*
           ((initial-block (.@ contract-state initial-block))
            (contract-config (.@ contract-state contract-config))
            (create-pretx {prepare-create-contract-transaction self initial-block}))
          (verify-contract-config contract-config create-pretx)))
      ((element? TransactionReceipt contract-state)
        (displayln "extracting published data from transaction receipt ...")
        (let
          ;; TODO: handle multiple logs
          (log-data (.@ (car (.@ contract-state logs)) data))
          (set! (@ (@ self message) inbox) (open-input-u8vector log-data))))
      (else
        (void)))))

(defmethod {commit Runtime}
  (λ (self)
    (def contract-state (@ self contract-state))
    ;; TODO: check that previous transaction for expected asset transfers
    (cond
      ((equal? contract-state #f)
        (displayln "deploying contract ...")
        {deploy-contract self})
      ((element? ContractHandshake contract-state)
        (displayln "publishing message ...")
        (let*
          ((initial-block (.@ contract-state initial-block))
           (contract-address (.@ (.@ contract-state contract-config) contract-address))
           (message (@ self message))
           (outbox (@ message outbox))
           (message-pretx {prepare-call-function-transaction self outbox initial-block contract-address})
           (tx-receipt (post-transaction message-pretx)))
          {reset message}
          ;; TODO: remove once we have transaction watching
          (write-file-json "run/tx-receipt.json" (json<- TransactionReceipt tx-receipt)))))))

(defmethod {add-to-environment Runtime}
  (λ (self name value)
    (hash-put! (@ self environment) name value)))

(defmethod {prepare-create-contract-transaction Runtime}
  (λ (self initial-block)
    (def sender-address {get-active-participant self})
    (defvalues (contract-runtime-bytes contract-runtime-labels)
      {generate-consensus-runtime (@ self contract)})
    (def initial-state
      {create-frame-variables (@ self contract) initial-block contract-runtime-labels})
    (def initial-state-digest
      (digest-product-f initial-state))
    (def contract-bytes
      (stateful-contract-init initial-state-digest contract-runtime-bytes))
    (create-contract sender-address contract-bytes
      value: {compute-participant-dues (@ self message) sender-address})))

(defmethod {deploy-contract Runtime}
  (λ (self)
    (def timeoutInBlocks (ethereum-timeout-in-blocks))
    (def initial-block (+ (eth_blockNumber) timeoutInBlocks))
    (def pretx {prepare-create-contract-transaction self initial-block})
    (display-poo ["Deploying contract... " "timeoutInBlocks: " timeoutInBlocks
                  "initial-block: " initial-block "\n"])
    (def receipt (post-transaction pretx))
    (def contract-config (contract-config<-creation-receipt receipt))
    (display-poo ["Contract config: " ContractConfig contract-config "\n"])
    (verify-contract-config contract-config pretx)
    (def handshake (new ContractHandshake initial-block contract-config))
    (display-poo ["Handshake: " ContractHandshake handshake "\n"])
    ;; TODO: display at terminal for user to copy paste and send to
    ;; other participants through outside channel
    (write-file-json "run/contract-handshake.json" (json<- ContractHandshake handshake))))

;; See gerbil-ethereum/contract-runtime.ss for spec.
(defmethod {prepare-call-function-transaction Runtime}
  (λ (self outbox initial-block contract-address)
    (def sender-address {get-active-participant self})
    (defvalues (_ contract-runtime-labels)
      {generate-consensus-runtime (@ self contract)})
    (def frame-variables
      {create-frame-variables (@ self contract) initial-block contract-runtime-labels})
    (def frame-variable-bytes (marshal-product-f frame-variables))
    (def frame-length (bytes-length frame-variable-bytes))
    (def out (open-output-u8vector))
    (marshal UInt16 frame-length out)
    (marshal-product-to frame-variables out)
    (for ([type . value] (hash-values outbox))
      (marshal type value out))
    (marshal UInt8 1 out)
    (def message-bytes (get-output-u8vector out))
    (call-function sender-address contract-address message-bytes
      value: {compute-participant-dues (@ self message) sender-address})))

(defmethod {get-current-code-block Runtime}
  (λ (self)
    (def contract (@ self contract))
    (def participant-interaction
      {get-interaction (@ contract program) (@ self role)})
    (hash-get participant-interaction (@ self current-code-block))))

;; TODO: map alpha-converted names to names in original source when displaying to user
(defmethod {initialize-environment Runtime}
  (lambda (self)
    (def contract (@ self contract))
    (for ((values key value) (in-hash (@ contract participants)))
      {add-to-environment self key value})
    (for ((values key [_ . value]) (in-hash (@ contract arguments)))
      {add-to-environment self key value})))

(defmethod {reduce-expression Runtime}
  (λ (self expression)
    (if (symbol? expression)
      (let (variable-value (hash-get (@ self environment) expression))
        (match variable-value
          ([type . value]
            value)
          (#f
            (error (string-append expression " is missing from execution environment")))
          (value
            value)))
      (expression))))

(defmethod {get-active-participant Runtime}
  (λ (self)
    (def contract (@ self contract))
    (hash-get (@ contract participants) (@ self role))))

(def (marshal-product-f fields)
  (def out (open-output-u8vector))
  (marshal-product-to fields out)
  (get-output-u8vector out))

(def (marshal-product-to fields port)
  (for ((p fields))
    (with (([t . v] p)) (marshal t v port))))

(def (digest-product-f fields)
  (def out (open-output-u8vector))
  (for ((field fields))
    (with (([t . v] field)) (marshal t v out)))
  (digest<-bytes (marshal-product-f fields)))

(defmethod {interpret-participant-statement Runtime}
  (λ (self statement)
    (displayln statement)
    (match statement

      (['set-participant new-participant]
        (let (this-participant (@ self role))
          (unless (eq? new-participant this-participant)
            (error "Change participant inside code block, this should be impossible"))))

      (['add-to-deposit amount-variable]
        (let
          ((this-participant {get-active-participant self})
           (amount {reduce-expression self amount-variable}))
          {add-to-deposit (@ self message) this-participant amount}))

      (['expect-deposited amount-variable]
        (let
          ((this-participant {get-active-participant self})
           (amount {reduce-expression self amount-variable}))
          {expect-deposited (@ self message) this-participant amount}))

      (['participant:withdraw address-variable price-variable]
        (let
          ((address {reduce-expression self address-variable})
           (price {reduce-expression self price-variable}))
          {add-to-withdraw (@ self message) address price}))

      (['add-to-publish ['quote publish-name] variable-name]
        (let
          ((publish-value {reduce-expression self variable-name})
           (publish-type {lookup-type (@ (@ self contract) program) variable-name}))
          {add-to-published (@ self message) publish-name publish-type publish-value}))

      (['def variable-name expression]
        (let (variable-value
          (match expression
            (['expect-published ['quote publish-name]]
              (let (publish-type {lookup-type (@ (@ self contract) program) publish-name})
                {expect-published (@ self message) publish-name publish-type}))
            (['@app 'isValidSignature address-variable digest-variable signature-variable]
              (let
                ((address {reduce-expression self address-variable})
                 (digest {reduce-expression self digest-variable})
                 (signature {reduce-expression self signature-variable}))
                (isValidSignature address digest signature)))
            (['sign digest-variable]
              (let
                ((this-participant {get-active-participant self})
                 (digest {reduce-expression self digest-variable}))
                (make-message-signature (secret-key<-address this-participant) digest)))))
          {add-to-environment self variable-name variable-value}))

      (['require! variable-name]
        (match {reduce-expression self variable-name}
          (#t (void))
          (#f
            (error "Assertion failed"))
          (else
            (error "Assertion failed, variable is not a Boolean" variable-name))))

      (['return ['@tuple]]
        (void))

      (['@label name]
        (set! (@ self current-label) name)))))

(define-type ContractHandshake
  (Record
   initial-block: [Block]
   contract-config: [ContractConfig]))

;; MESSAGE
(defclass Message (inbox outbox asset-transfers)
  constructor: :init!
  transparent: #t)

(defmethod {:init! Message}
  (λ (self (i #f) (o (make-hash-table)) (at []))
    (set! (@ self inbox) i)
    (set! (@ self outbox) o)
    (set! (@ self asset-transfers) at)))

(defmethod {reset Message}
  (lambda (self)
    (set! (@ self inbox) #f)
    (hash-clear! (@ self outbox))))

(defmethod {add-to-published Message}
  (λ (self name type value)
    (hash-put! (@ self outbox) name [type . value])))

;; expect-published : Sym TypeMethods -> Any
(defmethod {expect-published Message}
  (λ (self name type)
    ;; ignore name, by order not by name
    (unmarshal type (@ self inbox))))

;; add-to-withdraw : Address Nat -> Void
(defmethod {add-to-withdraw Message}
  (λ (self address amount)
    (def mat (@ self asset-transfers))
    (def mat2 (assq-update mat address (cut + <> amount) 0))
    (def mat3 (assq-update mat2 #f (cut - <> amount) 0))
    (set! (@ self asset-transfers) mat3)))

;; add-to-deposit : Nat -> Void
(defmethod {add-to-deposit Message}
  (λ (self address amount)
    (def mat (@ self asset-transfers))
    (def mat2 (assq-update mat address (cut - <> amount) 0))
    (def mat3 (assq-update mat2 #f (cut + <> amount) 0))
    (set! (@ self asset-transfers) mat3)))

;; expect-deposited : Nat -> Void
(defmethod {expect-deposited Message}
  (λ (self address amount)
    (def mat (@ self asset-transfers))
    (def mat2 (assq-update mat address (cut + <> amount) 0))
    (def mat3 (assq-update mat2 #f (cut - <> amount) 0))
    (set! (@ self asset-transfers) mat3)))

;; expect-withdrawn : Address Nat -> Void
(defmethod {expect-withdrawn Message}
  (λ (self address amount)
    (def mat (@ self asset-transfers))
    (def mat2 (assq-update mat address (cut - <> amount) 0))
    (def mat3 (assq-update mat2 #f (cut + <> amount) 0))
    (set! (@ self asset-transfers) mat3)))

(defmethod {compute-participant-dues Message}
  (λ (self participant)
    (def asset-transfers (@ self asset-transfers))
    (def active-balance
      (if (assq-has-key? asset-transfers participant)
        (assq-ref asset-transfers participant)
        0))
    (if (negative? active-balance)
      (abs active-balance)
      0)))

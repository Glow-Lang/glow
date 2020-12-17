(export #t)

(import
  :gerbil/gambit/bytes
  :std/iter :std/sugar
  :clan/exception :clan/json :clan/poo/io :clan/poo/mop :clan/pure/dict/assq :clan/persist/content-addressing
  :mukn/ethereum/ethereum :mukn/ethereum/network-config  :mukn/ethereum/json-rpc
  :mukn/ethereum/transaction :mukn/ethereum/tx-tracker :mukn/ethereum/watch
  :mukn/ethereum/contract-runtime :mukn/ethereum/contract-config
  ./program
  ./ethereum-contract
  ../compiler/method-resolve/method-resolve
  ../compiler/project/runtime-2)

;; PARTICIPANT RUNTIME
(defclass Runtime (role contract contract-status current-code-block-label current-label environment message)
  constructor: :init!
  transparent: #t)

(defmethod {:init! Runtime}
  (λ (self role contract
        (ccbl 'begin0)
        (cs (ContractStatus-NotYetDeployed (void)))
        (cl 'begin)
        (e (make-hash-table))
        (m (make-Message))
        (ib #f))
    (set! (@ self role) role)
    (set! (@ self contract) contract)
    (set! (@ self contract-status) cs)
    ;; TODO: extract initial code block label from contract compiler output
    (set! (@ self current-code-block-label) ccbl)
    (set! (@ self current-label) cl)
    (set! (@ self environment) e)
    (set! (@ self message) m)
    {initialize-environment self}))

(defmethod {execute Runtime}
  (λ (self)
    (with-logged-exceptions ()
      (def ccbl (@ self current-code-block-label))
      (displayln "executing code block: " ccbl)

      ;; non-active participants verify previous move
      (unless {is-active-participant? self}
        {receive self})

      ;; interpreter
      (def code-block {get-current-code-block self})
      (for ((statement (code-block-statements code-block)))
        {interpret-participant-statement self statement})

      ;; active participants make next move
      (when {is-active-participant? self}
        {publish self})

      (match (code-block-exit code-block)
        (#f
          (void)) ; contract finished
        (exit
          (set! (@ self current-code-block-label) exit)
          {execute self})))))

(defmethod {is-active-participant? Runtime}
  (λ (self)
    (def current-code-block {get-current-code-block self})
    (equal? (@ self role) (code-block-participant current-code-block))))

(defmethod {watch Runtime}
  (λ (self contract-address from-block)
    (let/cc return
      (def callback (λ (log) (return log)))
      (def to-block (+ from-block (@ (@ self contract) timeout)))
      (watch-contract callback contract-address from-block to-block))))

(define-type ContractStatus
  (Sum
    NotYetDeployed: Unit
    Active: (Tuple Quantity TransactionReceipt)
    Complete: Unit))
(define-sum-constructors ContractStatus NotYetDeployed Active Complete)

(defmethod {receive Runtime}
  (λ (self)
    (match (@ self contract-status)
      ((ContractStatus-NotYetDeployed _)
        (displayln "verifying contract config ...")
        (let*
          ((contract-handshake {read-handshake self})
           (initial-block (.@ contract-handshake initial-block))
           (contract-config (.@ contract-handshake contract-config))
           (create-pretx {prepare-create-contract-transaction self initial-block}))
            (verify-contract-config contract-config create-pretx)
            (set! (@ self contract-status)
              (ContractStatus-Active
                (vector initial-block (eth_getTransactionReceipt (.@ contract-config creation-hash)))))))

      ((ContractStatus-Active (vector initial-block tx-receipt))
        ;; TODO: handle multiple logs and also handle multiple events within the same block
        (displayln "watching for new transaction ...")
        (let*
          ;; TODO: `from` should be calculated using the deadline and not necessarily the previous tx,
          ;; since it may or not be setting the deadline
          ((from (.@ tx-receipt blockNumber))
            (new-tx-receipt {watch self (.@ tx-receipt contractAddress) from})
            (log-data (.@ new-tx-receipt data)))
          (set! (@ self contract-status) (ContractStatus-Active (vector initial-block new-tx-receipt)))
          (set! (@ (@ self message) inbox) (open-input-u8vector log-data))))

      ((ContractStatus-Complete _)
        (void)))))

(defmethod {read-handshake Runtime}
  (λ (self)
    (def handshake-json (json<-string (string<-json (read-file-json "run/contract-handshake.json"))))
    (<-json ContractHandshake handshake-json)))

(defmethod {publish Runtime}
  (λ (self)
    (match (@ self contract-status)

      ((ContractStatus-NotYetDeployed _)
        (displayln "deploying contract ...")
        {deploy-contract self})

      ;; TODO: Verify asset transfers using previous transaction and balances
      ;; recorded in Message's asset-transfer table during interpretation. Probably
      ;; requires getting TransactionInfo using the TransactionReceipt.
      ((ContractStatus-Active (vector initial-block tx-receipt))
        (displayln "publishing message ...")
        (let*
          ((contract-address (.@ tx-receipt contractAddress))
            (message (@ self message))
            (outbox (@ message outbox))
            (message-pretx {prepare-call-function-transaction self outbox initial-block contract-address}))
          (begin
            (def new-tx-receipt (post-transaction message-pretx))
            {reset message}
            (set! (@ self contract-status) (ContractStatus-Active (vector initial-block new-tx-receipt))))))

      ((ContractStatus-Complete _)
        (void)))))

(defmethod {add-to-environment Runtime}
  (λ (self name value)
    (hash-put! (@ self environment) name value)))

(defmethod {prepare-create-contract-transaction Runtime}
  (λ (self initial-block)
    (def sender-address {get-active-participant self})
    (def next (code-block-exit {get-current-code-block self}))
    (defvalues (contract-runtime-bytes contract-runtime-labels)
      {generate-consensus-runtime (@ self contract)})
    (def initial-state
      {create-frame-variables (@ self contract) initial-block contract-runtime-labels next})
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
    (write-file-json "run/contract-handshake.json" (json<- ContractHandshake handshake))
    (set! (@ self contract-status) (ContractStatus-Active (vector initial-block receipt)))))

;; See gerbil-ethereum/contract-runtime.ss for spec.
(defmethod {prepare-call-function-transaction Runtime}
  (λ (self outbox initial-block contract-address)
    (def sender-address {get-active-participant self})
    (defvalues (_ contract-runtime-labels)
      {generate-consensus-runtime (@ self contract)})
    (def frame-variables
      {create-frame-variables (@ self contract) initial-block contract-runtime-labels (@ self current-code-block-label)})
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
      value: {compute-participant-dues (@ self message) sender-address}
      gas: 800000)))

(defmethod {get-current-code-block Runtime}
  (λ (self)
    (def contract (@ self contract))
    (def participant-interaction
      {get-interaction (@ contract program) (@ self role)})
    (hash-get participant-interaction (@ self current-code-block-label))))

;; TODO: map alpha-converted names to names in original source when displaying to user
(defmethod {initialize-environment Runtime}
  (λ (self)
    (def contract (@ self contract))
    (for ((values key value) (in-hash (@ contract participants)))
      {add-to-environment self key value})
    (for ((values key [_ . value]) (in-hash (@ contract arguments)))
      {add-to-environment self key value})))

(defmethod {reduce-expression Runtime}
  (λ (self expression)
    (if (symbol? expression)
      (match (hash-get (@ self environment) expression)
        ([type . value]
          value)
        (#f
          (error (string-append expression " is missing from execution environment")))
        (value
          value))
      expression)))

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
    (match statement

      (['set-participant new-participant]
        ;; Since the contract has already been separated into transaction boundaries,
        ;; the participant doesn't need to do anything here, since the active participant
        ;; is already known.
        (void))

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
  (λ (self)
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

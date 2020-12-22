(export #t)

(import
  :gerbil/gambit/bytes :gerbil/gambit/threads
  :std/iter :std/sugar
  :clan/exception :clan/json :clan/path-config :clan/pure/dict/assq
  :clan/poo/poo :clan/poo/io :clan/poo/mop
  :clan/persist/content-addressing
  :mukn/ethereum/hex :mukn/ethereum/ethereum :mukn/ethereum/network-config :mukn/ethereum/json-rpc
  :mukn/ethereum/transaction :mukn/ethereum/tx-tracker :mukn/ethereum/watch
  :mukn/ethereum/contract-runtime :mukn/ethereum/contract-config
  ./program
  ./ethereum-contract
  ../compiler/method-resolve/method-resolve
  ../compiler/project/runtime-2)

;; PARTICIPANT RUNTIME

;; WAS: (role contract contract-status current-code-block-label current-label environment message)
(defclass Runtime
  (role contract contract-config status processed-events unprocessed-events
        current-code-block-label current-label environment message)
  constructor: :init!
  transparent: #t)

(defmethod {:init! Runtime}
  (λ (self
      role: role contract: contract
      current-code-block-label: current-code-block-label ;; TODO: grab the start label from the compilation output, instead of 'begin0
      current-label: current-label) ;; TODO: grab the start label from the compilation output, instead of 'begin
    (set! (@ self role) role)
    (set! (@ self contract) contract)
    ;; TODO: extract initial code block label from contract compiler output
    (set! (@ self current-code-block-label) current-code-block-label)
    (set! (@ self current-label) current-label)

    (set! (@ self contract-config) #f)
    (set! (@ self status) 'running) ;; (Enum running stopped completed aborted)
    (set! (@ self processed-events) '())
    (set! (@ self unprocessed-events) '())
    (set! (@ self environment) (make-hash-table))
    (set! (@ self message) (make-Message))
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
  ;; TODO: consult unprocessed log objects first, if none is available, then use getLogs
  ;; TODO: be able to split getLogs into smaller requests if it a bigger request times out.
  ;; TODO: (optional) push all the previously processed log objects to the processed list after processing
  (λ (self contract-address from-block)
    (let/cc return
      (def callback (λ (log) (return log))) ;; TODO: handle multiple log entries!!!
      (def to-block (+ from-block (ethereum-timeout-in-blocks))) ;; TODO: get the timeout that from the agreement
      (watch-contract callback contract-address from-block to-block))))

(define-type ContractStatus
  (Sum
    NotYetDeployed: Unit
    Active: (Tuple Quantity TransactionReceipt)
    Complete: Unit))
(define-sum-constructors ContractStatus NotYetDeployed Active Complete)

(defmethod {receive Runtime}
  (λ (self)
    (def role (@ self role))
    (def contract-config (@ self contract-config))
    (when (eq? (@ self status) 'running)
      (if contract-config
        (let ()
          (displayln role ": Watching for new transaction ...")
          ;; TODO: `from` should be calculated using the deadline and not necessarily the previous tx,
          ;; since it may or not be setting the deadline
          (display-poo-ln role ": contract-config=" ContractConfig contract-config)
          (def from (.@ contract-config creation-block))
          (def new-log-object {watch self (.@ contract-config contract-address) from})
          ;; TODO: handle the case when there is no log objects
          (display-poo-ln role ": New TX: " (Maybe LogObject) new-log-object)
          (def log-data (.@ new-log-object data))
          ;; TODO: process the data in the same method?
          (set! (@ (@ self message) inbox) (open-input-u8vector log-data)))
        (let ()
          (displayln role ": Reading contract handshake ...")
          (def contract-handshake {read-handshake self})
          (display-poo-ln role ": contract-handshake: " ContractHandshake contract-handshake)
          (displayln role ": Verifying contract config ...")
          (def-slots (contract-config) contract-handshake)
          (def contract (@ self contract))
          (def create-pretx {prepare-create-contract-transaction self (@ contract initial-timer-start)})
          (verify-contract-config contract-config create-pretx)
          (set! (@ self contract-config) contract-config))))))

(defmethod {read-handshake Runtime}
  (λ (self)
    (def handshake-json (read-file-json (run-path "contract-handshake.json")))
    (<-json ContractHandshake handshake-json)))

(defmethod {publish Runtime}
  (λ (self)
    (def role (@ self role))
    (def contract-config (@ self contract-config))
    (when (eq? (@ self status) 'running)
      (if (not contract-config)
        (let ()
          (displayln role ": deploying contract ...")
          {deploy-contract self})
        (let ()
          ;; TODO: Verify asset transfers using previous transaction and balances
          ;; recorded in Message's asset-transfer table during interpretation. Probably
          ;; requires getting TransactionInfo using the TransactionReceipt.
          (displayln "publishing message ...")
          (def contract-address (.@ contract-config contract-address))
          (def contract (@ self contract))
          (def timer-start (@ contract initial-timer-start))
          (def message (@ self message))
          (def outbox (@ message outbox))
          (def message-pretx {prepare-call-function-transaction self outbox timer-start contract-address})
          (def new-tx-receipt (post-transaction message-pretx))
          {reset message})))))

(def (sexp<-state state) (map (match <> ([t . v] (sexp<- t v))) state))

(defmethod {add-to-environment Runtime}
  (λ (self name value)
    (hash-put! (@ self environment) name value)))

(defmethod {prepare-create-contract-transaction Runtime}
  (λ (self timer-start)
    (def sender-address {get-active-participant self})
    (def next (code-block-exit {get-current-code-block self}))
    (defvalues (contract-runtime-bytes contract-runtime-labels)
      {generate-consensus-runtime (@ self contract)})
    (def contract (@ self contract))
    (def initial-state
      {create-frame-variables contract (@ contract initial-timer-start) contract-runtime-labels next})
    (def initial-state-digest
      (digest-product-f initial-state))
    (def contract-bytes
      (stateful-contract-init initial-state-digest contract-runtime-bytes))
    (create-contract sender-address contract-bytes
      value: {compute-participant-dues (@ self message) sender-address})))

(defmethod {deploy-contract Runtime}
  (λ (self)
    (def role (@ self role))
    (def contract (@ self contract))
    (def timer-start (@ contract initial-timer-start))
    (def pretx {prepare-create-contract-transaction self timer-start})
    (display-poo-ln role ": Deploying contract... "
                    "timer-start: " timer-start)
    (def receipt (post-transaction pretx))
    (def contract-config (contract-config<-creation-receipt receipt))
    (display-poo-ln role ": Contract config: " ContractConfig contract-config)
    (verify-contract-config contract-config pretx)
    (def handshake (new ContractHandshake timer-start contract-config))
    (display-poo-ln role ": Handshake: " ContractHandshake handshake)
    ;; TODO: display at terminal for user to copy paste and send to
    ;; other participants through outside channel
    (write-file-json (run-path "contract-handshake.json") (json<- ContractHandshake handshake))
    (set! (@ self contract-config) contract-config)))

;; See gerbil-ethereum/contract-runtime.ss for spec.
(defmethod {prepare-call-function-transaction Runtime}
  (λ (self outbox timer-start contract-address)
    (def sender-address {get-active-participant self})
    (defvalues (_ contract-runtime-labels)
      {generate-consensus-runtime (@ self contract)})
    (def frame-variables
      {create-frame-variables (@ self contract) timer-start contract-runtime-labels (@ self current-code-block-label)})
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
;;       using the alpha-back-table
(defmethod {initialize-environment Runtime}
  (λ (self)
    (def contract (@ self contract))
    (for ((values key value) (in-hash (@ contract participants)))
      {add-to-environment self key value})
    (for ((values key [_ . value]) (in-hash (@ contract arguments)))
      {add-to-environment self key value})))

(defmethod {reduce-expression Runtime}
  (λ (self expression)
    (cond
     ((symbol? expression)
      (match (hash-get (@ self environment) expression)
        ([type . value]
          value)
        (#f
          (error (string-append expression " is missing from execution environment")))
        (value
          value)))
     ((boolean? expression) expression)
     ((string? expression) (string->bytes expression))
     ((bytes? expression) expression)
     ((integer? expression) expression)
     ;; TODO: reduce other trivial expressions
     (else
      expression))))

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
        ;; the participant doesn't need to do anything here since the active participant
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
            (['@app '< a b]
              (let
                ((av {reduce-expression self a})
                 (bv {reduce-expression self b}))
                (< av bv)))
            (['@app 'randomUInt256]
             (randomUInt256))
            (['@tuple . es]
             (list->vector
              (for/collect ((e es))
                {reduce-expression self e})))
            (['digest . es]
             (digest
              (for/collect ((e es))
                (cons {lookup-type (@ (@ self contract) program) e}
                      {reduce-expression self e}))))
            (['sign digest-variable]
              (let
                ((this-participant {get-active-participant self})
                 (digest {reduce-expression self digest-variable}))
                (make-message-signature (secret-key<-address this-participant) digest)))
            (['input 'Nat tag]
             (let ((tagv {reduce-expression self tag}))
               (input UInt256 tagv)))))
          {add-to-environment self variable-name variable-value}))

      (['require! variable-name]
        (match {reduce-expression self variable-name}
          (#t
            (void))
          (#f
            (error "Assertion failed"))
          (else
            (error "Assertion failed, " variable-name " is not a Boolean"))))

      (['return ['@tuple]]
        (void))

      (['@label name]
        (set! (@ self current-label) name)))))

(define-type ContractHandshake
  (Record
   ;;agreement: [Contract]
   timer-start: [Block]
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

(export #t)

(import
  :gerbil/gambit/bytes :gerbil/gambit/threads
  :std/iter :std/sugar
  :clan/exception :clan/json :clan/path-config :clan/pure/dict/assq
  :clan/poo/poo :clan/poo/io :clan/poo/mop :clan/poo/type
  :clan/persist/content-addressing
  :mukn/ethereum/hex :mukn/ethereum/ethereum :mukn/ethereum/network-config :mukn/ethereum/json-rpc
  :mukn/ethereum/transaction :mukn/ethereum/tx-tracker :mukn/ethereum/watch
  :mukn/ethereum/contract-runtime :mukn/ethereum/contract-config
  ./program
  ./ethereum-contract
  ../compiler/method-resolve/method-resolve
  ../compiler/project/runtime-2)

;; NB: Whichever function exports data end-users / imports from them should make sure to put in a Json array (Scheme list) prepend by the name of the type. And/or we may have a {"": "InteractionAgreement" ...} field with this asciibetically always-first name. Maybe such function belongs to gerbil-poo, too.

(define-type Tokens (MonomorphicPoo Nat))

(define-type AgreementOptions
  (Record
   blockchain: [String] ;; e.g. "Cardano KEVM Testnet", as per ethereum_networks.json
   escrowAmount: [(Maybe Tokens) default: (void)] ;; not meaningful for all contracts
   timeoutInBlocks: [Nat]
   maxInitialBlock: [Nat]))

(define-type InteractionAgreement
  (.+
   (Record
    glow-version: [String] ;; e.g. "Glow v0.0-560-gda782c9 on Gerbil-ethereum v0.0-83-g6568bc6" ;; TODO: have a function to compute that from versioning.ss
    interaction: [String] ;; e.g. "mukn/glow/examples/buy_sig#payForSignature", fully qualified Gerbil symbol
    participants: [(MonomorphicPoo Address)] ;; e.g. {Buyer: alice Seller: bob}
    parameters: [Json] ;; This Json object to be decoded according to a type descriptor from the interaction (dependent types yay!)
    reference: [(MonomorphicPoo Json)] ;; Arbitrary reference objects from each participant, with some conventional size limits on the Json string.
    options: [AgreementOptions] ;; See above
    code-digest: [Digest]))) ;; Make it the digest of Glow source code (in the future, including all Glow libraries transitively used)

(define-type AgreementHandshake
  (Record
   agreement: [InteractionAgreement]
   contract-config: [ContractConfig]))
;; timer-start = (.@ agreement-handshake agreement options maxInitialBlock)

(define-type IOContext
  (instance Class
    slots: (.o send-handshake: (.o type: (Fun Unit <- AgreementHandshake))
               receive-handshake: (.o type: (Fun AgreementHandshake <-)))))

;; TODO: make an alternate version of io-context that
;;       displays at the terminal for the user to copy/paste and send to
;;       other participants through an outside channel
(.def io-context:special-file
  send-handshake:
  (lambda (handshake)
    (write-file-json (run-path "agreement-handshake.json") (json<- AgreementHandshake handshake)))
  receive-handshake:
  (lambda ()
    (def agreement-handshake.json (run-path "agreement-handshake.json"))
    (while (not (file-exists? agreement-handshake.json))
      (displayln "waiting for agreement handshake ...")
      (thread-sleep! 1))
    (def handshake-json (read-file-json agreement-handshake.json))
    (<-json AgreementHandshake handshake-json)))

;; PARTICIPANT RUNTIME

;; TODO: derive the contract from the agreement,
;;       check that the code-digest in the agreement matches
(defclass Runtime
  (role ;; : Symbol
   agreement ;; : InteractionAgreement
   contract ;; : Contract
   contract-config ;; : ContractConfig
   status ;; (Enum running completed aborted stopped)
   processed-events ;; : (List LogObjects) ;; ???
   unprocessed-events ;; : (List LogObjects) ;; ???
   current-code-block-label ;; : Symbol
   current-label ;; : Symbol
   environment ;; : (Table (Or DependentPair Any) <- Symbol) ;; TODO: have it always typed???
   message ;; : Message ;; byte buffer?
   timer-start ;; Block ;;
   io-context) ; : IOContext
  constructor: :init!
  transparent: #t)

(defmethod {:init! Runtime}
  (λ (self
      role: role
      agreement: agreement
      contract: contract
      current-code-block-label: current-code-block-label ;; TODO: grab the start label from the compilation output, instead of 'begin0
      current-label: current-label ;; TODO: grab the start label from the compilation output, instead of 'begin
      io-context: (io-context io-context:special-file))
    (set! (@ self role) role)
    (set! (@ self agreement) agreement)
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
    (set! (@ self io-context) io-context)
    {initialize-environment self}))

;; <- Runtime
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

;; Bool <- Runtime
(defmethod {is-active-participant? Runtime}
  (λ (self)
    (def current-code-block {get-current-code-block self})
    (equal? (@ self role) (code-block-participant current-code-block))))

;; TODO: everything about this function, from the timer-start and/or wherever we left off
;; to timeout or (indefinite future if no timeout???)
;; : LogObject <- Runtime Address Block
(defmethod {watch Runtime}
  ;; TODO: consult unprocessed log objects first, if none is available, then use getLogs
  ;; TODO: be able to split getLogs into smaller requests if it a bigger request times out.
  ;; TODO: (optional) push all the previously processed log objects to the processed list after processing
  (λ (self contract-address from-block)
    (let/cc return
      (def callback (λ (log) (return log))) ;; TODO: handle multiple log entries!!!
      (def to-block (+ from-block (@ (@ self contract) timeout)))
      (watch-contract callback contract-address from-block to-block))))

;; <- Runtime
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
          (set! (@ self timer-start) (.@ new-log-object blockNumber))
          ;; TODO: process the data in the same method?
          (set! (@ (@ self message) inbox) (open-input-u8vector log-data)))
        (let ()
          (displayln role ": Reading contract handshake ...")
          (def agreement-handshake {read-handshake self})
          (display-poo-ln role ": agreement-handshake: " AgreementHandshake agreement-handshake)
          (displayln role ": Verifying contract config ...")
          (def-slots (agreement contract-config) agreement-handshake)
          ;; check that the agreement part matches
          (assert! (equal? (json<- InteractionAgreement (@ self agreement))
                           (json<- InteractionAgreement agreement)))
          (def timer-start (.@ agreement options maxInitialBlock))
          (def contract (@ self contract))
          (set! (@ self timer-start) timer-start)
          (def create-pretx {prepare-create-contract-transaction self})
          (verify-contract-config contract-config create-pretx)
          (set! (@ self contract-config) contract-config))))))

;; : AgreementHandshake <- Runtime
(defmethod {read-handshake Runtime}
  (λ (self)
    (def io-context (@ self io-context))
    (.call io-context receive-handshake)))

;; <- Runtime
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
          (def message (@ self message))
          (def outbox (@ message outbox))
          (def message-pretx {prepare-call-function-transaction self outbox contract-address})
          (def new-tx-receipt (post-transaction message-pretx))
          {reset message})))))

;; Sexp <- State
(def (sexp<-state state) (map (match <> ([t . v] (sexp<- t v))) state))

;; TODO: include type output, too, looked up in type table.
;; <- Runtime Symbol Value
(defmethod {add-to-environment Runtime}
  (λ (self name value)
    (hash-put! (@ self environment) name value)))

;; PreTransaction <- Runtime Block
(defmethod {prepare-create-contract-transaction Runtime}
  (λ (self)
    (def sender-address {get-active-participant self})
    (def code-block {get-current-code-block self})
    (def next (code-block-exit code-block))
    (def participant (code-block-participant code-block))
    (defvalues (contract-runtime-bytes contract-runtime-labels)
      {generate-consensus-runtime (@ self contract)})
    (def contract (@ self contract))
    (def initial-state
      {create-frame-variables contract (@ contract initial-timer-start) contract-runtime-labels next participant})
    (def initial-state-digest
      (digest-product-f initial-state))
    (def contract-bytes
      (stateful-contract-init initial-state-digest contract-runtime-bytes))
    (create-contract sender-address contract-bytes
      value: {compute-participant-dues (@ self message) sender-address})))

;; PreTransaction <- Runtime Block
(defmethod {deploy-contract Runtime}
  (λ (self)
    (def role (@ self role))
    (def agreement (@ self agreement))
    (def contract (@ self contract))
    (def timer-start (@ contract initial-timer-start))
    (def pretx {prepare-create-contract-transaction self})
    (display-poo-ln role ": Deploying contract... "
                    "timer-start: " timer-start)
    (def receipt (post-transaction pretx))
    (def contract-config (contract-config<-creation-receipt receipt))
    (display-poo-ln role ": Contract config: " ContractConfig contract-config)
    (verify-contract-config contract-config pretx)
    (def handshake (.new AgreementHandshake agreement contract-config))
    (display-poo-ln role ": Handshake: " AgreementHandshake handshake)
    {send-contract-handshake self handshake}
    (set! (@ self contract-config) contract-config)))

(defmethod {send-contract-handshake Runtime}
  (lambda (self handshake)
    (def io-context (@ self io-context))
    (.call io-context send-handshake handshake)))

;; See gerbil-ethereum/contract-runtime.ss for spec.
;; PreTransaction <- Runtime Message.Outbox Block Address
(defmethod {prepare-call-function-transaction Runtime}
  (λ (self outbox contract-address)
    (def sender-address {get-active-participant self})
    (defvalues (_ contract-runtime-labels)
      {generate-consensus-runtime (@ self contract)})
    (def frame-variables
      {create-frame-variables
        (@ self contract)
        (@ self timer-start)
        contract-runtime-labels
        (@ self current-code-block-label)
        (@ self role)})
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
      ;; default should be (void), i.e. ask for an automatic estimate,
      ;; unless we want to force the TX to happen
      gas: 800000)))

;; CodeBlock <- Runtime
(defmethod {get-current-code-block Runtime}
  (λ (self)
    (def contract (@ self contract))
    (def participant-interaction
      {get-interaction (@ contract program) (@ self role)})
    (hash-get participant-interaction (@ self current-code-block-label))))

;; TODO: map alpha-converted names to names in original source when displaying to user
;;       using the alpha-back-table
;; <- Runtime
(defmethod {initialize-environment Runtime}
  (λ (self)
    (def contract (@ self contract))
    (for ((values key value) (in-hash (@ contract participants)))
      {add-to-environment self key value})
    (for ((values key [_ . value]) (in-hash (@ contract arguments)))
      {add-to-environment self key value})))

;; TODO: use the one in std/misc/hash after this PR gets merged: https://github.com/vyzo/gerbil/pull/588
(def %none '(none))
(def (hash-ref/default hash key default)
  (def v (hash-ref hash key %none))
  (if (eq? v %none)
    (default)
    v))

;; TODO: make sure everything always has a type ???
;; Any <- Runtime
(defmethod {reduce-expression Runtime}
  (λ (self expression)
    (cond
     ((symbol? expression)
      (match (hash-ref/default
              (@ self environment) expression
              (cut error "variable missing from execution environment" expression))
        ([type . value]
          value)
        (value
          value)))
     ((boolean? expression) expression)
     ((string? expression) (string->bytes expression))
     ((bytes? expression) expression)
     ((integer? expression) expression)
     ;; TODO: reduce other trivial expressions
     (else
      expression))))

;; Symbol <- Runtime
(defmethod {get-active-participant Runtime}
  (λ (self)
    (def contract (@ self contract))
    (hash-get (@ contract participants) (@ self role))))

;; Bytes <- (List DependentPair)
(def (marshal-product-f fields)
  (def out (open-output-u8vector))
  (marshal-product-to fields out)
  (get-output-u8vector out))

;; <- (List DependentPair) BytesOutputPort
(def (marshal-product-to fields port)
  (for ((p fields))
    (with (([t . v] p)) (marshal t v port))))

;; : Digest <- (List DependentPair)
(def (digest-product-f fields)
  (digest<-bytes (marshal-product-f fields)))

;; : <- Runtime ProjectStatement
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
              (let ((this-participant {get-active-participant self})
                    (digest {reduce-expression self digest-variable}))
                (make-message-signature (secret-key<-address this-participant) digest)))
            (['input 'Nat tag]
             (let ((tagv {reduce-expression self tag}))
               (input UInt256 tagv)))
            (else
              {reduce-expression self expression})))
          {add-to-environment self variable-name variable-value}))

      (['require! variable-name]
        (match {reduce-expression self variable-name}
          (#t (void))
          ;; TODO: include debugging information when something fails!
          (#f
           (error "Assertion failed"))
          (else
           (error "Assertion failed, variable is not a Boolean" variable-name))))

      (['return ['@tuple]]
        (void))

      (['@label name]
        (set! (@ self current-label) name))

      (['switch variable-name cases ...]
        (let*
          ((variable-value {reduce-expression self variable-name})
           (matching-case (find (λ (case) (equal? {reduce-expression self (car case)} variable-value)) cases)))
        (for (case-statement (cdr matching-case))
          {interpret-participant-statement self case-statement}))))))

;; MESSAGE ;; TODO: more like CommunicationState
(defclass Message
  (inbox ;; : BytesInputPort
   outbox ;; : (Table DependentPair <- Symbol) ;; TODO: just have a BytesOutputPort ?
   asset-transfers) ;; : (Alist Z <- Address)
  constructor: :init!
  transparent: #t)

(defmethod {:init! Message}
  (λ (self (i #f) (o (make-hash-table)) (at []))
    (set! (@ self inbox) i)
    (set! (@ self outbox) o)
    (set! (@ self asset-transfers) at)))

;; <- Message
(defmethod {reset Message}
  (λ (self)
    (set! (@ self inbox) #f)
    (hash-clear! (@ self outbox))))

;; <- Message Symbol t:Type t
(defmethod {add-to-published Message}
  (λ (self name type value)
    (hash-put! (@ self outbox) name [type . value])))

;; expect-published : t <- Symbol t:Type
(defmethod {expect-published Message}
  (λ (self name type)
    ;; ignore name, by order not by name
    (unmarshal type (@ self inbox))))

;; add-to-withdraw : <- Address Nat
(defmethod {add-to-withdraw Message}
  (λ (self address amount)
    (def mat (@ self asset-transfers))
    (def mat2 (assq-update mat address (cut + <> amount) 0))
    (def mat3 (assq-update mat2 #f (cut - <> amount) 0))
    (set! (@ self asset-transfers) mat3)))

;; add-to-deposit : <- Nat
(defmethod {add-to-deposit Message}
  (λ (self address amount)
    (def mat (@ self asset-transfers))
    (def mat2 (assq-update mat address (cut - <> amount) 0))
    (def mat3 (assq-update mat2 #f (cut + <> amount) 0))
    (set! (@ self asset-transfers) mat3)))

;; expect-deposited : <- Nat
(defmethod {expect-deposited Message}
  (λ (self address amount)
    (def mat (@ self asset-transfers))
    (def mat2 (assq-update mat address (cut + <> amount) 0))
    (def mat3 (assq-update mat2 #f (cut - <> amount) 0))
    (set! (@ self asset-transfers) mat3)))

;; expect-withdrawn : <- Address Nat
(defmethod {expect-withdrawn Message}
  (λ (self address amount)
    (def mat (@ self asset-transfers))
    (def mat2 (assq-update mat address (cut - <> amount) 0))
    (def mat3 (assq-update mat2 #f (cut + <> amount) 0))
    (set! (@ self asset-transfers) mat3)))

;; : Nat <- Message Symbol
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

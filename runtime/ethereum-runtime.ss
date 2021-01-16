(export #t)

(import
  :gerbil/gambit/bytes :gerbil/gambit/threads
  :std/iter :std/misc/hash :std/sugar :std/misc/number :std/misc/list :std/sort :std/srfi/1
  :clan/base :clan/exception :clan/io :clan/json :clan/number :clan/path-config :clan/ports :clan/syntax
  :clan/poo/poo :clan/poo/io :clan/poo/debug :clan/debug
  :clan/persist/content-addressing
  :mukn/ethereum/hex :mukn/ethereum/ethereum :mukn/ethereum/network-config :mukn/ethereum/json-rpc
  :mukn/ethereum/transaction :mukn/ethereum/tx-tracker :mukn/ethereum/watch :mukn/ethereum/assets
  :mukn/ethereum/contract-runtime :mukn/ethereum/contract-config :mukn/ethereum/assembly :mukn/ethereum/types
  ./program ./message
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
    code-digest: [Bytes]))) ;; Make it the digest of Glow source code (in the future, including all Glow libraries transitively used)

(define-type AgreementHandshake
  (Record
   agreement: [InteractionAgreement]
   contract-config: [ContractConfig]
   published-data: [Bytes])) ;; Variables published by the first active participant inside the first code block.
;; timer-start = (.@ agreement-handshake agreement options maxInitialBlock)

(define-type IOContext
  (instance Class
    slots: (.o send-handshake: (.o type: (Fun Unit <- AgreementHandshake))
               receive-handshake: (.o type: (Fun AgreementHandshake <-)))))

;; TODO: make an alternate version of io-context that
;;       displays at the terminal for the user to copy/paste and send to
;;       other participants through an outside channel
(.def io-context:special-file
  setup:
  (λ () (ignore-errors (delete-file (run-path "agreement-handshake.json"))))
  teardown:
  (λ () (ignore-errors (delete-file (run-path "agreement-handshake.json"))))
  send-handshake:
  (λ (handshake)
    (write-file-json (run-path "agreement-handshake.json") (json<- AgreementHandshake handshake)))
  receive-handshake:
  (λ ()
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
   contract-config ;; : ContractConfig
   status ;; (Enum running completed aborted stopped)
   processed-events ;; : (List LogObjects) ;; ???
   unprocessed-events ;; : (List LogObjects) ;; ???
   current-code-block-label ;; : Symbol
   current-label ;; : Symbol
   environment ;; : (Table (Or DependentPair Any) <- Symbol) ;; TODO: have it always typed???
   message ;; : Message ;; byte buffer?
   timer-start ;; Block
   io-context ; : IOContext
   program ;; : Program ;; from program.ss
   variable-offsets ;; : (Table (Table Offset <- Symbol) <- Symbol)
   params-end)
  constructor: :init!
  transparent: #t)

(defmethod {:init! Runtime}
  (λ (self
      role: role
      agreement: agreement
      current-code-block-label: current-code-block-label ;; TODO: grab the start label from the compilation output, instead of 'begin0
      current-label: current-label ;; TODO: grab the start label from the compilation output, instead of 'begin
      io-context: (io-context io-context:special-file)
      program: program)
    (set! (@ self role) role)
    (set! (@ self agreement) agreement)
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
    (set! (@ self program) program)
    {initialize-environment self}))

;; <- Runtime
(defmethod {execute Runtime}
  (λ (self)
    (with-logged-exceptions ()
      (def ccbl (@ self current-code-block-label))
      (displayln "executing code block: " ccbl)

      (if {is-active-participant? self}
        {publish self}
        {receive self})

      (match (code-block-exit {get-current-code-block self})
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
      (def to-block (+ from-block (.@ (@ self agreement) options timeoutInBlocks)))
      (watch-contract callback contract-address from-block to-block))))

(def (run-passive-code-block/contract self role contract-config)
  (displayln role ": Watching for new transaction ...")
  ;; TODO: `from` should be calculated using the deadline and not necessarily the previous tx,
  ;; since it may or not be setting the deadline
  (display-poo-ln role ": contract-config=" ContractConfig contract-config)
  (def from (if (@ self timer-start) (@ self timer-start) (.@ contract-config creation-block)))
  (displayln role ": watching from block " from)
  (def new-log-object {watch self (.@ contract-config contract-address) from})
  ;; TODO: handle the case when there is no log objects
  (display-poo-ln role ": New TX: " (Maybe LogObject) new-log-object)
  (def log-data (.@ new-log-object data))
  (set! (@ self timer-start) (.@ new-log-object blockNumber))
  ;; TODO: process the data in the same method?
  (set! (@ self message inbox) (open-input-u8vector log-data))
  (interpret-current-code-block self))

(def (interpret-current-code-block self)
  (let (code-block {get-current-code-block self})
    (for ((statement (code-block-statements code-block)))
      {interpret-participant-statement self statement})))


(def (run-passive-code-block/handshake self role)
  (nest
   (begin (displayln role ": Reading contract handshake ..."))
   (let (agreement-handshake {read-handshake self}))
   (begin
     (displayln role ": Verifying contract config ...")
     (force-current-outputs))
   (with-slots (agreement contract-config published-data) agreement-handshake)
   (let (message (@ self message)))
   (begin
     (set! (@ message inbox) (open-input-u8vector published-data))
     ;; TODO: Execute contract until first change participant.
     ;; Check that the agreement part matches
     (unless (equal? (json<- InteractionAgreement (@ self agreement))
                     (json<- InteractionAgreement agreement))
       (DDT agreements-mismatch:
            InteractionAgreement (@ self agreement)
            InteractionAgreement agreement)
       (error "agreements don't match" (@ self agreement) agreement))
     (set! (@ self timer-start) (.@ agreement options maxInitialBlock))
     (interpret-current-code-block self))
   (let (create-pretx {prepare-create-contract-transaction self})
     (verify-contract-config contract-config create-pretx)
     (set! (@ self contract-config) contract-config))))

;; TODO: rename to RunPassiveCodeBlock or something
;; <- Runtime
(defmethod {receive Runtime}
  (λ (self)
    (def role (@ self role))
    (def contract-config (@ self contract-config))
    (when (eq? (@ self status) 'running)
      (if contract-config
        (run-passive-code-block/contract self role contract-config)
        (run-passive-code-block/handshake self role)))))

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
    (interpret-current-code-block self)
    (when (eq? (@ self status) 'running)
      (if (not contract-config)
        (let ()
          (displayln role ": deploying contract ...")
          (def outbox (@ self message outbox))
          (def published-data
            (let (out (open-output-u8vector))
              (for ([type . value] outbox)
                (marshal type value out))
              (def out2 (open-output-u8vector))
              (marshal-sized16-bytes (get-output-u8vector out) out2)
              (get-output-u8vector out2)))
          {deploy-contract self}
          (def contract-config (@ self contract-config))
          (def agreement (@ self agreement))
          (def handshake (.new AgreementHandshake agreement contract-config published-data))
          (display-poo-ln role ": Handshake: " AgreementHandshake handshake)
          {send-contract-handshake self handshake})
        (let ()
          ;; TODO: Verify asset transfers using previous transaction and balances
          ;; recorded in Message's asset-transfer table during interpretation. Probably
          ;; requires getting TransactionInfo using the TransactionReceipt.
          (displayln role ": publishing message ...")
          (def contract-address (.@ contract-config contract-address))
          (def message (@ self message))
          (def outbox (@ message outbox))
          (def message-pretx {prepare-call-function-transaction self outbox contract-address})
          (def new-tx-receipt (post-transaction message-pretx))
          (display-poo-ln role ": Tx Receipt: " TransactionReceipt new-tx-receipt)
          (set! (@ self timer-start) (.@ new-tx-receipt blockNumber))
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
      {generate-consensus-runtime self})
    (def initial-state
      {create-frame-variables self (.@ (@ self agreement) options maxInitialBlock) contract-runtime-labels next participant})
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
    (def timer-start (.@ (@ self agreement) options maxInitialBlock))
    (def pretx {prepare-create-contract-transaction self})
    (display-poo-ln role ": Deploying contract... "
                    "timer-start: " timer-start)
    (def receipt (post-transaction pretx))
    (def contract-config (contract-config<-creation-receipt receipt))
    (display-poo-ln role ": Contract config: " ContractConfig contract-config)
    (verify-contract-config contract-config pretx)
    (set! (@ self contract-config) contract-config)))

(defmethod {send-contract-handshake Runtime}
  (λ (self handshake)
    (def io-context (@ self io-context))
    (.call io-context send-handshake handshake)))

;; See gerbil-ethereum/contract-runtime.ss for spec.
;; PreTransaction <- Runtime Message.Outbox Block Address
(defmethod {prepare-call-function-transaction Runtime}
  (λ (self outbox contract-address)
    (def sender-address {get-active-participant self})
    (defvalues (_ contract-runtime-labels)
      {generate-consensus-runtime self})
    (def frame-variables
      {create-frame-variables
        self
        (@ self timer-start)
        contract-runtime-labels
        (@ self current-code-block-label)
        (@ self role)})
    (def frame-variable-bytes (marshal-product-f frame-variables))
    (def frame-length (bytes-length frame-variable-bytes))
    (def message-bytes
      (let (out (open-output-u8vector))
        (marshal UInt16 frame-length out)
        (marshal-product-to frame-variables out)
        (for ([type . value] outbox)
          (marshal type value out))
        (marshal UInt8 1 out)
        (get-output-u8vector out)))
    (call-function sender-address contract-address message-bytes
      value: {compute-participant-dues (@ self message) sender-address})))
      ;; default gas value should be (void), i.e. ask for an automatic estimate,
      ;; unless we want to force the TX to happen
      ;; (gas: 800000))))

;; CodeBlock <- Runtime
(defmethod {get-current-code-block Runtime}
  (λ (self)
    (def participant-interaction
      {get-interaction (@ self program) (@ self role)})
    (hash-get participant-interaction (@ self current-code-block-label))))

;; TODO: map alpha-converted names to names in original source when displaying to user
;;       using the alpha-back-table
;; <- Runtime
(defmethod {initialize-environment Runtime}
  (λ (self)
    (def agreement (@ self agreement))
    (.call (@ self io-context) setup) ;; NB: putting this call before the def above crashes gxc(!)
    (def participants (.@ agreement participants))
    (for (participant-name (.all-slots-sorted participants))
      {add-to-environment self participant-name (.ref participants participant-name)})
    (def parameters (.@ agreement parameters))
    (for ((values parameter-name-key parameter-json-value) parameters)
      (def parameter-name (symbolify parameter-name-key))
      (def parameter-type {lookup-type (@ self program) parameter-name})
      (def parameter-value (<-json parameter-type parameter-json-value))
      {add-to-environment self parameter-name parameter-value})))

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
    (def environment (@ self environment))
    (hash-get environment (@ self role))))

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
    (displayln statement)
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
           (publish-type {lookup-type (@ self program) variable-name}))
          {add-to-published (@ self message) publish-name publish-type publish-value}))

      (['def variable-name expression]
        (let
          ((variable-value {interpret-participant-expression self expression}))
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

(defmethod {interpret-participant-expression Runtime}
  (λ (self expression)
    (match expression
      (['expect-published ['quote publish-name]]
        (let (publish-type {lookup-type (@ self program) publish-name})
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
      (['@app '+ a b]
        (let
          ((av {reduce-expression self a})
            (bv {reduce-expression self b}))
          (+ av bv)))
      (['@app bitwise-xor a b]
        (let
          ((av {reduce-expression self a})
            (bv {reduce-expression self b}))
          (+ av bv)))
      (['@app 'randomUInt256]
        (randomUInt256))
      (['@tuple . es]
        (list->vector
        (for/collect ((e es))
          {reduce-expression self e})))
      (['digest . es]
        (digest
        (for/collect ((e es))
          (cons {lookup-type (@ self program) e}
                {reduce-expression self e}))))
      (['sign digest-variable]
        (let ((this-participant {get-active-participant self})
              (digest {reduce-expression self digest-variable}))
          (make-message-signature (secret-key<-address this-participant) digest)))
      (['input 'Nat tag]
        (let ((tagv {reduce-expression self tag}))
          (input UInt256 tagv)))
      (else
        {reduce-expression self expression}))))

;; : Frame <- Runtime Block (Table Offset <- Symbol) Symbol
(defmethod {create-frame-variables Runtime}
  (λ (self timer-start contract-runtime-labels code-block-label code-block-participant)
    (DBG c-f-v: code-block-label)
    (def checkpoint-location
      (hash-get contract-runtime-labels {make-checkpoint-label self code-block-label}))
    (def active-participant-offset
      {lookup-variable-offset self code-block-label code-block-participant})
    (def agreement (@ self agreement))
    (def participants (.@ agreement participants))
    ;; TODO better way to get the difference between two lists. Use sets instead?
    (def live-variables
      (lset-difference equal? {lookup-live-variables (@ self program) code-block-label} (.all-slots participants)))
    (DBG live-variables: live-variables)
    ;; TODO: ensure keys are sorted in both hash-values
    [[UInt16 . checkpoint-location]
     [Block . timer-start]
     ;; [UInt16 . active-participant-offset]
     ;; TODO: designate participant addresses as global variables that are stored outside of frames
     (map (λ (slot-name) (cons Address (.ref participants slot-name))) (.all-slots-sorted participants))...
     (map
        (λ (variable-name)
          (def variable-type {lookup-type (@ self program) variable-name})
          (def variable-value (hash-get (@ self environment) variable-name))
          (cons variable-type variable-value))
        (sort live-variables symbol<?))...]))

;; Block <- Frame
(def (timer-start<-frame-variables frame-variables)
  (cdadr frame-variables))

;; TODO: use [t . v] everywhere instead of [v t] ? and unify with sexp<-state in ethereum-runtime
;; Sexp <- Frame
(def (sexp<-frame-variables frame-variables)
  `(list ,@(map (match <> ([v t] `(list ,(sexp<- t v) ,(sexp<- Type t)))) frame-variables)))

;; TODO: Exclude checkpoints that have already been executed by the first active
;; participant.
;; : Bytes (Table Offset <- Symbol) <- Runtime
(defmethod {generate-consensus-runtime Runtime}
  (λ (self)
    (parameterize ((brk-start (box params-start@)))
      (def consensus-code {generate-consensus-code self})
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

;; : Bytes (Table Offset <- Symbol) <- Runtime
(defmethod {make-checkpoint-label Runtime}
  (λ (self checkpoint)
    (symbolify (@ self program name) "--" checkpoint)))

;; <- Runtime
(defmethod {compute-variable-offsets Runtime}
  (λ (self code-block-label)
    (def frame-variables (make-hash-table))
    ;; Initial offset computed by global registers, see :mukn/ethereum/contract-runtime
    (def start params-start@)
    (def agreement (@ self agreement))
    (def participants (.@ agreement participants))
    (for-each (λ (role)
                 (let (parameter-length (param-length Address))
                   (hash-put! frame-variables role (post-increment! start parameter-length))))
              (.all-slots-sorted participants))
    (def live-variables
      (lset-difference equal? {lookup-live-variables (@ self program) code-block-label} (.all-slots participants)))
    (for-each
      (λ (live-variable)
        (let* ((type {lookup-type (@ self program) live-variable})
               (parameter-length (param-length type)))
          (hash-put! frame-variables live-variable (post-increment! start parameter-length))))
      (sort live-variables symbol<?))
    (hash-put! (@ self variable-offsets) code-block-label frame-variables)
    (set! (@ self params-end) start)))

;; Offset <- Runtime Symbol
(defmethod {lookup-variable-offset Runtime}
  (λ (self code-block-label variable-name)
    (def offset
      (hash-get (hash-get (@ self variable-offsets) code-block-label) variable-name))
    (if offset
      offset
      (error "No offset for variable: " variable-name))))

;; Assembly directives to load an immediate variable (i.e. for unboxed type) onto the stack
;; : Directives <- Runtime Symbol Type
(defmethod {load-immediate-variable Runtime}
  (λ (self code-block-label variable-name variable-type)
    (&mloadat
      {lookup-variable-offset self code-block-label variable-name}
      (param-length variable-type))))

;; Directives to load onto stack a representation for a trivial expression
;; use load-immediate-variable for types passed by value
;; use lookup-variable-offset for types passed by reference (Signature)
;; otherwise place a constant on the stack
;; : Directives <- Runtime
(defmethod {trivial-expression Runtime}
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
;; <- Runtime Symbol
(defmethod {add-local-variable-to-frame Runtime}
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
;; Directive <- Runtime
(defmethod {generate-consensus-code Runtime}
  (λ (self)
    (def consensus-interaction {get-interaction (@ self program) #f})
    (set! (@ self variable-offsets) (make-hash-table))
    (&begin*
     (append-map (match <> ([code-block-label . code-block]
                            {generate-consensus-code-block self code-block-label code-block}))
                 (hash->list/sort consensus-interaction symbol<?)))))

;; Directives from a code block
;; (List Directive) <- Runtime Symbol CodeBlock
(defmethod {generate-consensus-code-block Runtime}
  (λ (self code-block-label code-block)
    (def checkpoint-statements (code-block-statements code-block))
    (set! (@ self params-end) #f)
    {compute-variable-offsets self code-block-label}
    (def code-block-directives
      [[&jumpdest {make-checkpoint-label self code-block-label}]
       (&check-timeout! timeout: (.@ (@ self agreement) options timeoutInBlocks))
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
;; Symbol <- Runtime Symbol
(defmethod {find-other-participant Runtime}
  (λ (self participant)
    (find
      (λ (p) (not (equal? p participant)))
      (.all-slots-sorted (.@ (@ self agreement) participants)))))

;; (List Directive) <- Runtime Sexp
(defmethod {interpret-consensus-statement Runtime}
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
       (error "Runtime does not recognize consensus statement: " statement)))))

(def (typed-directive<-trivial-expr runtime code-block-label expr)
  (def program (@ runtime program))
  (cons {lookup-type program expr} {trivial-expression runtime code-block-label expr}))

(defmethod {interpret-consensus-expression Runtime}
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

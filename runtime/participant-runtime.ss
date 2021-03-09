(export #t)

(import
  :gerbil/gambit/bits :gerbil/gambit/bytes :gerbil/gambit/threads
  :std/pregexp
  :std/format :std/iter :std/misc/hash :std/sugar :std/misc/number :std/misc/list :std/sort :std/srfi/1 :std/text/json
  :clan/base :clan/exception :clan/io :clan/json :clan/number
  :clan/path :clan/path-config :clan/ports :clan/syntax :clan/timestamp
  :clan/poo/object :clan/poo/brace :clan/poo/io :clan/poo/debug :clan/debug :clan/crypto/random
  :clan/persist/content-addressing
  :mukn/ethereum/hex :mukn/ethereum/ethereum :mukn/ethereum/network-config :mukn/ethereum/json-rpc
  :mukn/ethereum/transaction :mukn/ethereum/tx-tracker :mukn/ethereum/watch :mukn/ethereum/assets
  :mukn/ethereum/evm-runtime :mukn/ethereum/contract-config :mukn/ethereum/assembly :mukn/ethereum/types
  (only-in ../compiler/alpha-convert/env symbol-refer)
  ./program ./block-ctx ./consensus-code-generator ./terminal-codes
  ../compiler/method-resolve/method-resolve
  ../compiler/project/runtime-2)

;; NB: Whichever function exports data end-users / imports from them should make sure to put in a Json array (Scheme list) prepend by the name of the type. And/or we may have a {"": "InteractionAgreement" ...} field with this asciibetically always-first name. Maybe such function belongs to gerbil-poo, too.

(define-type Tokens (MonomorphicObject Nat))

(define-type AgreementOptions
  (Record
   blockchain: [String] ;; e.g. "Cardano KEVM Testnet", as per ethereum_networks.json
   escrowAmount: [(Maybe Tokens) default: (void)] ;; not meaningful for all contracts
   timeoutInBlocks: [Nat]
   maxInitialBlock: [Nat]))

(define-type InteractionAgreement
  (Record
   glow-version: [String] ;; e.g. "Glow v0.0-560-gda782c9 on Gerbil-ethereum v0.0-83-g6568bc6" ;; TODO: have a function to compute that from versioning.ss
   interaction: [String] ;; e.g. "mukn/glow/dapps/buy_sig#payForSignature", fully qualified Gerbil symbol
   participants: [(MonomorphicObject Address)] ;; e.g. {Buyer: alice Seller: bob}
   parameters: [Json] ;; This Json object to be decoded according to a type descriptor from the interaction (dependent types yay!)
   reference: [(MonomorphicObject Json)] ;; Arbitrary reference objects from each participant, with some conventional size limits on the Json string.
   options: [AgreementOptions] ;; See above
   code-digest: [Digest])) ;; Make it the digest of Glow source code (in the future, including all Glow libraries transitively used)

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

;; split-interaction-path-name : String -> (values String Symbol)
(def (split-interaction-path-name interaction-str)
  (match (pregexp-match "^([^#]*)#([^#]*)$" interaction-str)
    ([_ path name] (values path (string->symbol name)))
    (else (error "Bad interaction name" interaction-str))))

(def (delete-agreement-handshake)
  (def file (special-file:handshake))
  (displayln "Deleting any old agreement handshake file " file " ...")
  (ignore-errors (delete-file file)))

(def (special-file:handshake) (transient-path "agreement-handshake.json"))
(def (handshake-timeout-in-seconds) (* 15 (ethereum-timeout-in-blocks)))

;; TODO: make an alternate version of io-context that
;;       displays at the terminal for the user to copy/paste and send to
;;       other participants through an outside channel
(.def io-context:special-file
  setup: delete-agreement-handshake
  teardown: delete-agreement-handshake
  send-handshake:
  (λ (handshake)
    (def file (special-file:handshake))
    (displayln "Writing agreement handshake to file " file " ...")
    (create-directory* (path-parent file))
    (write-file-json (special-file:handshake) (json<- AgreementHandshake handshake)))
  receive-handshake:
  (λ ()
    (def file (special-file:handshake))
    (def deadline (+ (current-unix-time) (handshake-timeout-in-seconds)))
    (displayln "Waiting for agreement handshake file " file " ...")
    (until (file-exists? file)
      (when (> (current-unix-time) deadline)
        (displayln "Timeout while waiting for handshake!")
        (error "Timeout while waiting for handshake"))
      (thread-sleep! 1))
    (<-json AgreementHandshake (read-file-json file))))

;; PARTICIPANT RUNTIME

;; TODO: derive the contract from the agreement,
;;       check that the code-digest in the agreement matches
(define-type Runtime
  (.+
   (Record
    role: [Symbol]
    agreement: [InteractionAgreement]
    contract-config: [ContractConfig]
    status: SExp ;; (Enum running completed aborted stopped)
    processed-events: [(List SExp)] ;; : (List LogObjects) ;; ???
    unproceed-events: [(List SExp)] ;; : (List LogObjects) ;; ???
    current-code-block-label: [Symbol]
    current-label: [Symbol]
    current-debug-label: [(Or Symbol False)]
    environment: [(Map (Or Any Any) <- Symbol)] ;; (Table (Or DependentPair Any) <- Symbol) ;; TODO: have it always typed???
    block-ctx: [BlockCtx] ;; byte buffer?
    timer-start: [Block]
    io-context: [IOContext]
    program: [Program]
    name: [Symbol]
    consensus-code-generator: [ConsensusCodeGenerator]
   )
   {.make: (lambda (role: role
                    agreement: agreement
                    io-context: (io-context io-context:special-file)
                    program: program)
             (let* (((values modpath surface-name)
                     (split-interaction-path-name (.@ agreement interaction)))
                    (name
                     (symbol-refer (hash-ref (.@ program compiler-output) 'AlphaEnv)
                                   surface-name))
                    (interaction-info
                     (hash-ref (.@ program interactions) name))
                    (self
                     { role
                       agreement
                       contract-config: #f
                       status: 'running
                       processed-events: '()
                       unprocessed-events: '()
                       current-code-block-label: (.@ interaction-info initial-code-block-label)  ;; TODO: extract initial code block label from contract compiler output
                       current-label: (.@ program initial-label)
                       current-debug-label: #f
                       environment: (make-hash-table)
                       block-ctx: #f
                       timer-start: #f
                       io-context
                       program
                       name
                       consensus-code-generator: (.call ConsensusCodeGenerator .make program name (.@ agreement options timeoutInBlocks))
                       }))
               (set! (.@ self consensus-code-generator)
                 (.call ConsensusCodeGenerator .make program name (.@ agreement options timeoutInBlocks)))
               (.call ConsensusCodeGenerator .generate (.@ self consensus-code-generator))
               (initialize-environment self)
               self))}))

;; <- Runtime
(def (execute self)
  (with-logged-exceptions ()
    (def ccbl (.@ self current-code-block-label))

    (if (is-active-participant self)
      (publish self)
      (receive self))
    (set! (.@ self block-ctx) #f)

    (match (code-block-exit (get-current-code-block self))
      (#f
        (displayln)) ; contract finished
      (exit
        (set! (.@ self current-code-block-label) exit)
        (execute self)))))

;; Bool <- Runtime
(def (is-active-participant self)
  (def current-code-block (get-current-code-block self))
  (equal? (.@ self role) (code-block-participant current-code-block)))

;; TODO: everything about this function, from the timer-start and/or wherever we left off
;; to timeout or (indefinite future if no timeout???)
;; : LogObject <- Runtime Address Block
(def (watch self contract-address from-block)
  ;; TODO: consult unprocessed log objects first, if none is available, then use getLogs
  ;; TODO: be able to split getLogs into smaller requests if it a bigger request times out.
  ;; TODO: (optional) push all the previously processed log objects to the processed list after processing
  (let/cc return
    (def callback (λ (log) (return log))) ;; TODO: handle multiple log entries!!!
    (def to-block (+ from-block (.@ (.@ self agreement) options timeoutInBlocks)))
    (watch-contract callback contract-address from-block to-block)))

(def (run-passive-code-block/contract self role contract-config)
  (displayln BOLD "\nWaiting for " (code-block-participant (get-current-code-block self)) " to make a move ..." END)
  ;; TODO: `from` should be calculated using the deadline and not necessarily the previous tx,
  ;; since it may or not be setting the deadline
  (def from
    (if (.@ self timer-start)
      (+ (.@ self timer-start) 1)
      (.@ contract-config creation-block)))
  (def new-log-object (watch self (.@ contract-config contract-address) from))
  ;; TODO: handle the case when there is no log objects
  (def log-data (.@ new-log-object data))
  (set! (.@ self timer-start) (.@ new-log-object blockNumber))
  ;; TODO: process the data in the same method?
  (set! (.@ self block-ctx) (.call PassiveBlockCtx .make log-data))
  (interpret-current-code-block self))

(def (interpret-current-code-block self)
  (let (code-block (get-current-code-block self))
    (displayln BOLD "\nExecuting code block " (.@ self current-code-block-label) " ..." END)
    (for ((statement (code-block-statements code-block)))
      (interpret-participant-statement self statement))))

(def (run-passive-code-block/handshake self role)
  (nest
   (let (agreement-handshake (read-handshake self)))
   (begin
     (force-current-outputs))
   (with-slots (agreement contract-config published-data) agreement-handshake)
   (let (block-ctx (.@ self block-ctx)))
   (begin
     (set! (.@ self block-ctx) (.call PassiveBlockCtx .make published-data))
     ;; TODO: Execute contract until first change participant.
     ;; Check that the agreement part matches
     (unless (equal? (json<- InteractionAgreement (.@ self agreement))
                     (json<- InteractionAgreement agreement))
       (DDT agreements-mismatch:
            InteractionAgreement (.@ self agreement)
            InteractionAgreement agreement)
       (error "agreements don't match" (.@ self agreement) agreement))
     (set! (.@ self timer-start) (.@ agreement options maxInitialBlock))
     (interpret-current-code-block self))
   (let (create-pretx (prepare-create-contract-transaction self))
     (verify-contract-config contract-config create-pretx)
     (set! (.@ self contract-config) contract-config))))

;; TODO: rename to RunPassiveCodeBlock or something
;; <- Runtime
(def (receive self)
  (def role (.@ self role))
  (def contract-config (.@ self contract-config))
  (when (eq? (.@ self status) 'running)
    (if contract-config
      (run-passive-code-block/contract self role contract-config)
      (run-passive-code-block/handshake self role))))

;; : AgreementHandshake <- Runtime
(def (read-handshake self)
  (def io-context (.@ self io-context))
  (.call io-context receive-handshake))

;; <- Runtime
(def (publish self)
  (def role (.@ self role))
  (def contract-config (.@ self contract-config))
  (set! (.@ self block-ctx) (.call ActiveBlockCtx .make))
  (when contract-config
    (publish-frame-data self))
  (interpret-current-code-block self)
  (when (eq? (.@ self status) 'running)
    (if (not contract-config)
      (let ()
        (deploy-contract self)
        (def contract-config (.@ self contract-config))
        (def agreement (.@ self agreement))
        (def published-data (get-output-u8vector (.@ (.@ self block-ctx) outbox)))
        (def handshake (.new AgreementHandshake agreement contract-config published-data))
        (send-contract-handshake self handshake))
      (let ()
        ;; TODO: Verify asset transfers using previous transaction and balances
        ;; recorded in Message's asset-transfer table during interpretation. Probably
        ;; requires getting TransactionInfo using the TransactionReceipt.
        (def contract-address (.@ contract-config contract-address))
        (def message-pretx (prepare-call-function-transaction self contract-address))
        (def new-tx-receipt (post-transaction message-pretx))
        (set! (.@ self timer-start) (.@ new-tx-receipt blockNumber))))))

;; Sexp <- State
(def (sexp<-state state) (map (match <> ([t . v] (sexp<- t v))) state))

;; TODO: include type output, too, looked up in type table.
;; <- Runtime Symbol Value
(def (add-to-environment self name value)
  (hash-put! (.@ self environment) name value))

;; PreTransaction <- Runtime Block
(def (prepare-create-contract-transaction self)
  (def sender-address (get-active-participant self))
  (def code-block (get-current-code-block self))
  (def next (code-block-exit code-block))
  (def participant (code-block-participant code-block))
  (def initial-state
    (create-frame-variables
      self
      (.@ (.@ self agreement) options maxInitialBlock)
      next
      participant))
  (def initial-state-digest
    (digest-product-f initial-state))
  (def contract-bytes
    (stateful-contract-init initial-state-digest (.@ (.@ self consensus-code-generator) bytes)))
  (create-contract sender-address contract-bytes
    value: (.@ (.@ self block-ctx) deposits)))

;; PreTransaction <- Runtime Block
(def (deploy-contract self)
  (def role (.@ self role))
  (def timer-start (.@ (.@ self agreement) options maxInitialBlock))
  (def pretx (prepare-create-contract-transaction self))
  (def receipt (post-transaction pretx))
  (def contract-config (contract-config<-creation-receipt receipt))
  (verify-contract-config contract-config pretx)
  (set! (.@ self contract-config) contract-config))

(def (send-contract-handshake self handshake)
  (def io-context (.@ self io-context))
  (.call io-context send-handshake handshake))

(def (publish-frame-data self)
  (def out (.@ (.@ self block-ctx) outbox))
  (def frame-variables
    (create-frame-variables
      self
      (.@ self timer-start)
      (.@ self current-code-block-label)
      (.@ self role)))
  (def frame-variable-bytes (marshal-product-f frame-variables))
  (def frame-length (bytes-length frame-variable-bytes))
  (marshal UInt16 frame-length out)
  (marshal-product-to frame-variables out))

;; See gerbil-ethereum/evm-runtime.ss for spec.
;; PreTransaction <- Runtime Message.Outbox Block Address
(def (prepare-call-function-transaction self contract-address)
  (def out (.@ (.@ self block-ctx) outbox))
  (marshal UInt8 1 out)
  (def message-bytes (get-output-u8vector out))
  (def sender-address (get-active-participant self))
  (call-function sender-address contract-address message-bytes
    ;; default gas value should be (void), i.e. ask for an automatic estimate,
    ;; unless we want to force the TX to happen, e.g. so we can see the failure in Remix
    gas: 1000000 ;; XXX ;;<=== DO NOT COMMIT THIS LINE UNCOMMENTED
    value: (.@ (.@ self block-ctx) deposits)))

;; CodeBlock <- Runtime
(def (get-current-code-block self)
    (def participant-interaction
      (get-interaction (.@ self program) (.@ self name) (.@ self role)))
    (hash-get participant-interaction (.@ self current-code-block-label)))

;; TODO: map alpha-converted names to names in original source when displaying to user
;;       using the alpha-back-table
;; <- Runtime
(def (initialize-environment self)
  (def inter (hash-ref (.@ (.@ self program) interactions) (.@ self name)))
  (def alba (hash-ref (.@ (.@ self program) compiler-output) 'albatable.sexp))
  (def agreement (.@ self agreement))
  (def participants (.@ agreement participants))
  (for (participant-name (filter symbol? (hash-keys (.@ inter specific-interactions))))
    (def participant-surface-name (hash-ref alba participant-name))
    (add-to-environment self participant-name (.ref participants participant-surface-name)))
  (def parameters (.@ agreement parameters))
  (for (parameter-name (.@ inter parameter-names))
    (def parameter-surface-name (hash-ref alba parameter-name))
    (def parameter-json-value (json-object-ref parameters parameter-surface-name))
    (def parameter-type (lookup-type (.@ self program) parameter-name))
    (def parameter-value (<-json parameter-type parameter-json-value))
    (add-to-environment self parameter-name parameter-value)))

;; TODO: make sure everything always has a type ???
;; Any <- Runtime
(def (reduce-expression self expression)
  (cond
   ((symbol? expression)
    (match (hash-ref/default
            (.@ self environment) expression
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
    expression)))

;; Symbol <- Runtime
(def (get-active-participant self)
  (def environment (.@ self environment))
  (hash-get environment (.@ self role)))

;; Bytes <- (List DependentPair)
(def (marshal-product-f fields)
  (call-with-output-u8vector (λ (out)
    (marshal-product-to fields out))))
;; <- (List DependentPair) BytesOutputPort
(def (marshal-product-to fields port)
  (for ((p fields))
    (with (([t . v] p)) (marshal t v port))))

;; : Digest <- (List DependentPair)
(def (digest-product-f fields)
  (digest<-bytes (marshal-product-f fields)))

;; : <- Runtime ProjectStatement
(def (interpret-participant-statement self statement)
  (displayln statement)
  (match statement

    (['set-participant new-participant]
      ;; Since the contract has already been separated into transaction boundaries,
      ;; the participant doesn't need to do anything here, since the active participant
      ;; is already known.
     (void))

    (['add-to-deposit amount-variable]
     (let
       ((this-participant (get-active-participant self))
        (amount (reduce-expression self amount-variable)))
       (.call BlockCtx .add-to-deposit (.@ self block-ctx) this-participant amount)))

    (['expect-deposited amount-variable]
     (let
       ((this-participant (get-active-participant self))
        (amount (reduce-expression self amount-variable)))
       (.call BlockCtx .add-to-deposit (.@ self block-ctx) this-participant amount)))

    (['participant:withdraw address-variable price-variable]
     (let ((address (reduce-expression self address-variable))
           (price (reduce-expression self price-variable)))
       (.call BlockCtx .add-to-withdraw (.@ self block-ctx) address price)))

    (['add-to-publish ['quote publish-name] variable-name]
     (let ((publish-value (reduce-expression self variable-name))
           (publish-type (lookup-type (.@ self program) variable-name)))
       (.call ActiveBlockCtx .add-to-published (.@ self block-ctx)
              publish-name publish-type publish-value)))

    (['def variable-name expression]
     (let
       ((variable-value (interpret-participant-expression self expression)))
       (add-to-environment self variable-name variable-value)))

    (['require! variable-name]
     (match (reduce-expression self variable-name)
       (#t (void))
        ;; TODO: include debugging information when something fails!
       (#f
        (error "Assertion failed"))
       (n
        (error "Assertion failed, " variable-name " is not a Boolean."))))

    (['return ['@tuple]]
      (void))

    (['return expression]
      (interpret-participant-expression self expression))

    (['@label name]
     (set! (.@ self current-label) name))
    (['@debug-label name]
     (set! (.@ self current-debug-label) name))

    (['switch variable-name cases ...]
      (let* ((variable-value (reduce-expression self variable-name))
             (matching-case (find (λ (case) (equal? (reduce-expression self (car case)) variable-value)) cases)))
        (for (case-statement (cdr matching-case))
          (interpret-participant-statement self case-statement))))))

(def (interpret-participant-expression self expression)
  (match expression

    (['expect-published ['quote publish-name]]
     (let (publish-type (lookup-type (.@ self program) publish-name))
       (.call PassiveBlockCtx .expect-published (.@ self block-ctx) publish-name publish-type)))

    (['@app 'isValidSignature address-variable digest-variable signature-variable]
     (let
       ((address (reduce-expression self address-variable))
        (digest (reduce-expression self digest-variable))
        (signature (reduce-expression self signature-variable)))
       (isValidSignature address digest signature)))

    (['@app '< a b]
     (let
       ((av (reduce-expression self a))
        (bv (reduce-expression self b)))
       (< av bv)))

    (['@app '+ a b]
     (let
       ((av (reduce-expression self a))
        (bv (reduce-expression self b)))
       (+ av bv)))

    (['@app '- a b]
     (let
       ((av (reduce-expression self a))
        (bv (reduce-expression self b)))
       (- av bv)))

    (['@app '* a b]
     (let
       ((av (reduce-expression self a))
        (bv (reduce-expression self b)))
       (* av bv)))

    (['@app 'bitwise-xor a b]
     (let
       ((av (reduce-expression self a))
        (bv (reduce-expression self b)))
       (bitwise-xor av bv)))

    (['@app 'bitwise-and a b]
     (let
       ((av (reduce-expression self a))
        (bv (reduce-expression self b)))
       (bitwise-and av bv)))

    (['@app 'mod a b]
      (let
       ((av (reduce-expression self a))
        (bv (reduce-expression self b)))
       (modulo av bv)))

    (['@app 'randomUInt256]
     (randomUInt256))

    ;; WARNING: This does not support re-entrancy!
    ;; TODO: Enable re-entrancy.
    (['@app name . argument-names]
      (let* ((small-function (hash-get (.@ (.@ self program) small-functions) name))
             (zipped-arguments (map cons argument-names (.@ small-function arguments))))
        (unless small-function
          (error "Unknown function " name))
        (for ((zipped-argument zipped-arguments))
          (add-to-environment self (cdr zipped-argument) (reduce-expression self (car zipped-argument))))
        (let/cc return
          (for ((statement (.@ small-function body)))
            (let (result (interpret-participant-statement self statement))
              (when (equal? (car statement) 'return) (return result)))))))

    (['@tuple . es]
     (list->vector
      (for/collect ((e es))
        (reduce-expression self e))))

    (['digest . es]
     (digest
      (for/collect ((e es))
        (cons (lookup-type (.@ self program) e)
              (reduce-expression self e)))))

    (['sign digest-variable]
     (let ((this-participant (get-active-participant self))
           (digest (reduce-expression self digest-variable)))
       (make-message-signature (secret-key<-address this-participant) digest)))

    (['input 'Nat tag]
     (let ((tagv (reduce-expression self tag)))
       (interaction-input UInt256 tagv)))

    (['== a b]
      (let ((av (reduce-expression self a))
            (bv (reduce-expression self b)))
        (equal? av bv)))

    (else
      (reduce-expression self expression))))

;; : Frame <- Runtime Block (Table Offset <- Symbol) Symbol
(def (create-frame-variables self timer-start code-block-label code-block-participant)
  (def consensus-code-generator (.@ self consensus-code-generator))
  (def checkpoint-location
    (hash-get (.@ consensus-code-generator labels) (make-checkpoint-label (.@ self name) code-block-label)))
  (def active-participant-offset
    (lookup-variable-offset consensus-code-generator code-block-label code-block-participant))
  (def live-variables (lookup-live-variables (.@ self program) (.@ self name) code-block-label))
  ;; TODO: ensure keys are sorted in both hash-values
  [[UInt16 . checkpoint-location]
   [Block . timer-start]
   ;; [UInt16 . active-participant-offset]
   ;; TODO: designate participant addresses as global variables that are stored outside of frames
   (map
     (λ (variable-name)
       (def variable-type (lookup-type (.@ self program) variable-name))
       (def variable-value (hash-get (.@ self environment) variable-name))
       [variable-type . variable-value])
     (sort live-variables symbol<?))...])

(def (interaction-input t s)
  (printf (string-append CYAN "\n~a [~s]\n" END) (if (u8vector? s) (bytes->string s) s) (.@ t sexp))
  (display (string-append CYAN "> " END))
  (def result (<-json t (read-json (current-input-port))))
  (displayln)
  result)

;; Block <- Frame
(def (timer-start<-frame-variables frame-variables)
  (cdadr frame-variables))

;; TODO: use [t . v] everywhere instead of [v t] ? and unify with sexp<-state in participant-runtime
;; Sexp <- Frame
(def (sexp<-frame-variables frame-variables)
  `(list ,@(map (match <> ([v t] `(list ,(sexp<- t v) ,(sexp<- Type t)))) frame-variables)))

;; json-object-ref : JsonObject StringOrSymbol -> Json
(def (json-object-ref j k)
  (hash-ref/default j k
    (lambda ()
      (hash-ref j
        (cond ((symbol? k) (symbol->string k))
              (else        (string->symbol k)))))))

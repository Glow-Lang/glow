(export #t)

(import
  :gerbil/gambit/ports
  :std/srfi/1 :std/sugar
  :std/misc/list :std/misc/number :std/misc/ports
  :clan/persist/content-addressing :clan/persist/db
  :clan/poo/io :clan/poo/poo
  :clan/path-config :clan/syntax
  :mukn/ethereum/assembly :mukn/ethereum/types :mukn/ethereum/ethereum :mukn/ethereum/network-config
  :mukn/ethereum/transaction :mukn/ethereum/tx-tracker :mukn/ethereum/json-rpc
  :mukn/ethereum/contract-runtime :mukn/ethereum/signing :mukn/ethereum/assets
  :mukn/ethereum/known-addresses :mukn/ethereum/contract-config)


(defalias λ lambda)

; INIT
;; A: requesting initialization parameters from the end-user based on spec produced by compiler
;; A: parsing the user inputs from command line
;; A: aggregate and present the parameters to user as json

;; J: producing the on-chain contract with initialization parameters

; EXECUTION
;; J: applying end-user inputs to a transaction <- not until RPS
;; J: applying published values to a transaction
;; J: constructing the activation frame and posting transaction

;; J:replacing variable names (per code block) with numbers representing offsets in on-chain memory

; PERSISTENCE
;; F: have post transaction handle persistent continuations

(def (initialize Buyer Seller)
  (ensure-db-connection (run-path "testdb"))
  (load-ethereum-networks "./etc/ethereum_networks.json")
  (ensure-ethereum-network "pet")
  (register-keypair
    "Buyer"
    (keypair ;; KLUGE: Fake public and secret key data. We only use the address via Geth.
      Buyer
      (<-json PublicKey "0x020000000000000000000000000000000000000000000000000000000000000001")
      (<-json SecretKey "0x0000000000000000000000000000000000000000000000000000000000000001")
      ""))
  (register-keypair
    "Seller"
    (keypair ;; KLUGE: Fake public and secret key data. We only use the address via Geth.
      Seller
      (<-json PublicKey "0x020000000000000000000000000000000000000000000000000000000000000001")
      (<-json SecretKey "0x0000000000000000000000000000000000000000000000000000000000000001")
      ""))
  (ensure-eth-signing-key Buyer)
  (ensure-eth-signing-key Seller))

(def (generate-buy-sig)
  (def program
    (parse-project-output "./examples/buy_sig.project.sexp"))
  (def participants
    (hash
      (Buyer #u8(197 78 134 223 251 135 185 115 110 46 53 221 133 199 117 53 143 28 49 206))
      (Seller #u8(244 116 8 20 61 50 126 75 198 168 126 244 167 10 78 10 240 155 154 28))))
  (def arguments
    (hash
      (digest0 [(string->bytes "abcdefghijklmnopqrstuvwxyz012345") Digest])
      (price [10000000 Ether])))

  (make-Interpreter
    program: program
    participants: participants
    arguments: arguments))

(def (run)
  (def buy-sig (generate-buy-sig))
  {initialize buy-sig}
  {execute buy-sig 'Buyer})

  ;(displayln "BUYER")
  ;(def buy-sig (payForSignature/Buyer #f Buyer Seller digest0 price))
  ;(displayln "SELLER")
  ;(payForSignature/Seller #f Buyer Seller digest0 price message-file))

; PROGRAM
(def (parse-project-output file-path)
  (def project-output-file (open-file file-path))
  (def project-output (read project-output-file))
  (extract-program project-output))

(defclass Program (name arguments interactions)
  transparent: #t)

(defmethod {:init! Program}
  (λ (self (n "") (as []) (is #f))
    (set! (@ self name) n)
    (set! (@ self arguments) as)
    (set! (@ self interactions) (if is is (make-hash-table)))))

(defmethod {get-interaction Program}
  (λ (self participant)
    (hash-get (@ self interactions) participant)))

; INTERPRETER
(defclass Interpreter (program participants arguments variable-addresses params-end)
  transparent: #t)

(defmethod {initialize Interpreter}
  (λ (self)
    (ensure-db-connection (run-path "testdb"))
    (load-ethereum-networks "./etc/ethereum_networks.json")
    (ensure-ethereum-network "pet")
    (for-each! (hash->list (@ self participants)) (λ (participant)
      (defvalues (name address) (values (car participant) (cdr participant)))
      (register-keypair
        (symbol->string name)
        (keypair ;; KLUGE: Fake public and secret key data. We only use the address via Geth.
          address
          (<-json PublicKey "0x020000000000000000000000000000000000000000000000000000000000000001")
          (<-json SecretKey "0x0000000000000000000000000000000000000000000000000000000000000001")
          ""))
      (ensure-eth-signing-key address)))))

(defmethod {interpret Interpreter}
  (λ (self participant)
    (def program-x (@ self program))
    (match (hash-get (hash-get (@ program-x interactions) participant) 'begin0)
      ((code-block statements exits)
        {initialize self participant}
        (map (λ (statement) {interpret-statement self statement}) statements))
      (#f
        (error (string-append participant " missing"))))))

(defmethod {execute Interpreter}
  (λ (self participant)
    (def timeoutInBlocks
      (.@ (current-ethereum-network) timeoutInBlocks))
    (def initial-block
      (+ (eth_blockNumber) timeoutInBlocks))
    {generate-parameter-addresses self}
    (defvalues (contract-runtime-bytes contract-runtime-labels)
      {generate-consensus-runtime self})
    (def checkpoint-label
      {make-checkpoint-label self})
    (def checkpoint-location
      (hash-get contract-runtime-labels checkpoint-label))
    (def initial-state-fields
      (flatten1
       [[[checkpoint-location UInt16]]
        [[initial-block Block]]
        (map (λ (participant) [participant Address]) (hash-values (@ self participants)))
        (hash-values (@ self arguments))]))
    (def initial-state
      (digest-product initial-state-fields))
    (def contract-bytes
      (stateful-contract-init initial-state contract-runtime-bytes))
    (displayln "creating contract ...")
    (def pretx
      (create-contract (hash-get (@ self participants) participant) contract-bytes))
    (displayln "posting transaction ...")
    (def receipt
      (post-transaction pretx))
    (displayln "generating contract config ...")
    (def contract-config
      (contract-config<-creation-receipt receipt))
    (displayln "verifying contract config ...")
    (verify-contract-config contract-config pretx)))

(defmethod {generate-consensus-runtime Interpreter}
  (λ (self)
    (parameterize ((brk-start (box params-start@)))
      (assemble
        (&begin
         &simple-contract-prelude
         &define-simple-logging
         (&define-check-participant-or-timeout)
         (&define-end-contract)
         {generate-consensus-code self}
         [&label 'brk-start@ (unbox (brk-start))])))))

; TODO: increment counter of checkpoints
(defmethod {make-checkpoint-label Interpreter}
  (λ (self)
    (string->symbol (string-append
      (symbol->string (@ (@ self program) name))
      "--cp0"))))

(defmethod {generate-parameter-addresses Interpreter}
  (λ (self)
    (def frame-variables (make-hash-table))
    (def start 0)
    (map (λ (participant)
      (match participant
        ([variable . value]
          (def parameter-length (param-length Address))
          (hash-put! frame-variables
            variable (post-increment! start parameter-length)))
        (else
          (error "invalid participant value: " participant))))
      (hash->list (@ self participants)))
    (map (λ (argument)
      (match argument
        ([variable _ type]
          (def argument-length (param-length type))
          (hash-put! frame-variables
            variable (post-increment! start argument-length)))
        (else
          (error "invalid argument value: " argument))))
      (hash->list (@ self arguments)))
    (set! (@ self variable-addresses) frame-variables)
    (set! (@ self params-end) start)))

(defmethod {lookup-variable-address Interpreter}
  (λ (self variable-name)
    (match (hash-get (@ self variable-addresses) variable-name)
      (#f
        (error "no address for variable: " variable-name))
      (address
        (displayln "found address for: " variable-name " at: " address)
        address))))

(defmethod {add-local-variable Interpreter}
  (λ (self variable-name)
    ; TODO: look this up in the type table
    (def type
      (if (eq? variable-name 'signature) Signature Bool))
    (def argument-length
      (param-length type))
    (hash-put! (@ self variable-addresses)
      variable-name (post-increment! (@ self params-end) argument-length))))

(defmethod {generate-consensus-code Interpreter}
  (λ (self)
    (def consensus-interaction
      {get-interaction (@ self program) #f})
    (def cp0-statements
      (code-block-statements (hash-get consensus-interaction 'cp0)))
    {generate-parameter-addresses self}
    (&begin*
      (cons
        [&jumpdest {make-checkpoint-label self}]
        (flatten1 (map (λ (statement)
          {interpret-consensus-statement self statement}) cp0-statements))))))

(defmethod {interpret-consensus-statement Interpreter}
  (λ (self statement)
    (def find-other-participant
      (λ (participant)
        (find (λ (p) (not (equal? p participant))) (hash-keys (@ self participants)))))
    (displayln statement)
    (match statement
      (['set-participant new-participant]
        (def other-participant
          (find-other-participant new-participant))
        [(&check-participant-or-timeout!
          must-act: (&mloadat {lookup-variable-address self new-participant} (param-length Address))
          or-end-in-favor-of: (&mloadat {lookup-variable-address self other-participant} (param-length Address)))])
      (['def variable-name expression]
        {add-local-variable self variable-name}
        (match expression
          (['expect-published published-variable-name]
            [{lookup-variable-address self variable-name} &read-published-data-to-mem])
          (['@app 'isValidSignature participant digest signature]
            [(&mloadat {lookup-variable-address self participant} (param-length Address))
             (&mloadat {lookup-variable-address self digest} (param-length Digest))
             ; signatures are passed by reference, not by value
             {lookup-variable-address self signature}
             &isValidSignature])))
      (['require! variable-name]
        [(&mloadat {lookup-variable-address self variable-name} (param-length Bool)) &require!])
      (['expect-withdrawn participant amount]
        [(&mloadat {lookup-variable-address self participant} (param-length Address))
         (&mloadat {lookup-variable-address self amount} (param-length Ether))
         &withdraw!])
      (['@label 'end0]
        [&end-contract!])
      (else
        (displayln "ignoring: " statement)))))

(def (digest-product fields)
  (digest<-marshal (λ (port)
    (map (λ (field)
      (match field ([value type]
        (marshal type value port)))) fields))))

(defmethod {interpret-off-chain-statement Interpreter}
  (λ (self statement)
    (match statement
      (['set-participant new-participant]
        (&check-participant-or-timeout!))
      (['add-to-deposit amount]
        (displayln amount))
      (['expect-deposited amount]
        (displayln amount)))))

(defclass ParseContext (current-participant current-label code)
  constructor: :init!
  transparent: #t)

(defmethod {:init! ParseContext}
  (λ (self (cp #f) (cl 'begin0) (c (make-hash-table)))
    (set! (@ self current-participant) cp)
    (set! (@ self current-label) cl)
    (set! (@ self code) c)))

(defmethod {add-statement ParseContext}
  (λ (self statement)
    (match (hash-get (@ self code) (@ self current-label))
      ((code-block statements exits)
        (let ((x (append statements [statement])))
          (hash-put! (@ self code) (@ self current-label) (make-code-block x exits))
          self))
      (#f
        self))))

(defstruct code-block (statements exit) transparent: #t)

(defmethod {set-participant ParseContext}
  (λ (self new-participant)
    (unless (and (@ self current-participant) (equal? new-participant (@ self current-participant)))
      (let (contract (@ self code))
        (match (hash-get contract (@ self current-label))
          ((code-block statements exits)
            (begin
              (match (last statements)
                (['@label last-label]
                  (def init-statements (take statements (- (length statements) 1)))
                  (hash-put! contract (@ self current-label) (make-code-block init-statements last-label))
                  (hash-put! contract last-label (make-code-block [['set-participant new-participant]] #f))
                  (set! (@ self current-participant) new-participant)
                  (set! (@ self current-label) last-label))
                (else
                  (error "change of participant with no preceding label")))))
          (#f
            (begin
              (set! (@ self current-participant) new-participant)
              (hash-put! contract (@ self current-label) (make-code-block [['set-participant new-participant]] #f))
              self)))))))


(def (extract-program statements)
  (def program (make-Program))
  (def (process-header-statement statement)
    (match statement
      (['def name ['@make-interaction [['@list participants ...]] arguments labels interactions ...]]
        (set! (@ program name) name)
        (set! (@ program arguments) arguments)
        (list->hash-table interactions))
      (else
        (displayln "ignoring: " statement))))
  (def raw-interactions (find hash-table? (map process-header-statement statements)))
  (def interactions-table (make-hash-table))
  (hash-map (λ (name body) (hash-put! interactions-table name (process-program name body))) raw-interactions)
  (set! (@ program interactions) interactions-table))

(def (process-program name body)
  (def parse-context (make-ParseContext))
  (for-each! body (λ (statement)
    (match statement
      (['participant:set-participant new-participant]
        {set-participant parse-context new-participant})
      (['consensus:set-participant new-participant]
        {set-participant parse-context new-participant})
      (else
        {add-statement parse-context statement}))))
  (@ parse-context code))
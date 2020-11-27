(export #t)

(import
  :gerbil/gambit/ports
  :std/srfi/1 :std/sugar :std/iter
  :std/misc/list :std/misc/number :std/misc/ports
  :clan/persist/content-addressing :clan/persist/db
  :clan/poo/io :clan/poo/poo
  :clan/path-config :clan/syntax
  :clan/base
  :mukn/ethereum/assembly :mukn/ethereum/types :mukn/ethereum/ethereum :mukn/ethereum/network-config
  :mukn/ethereum/transaction :mukn/ethereum/tx-tracker :mukn/ethereum/json-rpc
  :mukn/ethereum/contract-runtime :mukn/ethereum/signing :mukn/ethereum/assets
  :mukn/ethereum/known-addresses :mukn/ethereum/contract-config)

; INTERPRETER
(defclass Interpreter (program participants arguments variable-addresses params-end)
  transparent: #t)

(defmethod {initialize Interpreter}
  (λ (self)
    (for ((values name address) (in-hash (@ self participants)))
      (register-keypair
        (symbol->string name)
        (keypair ;; KLUGE: Fake public and secret key data. We only use the address via Geth.
          address
          (<-json PublicKey "0x020000000000000000000000000000000000000000000000000000000000000001")
          (<-json SecretKey "0x0000000000000000000000000000000000000000000000000000000000000001")
          ""))
      (ensure-eth-signing-key address))))

(defmethod {execute Interpreter}
  (λ (self participant)
    (def timeoutInBlocks
      (.@ (current-ethereum-network) timeoutInBlocks))
    (def initial-block
      (+ (eth_blockNumber) timeoutInBlocks))
    {compute-parameter-offsets self}
    (defvalues (contract-runtime-bytes contract-runtime-labels)
      {generate-consensus-runtime self})
    (def checkpoint-location
      (hash-get contract-runtime-labels {make-checkpoint-label self}))
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
    (displayln pretx)
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

(defmethod {compute-parameter-offsets Interpreter}
  (λ (self)
    (def frame-variables (make-hash-table))
    ;; Initial offset computed by global registers, see :mukn/ethereum/contract-runtime
    (def start params-start@)
    (for ((values variable value) (in-hash (@ self participants)))
      (def parameter-length (param-length Address))
      (hash-put! frame-variables
        variable (post-increment! start parameter-length)))
    (for ((values variable [_ type]) (in-hash (@ self arguments)))
      (def argument-length (param-length type))
      (hash-put! frame-variables
        variable (post-increment! start argument-length)))
    (set! (@ self variable-addresses) frame-variables)
    (set! (@ self params-end) start)))

(defmethod {lookup-variable-offset Interpreter}
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
    {compute-parameter-offsets self}
    (&begin*
      (cons
        [&jumpdest {make-checkpoint-label self}]
        (flatten1 (map (λ (statement)
          {interpret-consensus-statement self statement}) cp0-statements))))))

(defmethod {interpret-consensus-statement Interpreter}
  (λ (self statement)
    (def find-other-participant (λ (participant)
      (find (λ (p) (not (equal? p participant))) (hash-keys (@ self participants)))))
    (displayln statement)
    (match statement
      (['set-participant new-participant]
        (def other-participant
          (find-other-participant new-participant))
        ; TODO: support more than two participants
        [(&check-participant-or-timeout!
          must-act: (&mloadat {lookup-variable-offset self new-participant} (param-length Address))
          or-end-in-favor-of: (&mloadat {lookup-variable-offset self other-participant} (param-length Address)))])
      (['def variable-name expression]
        {add-local-variable self variable-name}
        (match expression
          (['expect-published published-variable-name]
            [{lookup-variable-offset self variable-name} &read-published-data-to-mem])
          (['@app 'isValidSignature participant digest signature]
            [(&mloadat {lookup-variable-offset self participant} (param-length Address))
             (&mloadat {lookup-variable-offset self digest} (param-length Digest))
             ; signatures are passed by reference, not by value
             {lookup-variable-offset self signature}
             &isValidSignature])))
      (['require! variable-name]
        [(&mloadat {lookup-variable-offset self variable-name} (param-length Bool)) &require!])
      (['expect-withdrawn participant amount]
        [(&mloadat {lookup-variable-offset self participant} (param-length Address))
         (&mloadat {lookup-variable-offset self amount} (param-length Ether))
         &withdraw!])
      (['@label 'end0]
        [&end-contract!])
      (else
        (displayln "ignoring: " statement)))))

(def (digest-product fields)
  (digest<-marshal (λ (port)
    (map (λ (field)
      (match field
        ([value type]
          (marshal type value port)))) fields))))

; PARSER
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
(export #t)

(import
  :gerbil/gambit/ports :std/misc/ports :std/misc/list :std/srfi/1
  :clan/persist/content-addressing :clan/poo/io :clan/poo/poo
  :mukn/ethereum/assembly :mukn/ethereum/types :mukn/ethereum/ethereum :mukn/ethereum/network-config
  :mukn/ethereum/transaction :mukn/ethereum/tx-tracker :mukn/ethereum/json-rpc
  :mukn/ethereum/contract-runtime :mukn/ethereum/signing :mukn/ethereum/assets)

(defvalues (payForSignature--contract-runtime payForSignature--labels)
  (parameterize ((brk-start (box params-start@)))
    (assemble
     (&begin
      &simple-contract-prelude
      &define-simple-logging
      (&define-check-participant-or-timeout)
      (&define-end-contract)
      (&payForSignature--cp0)
      [&label 'brk-start@ (unbox (brk-start))]))))


(def (parse-project-output file-path)
  (def project-output-file (open-file file-path))
  (def project-output (read project-output-file))
  (extract-program project-output))

(defclass Program (name arguments interactions) transparent: #t)

(defmethod {:init! Program}
  (lambda (self (n "") (as []) (is #f))
    (set! (@ self name) n)
    (set! (@ self arguments) as)
    (set! (@ self interactions) (if is is (make-hash-table)))))

(defclass Interpreter (program participants arguments))

(defmethod {interpret Interpreter}
  (lambda (self participant)
    (def program-x (@ self program))
    (match (hash-get (hash-get (@ self program-x interactions) participant) 'begin0)
      ((code-block statements exits)
        {initialize self participant}
        (map (lambda (statement) {interpret-statement self statement}) statements))
      (#f
        (error (string-append participant " missing"))))))

(defmethod {initialize Interpreter}
  (lambda (self participant)
    (begin
      (def timeoutInBlocks #f)
      (def initial-block #f)
        ;(+ (eth_blockNumber) timeoutInBlocks))
      (def initial-state
        (digest-product-f
          (append
            [(payForSignature--cp0 UInt16) (initial-block Block)]
            (@ self participants))
            (@ self arguments)))
      (def contract-bytes
        (stateful-contract-init initial-state payForSignature--contract-runtime))
      (def pretx
        (create-contract participant contract-bytes))
      (save-transaction% pretx))))

(def (digest-product-f fields)
  (digest<-marshal (lambda (port)
    (map (lambda (field)
      (match field ([value type]
        (marshal type value port)))) fields))))

(defmethod {interpret-statement Interpreter}
  (lambda (self statement)
    (match statement
      (['set-participant new-participant]
        (displayln new-participant))
      (['add-to-deposit amount]
        (displayln amount))
      (['expect-deposited amount]
        (displayln amount)))))

(defclass ParseContext (current-participant current-label code)
  constructor: :init!
  transparent: #t)

(defmethod {:init! ParseContext}
  (lambda (self (cp #f) (cl 'begin0) (c #f))
    (set! (@ self current-participant) cp)
    (set! (@ self current-label) cl)
    (set! (@ self code) (if c c (make-hash-table)))))

(defmethod {add-statement ParseContext}
  (lambda (self statement)
    (match (hash-get (@ self code) (@ self current-label))
      ((code-block statements exits)
        (let ((x (append statements [statement])))
          (hash-put! (@ self code) (@ self current-label) (make-code-block x exits))
          self))
      (#f
        self))))

(defstruct code-block (statements exit) transparent: #t)

(defmethod {set-participant ParseContext}
  (lambda (self new-participant)
    (if (and (@ self current-participant) (equal? new-participant (@ self current-participant)))
      self
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
        (begin
          (set! (@ program name) name)
          (set! (@ program arguments) arguments)
          (list->hash-table interactions)))
      (else [])))
  (def raw-interactions (find hash-table? (map process-header-statement statements)))
  (def interactions-table (make-hash-table))
  (hash-map (lambda (name body) (hash-put! interactions-table name (process-program name body))) raw-interactions)
  (set! (@ program interactions) interactions-table))

(def (process-program name body)
  (def parse-context (make-ParseContext))
  (for-each! body (lambda (statement)
    (match statement
      (['participant:set-participant new-participant]
        {set-participant parse-context new-participant})
      (['consensus:set-participant new-participant]
        {set-participant parse-context new-participant})
      (else
        {add-statement parse-context statement}))))
  (@ parse-context code))

;; Invoke a continuation. Shouldn't it also have a (dynamic?) thread context?
;; Indeed, we atomically deactivate a previous frame as we activate a new one,
;; and we have a list/set of active threads to reactivate.
(def (continue pk . vals) [pk vals])

;; TODO: modify save-transaction in tx-tracker so that it includes persistent continuation handling
;; as part of its own regular flow.
(def (save-transaction% pre-tx pk) (post-transaction pre-tx) (continue pk))



(def (&payForSignature--cp0)
  (define-frame-params &payForSignature--cp0
    (Buyer Address)
    (Seller Address)
    (digest0 Digest)
    (price Ether))
  (define-frame-locals &payForSignature--cp0 ;; NB: side-effects the brk-start@
    (signature Signature)
    #|(tmp Bool)|#)
  (&begin
   [&jumpdest 'payForSignature--cp0] ; label
   (&check-participant-or-timeout! must-act: Seller or-end-in-favor-of: Buyer) ; set-participant
   signature@ &read-published-data-to-mem ; expect-published
   Seller digest0 signature@ &isValidSignature #|tmp-set! tmp|# &require! ; isValidSignature
   Seller price &withdraw! ; withdraw
   &end-contract!))

(def payForSignature--cp0
  (hash-get payForSignature--labels 'payForSignature--cp0))
(export #t)

(import
  ; :gerbil/gambit/bytes :gerbil/gambit/ports
  ; :std/format :std/iter :std/srfi/1
  ; :std/misc/list :std/misc/number :std/misc/ports
  ; :clan/base :clan/exception :clan/json :clan/path-config :clan/ports :clan/syntax
  ; :clan/poo/io (only-in :clan/poo/mop display-poo sexp<- Type new) :clan/poo/poo
  ; :clan/persist/content-addressing :clan/persist/db
  ; :mukn/ethereum/assembly :mukn/ethereum/hex :mukn/ethereum/types
  ; :mukn/ethereum/ethereum :mukn/ethereum/network-config :mukn/ethereum/signing
  ; :mukn/ethereum/transaction :mukn/ethereum/tx-tracker :mukn/ethereum/json-rpc
  ; :mukn/ethereum/contract-runtime :mukn/ethereum/signing :mukn/ethereum/assets
  ; :mukn/ethereum/known-addresses :mukn/ethereum/contract-config :mukn/ethereum/hex
  ; <expander-runtime>
  ; ../compiler/method-resolve/method-resolve
  :std/iter :std/misc/ports
  :clan/base :clan/exception :clan/json
  :clan/pure/dict/assq :clan/poo/io (only-in :clan/poo/mop display-poo new)
  :mukn/ethereum/contract-config :mukn/ethereum/transaction :mukn/ethereum/tx-tracker
  :mukn/ethereum/ethereum :mukn/ethereum/json-rpc :mukn/ethereum/network-config
  ../compiler/project/runtime-2
  ./ethereum-interpreter)

(defclass ExecutionContext (role contract current-code-block current-label locals message)
  constructor: :init!
  transparent: #t)

(defmethod {:init! ExecutionContext}
  (lambda (self role (c #f) (ccb 'begin0) (cl 'begin) (l (make-hash-table)) (m (make-Message)))
    (set! (@ self role) role)
    (set! (@ self contract) c)
    (set! (@ self current-code-block) ccb)
    (set! (@ self current-label) cl)
    (set! (@ self locals) l)
    (set! (@ self message) m)))

(defmethod {add-to-locals ExecutionContext}
  (λ (self name value)
    (hash-put! (@ self locals) name value)))

(defclass Message (inbox outbox asset-transfers)
  constructor: :init!
  transparent: #t)

(defmethod {:init! Message}
  (λ (self (i (make-hash-table)) (o (make-hash-table)) (at []))
    (set! (@ self inbox) i)
    (set! (@ self outbox) o)
    (set! (@ self asset-transfers) at)))

(defmethod {add-to-published Message}
  (λ (self name type value)
    (hash-put! (@ self outbox) name [type . value])))

;; expect-published : Sym TypeMethods -> Any
(defmethod {expect-published Message}
  (λ (self _name type)
    ;; ignore name, by order not by name
    (unmarshal type (message-published (current-receiving-message)))))

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
  (lambda (self address amount)
    (def mat (@ self asset-transfers))
    (def mat2 (assq-update mat address (cut - <> amount) 0))
    (def mat3 (assq-update mat2 #f (cut + <> amount) 0))
    (set! (@ self asset-transfers) mat3)))

;; deploy contract or wait for other participant to make their move
(defmethod {change-participant Interpreter}
  (λ (self)
    (if (@ (@ self execution-context) contract)
      {wait self}
      {deploy-contract self})))

(defmethod {deploy-contract Interpreter}
  (λ (self)
    (displayln "Deploying contract ...")
    (def address (hash-get (@ self participants) (@ (@ self execution-context) role)))
    (def timeoutInBlocks (.@ (current-ethereum-network) timeoutInBlocks))
    (def initial-block (+ (eth_blockNumber) timeoutInBlocks))
    (def pretx {create-contract-pretransaction self initial-block address})
    (display-poo ["Deploying contract... " "timeoutInBlocks: " timeoutInBlocks
                  "initial-block: " initial-block "\n"])
    (def receipt (post-transaction pretx))
    (def contract-config (contract-config<-creation-receipt receipt))
    (display-poo ["Contract config: " ContractConfig contract-config "\n"])
    (verify-contract-config contract-config pretx)
    (def handshake (new ContractHandshake initial-block contract-config))
    (display-poo ["Handshake: " ContractHandshake handshake "\n"])
    (displayln "Please send this handshake to the other participant:\n```\n"
               (string<-json (json<- ContractHandshake handshake))
               "\n```\n")
    (write-file-json "contract-handshake.json" (json<- ContractHandshake handshake))))

(defmethod {execute Interpreter}
  (λ (self)
    (with-logged-exceptions ()
      (def code-block-label (@ (@ self execution-context) current-code-block))
      (displayln "executing code block: " code-block-label)
      (def code-block {get-current-code-block self})
      (if (eq? (@ (@ self execution-context) role) (code-block-participant code-block))
        ; if same participant
        (begin
          (for ((statement (code-block-statements code-block)))
            {interpret-participant-statement self statement})
          (match (code-block-exit code-block)
            (#f
              (void)) ; contract finished
            (exit
              (def ctx (@ self execution-context))
              (set! (@ ctx current-code-block ) exit)
              {execute self}))
        ; if other participant
        (begin
          (if (@ (@ self execution-context) contract)
            (void)
            {deploy-contract self})))))))

(defmethod {get-current-code-block Interpreter}
  (λ (self)
    (def ctx (@ self execution-context))
    (def participant-interaction
      {get-interaction (@ self program) (@ ctx role)})
    (hash-get participant-interaction (@ ctx current-code-block))))

(defmethod {resolve-variable Interpreter}
  (λ (self variable-name)
    (def variable-value
      (or
        (hash-get (@ self participants) variable-name)
        (hash-get (@ self arguments) variable-name)
        (hash-get (@ (@ self execution-context) locals) variable-name)))
    (match variable-value
      ([type . value]
        value)
      (#f
        (error (string-append variable-name " is missing from execution environment")))
      (value
        value))))

(defmethod {get-current-participant Interpreter}
  (λ (self)
    (def ctx (@ self execution-context))
    (hash-get (@ self participants) (@ ctx role))))

; (Seller
;   (hash
;     (begin0
;       (code-block
;         [['set-participant 'Buyer]
;         ['expect-deposited 'price]]
;         'cp0))
;     (cp0
;       (code-block
;         [['set-participant 'Seller]
;         ['def 'signature ['sign 'digest0]]
;         ['add-to-publish ['quote 'signature] 'signature]
;         ['def 'tmp ['@app 'isValidSignature 'Seller 'digest0 'signature]]
;         ['require! 'tmp]
;         ['add-to-withdraw 'Seller 'price]
;         ['return ['@tuple]]
;         ['@label 'end0]]
;         #f))))))
(defmethod {interpret-participant-statement Interpreter}
  (λ (self statement)
    (displayln statement)
    (match statement

      (['set-participant new-participant]
        (let (this-participant (@ (@ self execution-context) role))
          (if (eq? new-participant this-participant)
            (void) ; continue
            (error "Change participant inside code block, this should be impossible"))))

      (['add-to-deposit amount-variable]
        (let
          ((this-participant {get-current-participant self})
           (amount {resolve-variable self amount-variable}))
          {add-to-deposit (@ (@ self execution-context) message) this-participant amount}))

      (['expect-deposited amount-variable]
        (let
          ((this-participant {get-current-participant self})
           (amount {resolve-variable self amount-variable}))
          {expect-deposited (@ (@ self execution-context) message) this-participant amount}))

      (['participant:withdraw address-variable price-variable]
        (let
          ((address {resolve-variable self address-variable})
           (price {resolve-variable self price-variable}))
          {add-to-withdraw (@ (@ self execution-context) message) address price}))

      (['add-to-publish ['quote publish-name] variable-name]
        (let
          ((publish-value {resolve-variable self variable-name})
           (publish-type {lookup-type (@ self program) variable-name}))
          {add-to-published (@ (@ self execution-context) message) publish-name publish-type publish-value}))

      (['def variable-name expression]
        (let (variable-value
          (match expression
            (['expect-published ['quote publish-name]]
              (let (publish-type {lookup-type (@ self program) publish-name})
                {expect-published (@ (@ self execution-context) message) publish-name publish-type}))
            (['@app 'isValidSignature address-variable digest-variable signature-variable]
              (let
                ((address {resolve-variable self address-variable})
                 (digest {resolve-variable self digest-variable})
                 (signature {resolve-variable self signature-variable}))
                (isValidSignature address digest signature)))
            (['sign digest-variable]
              (let
                ((this-participant {get-current-participant self})
                 (digest {resolve-variable self digest-variable}))
                (make-message-signature (secret-key<-address this-participant) digest)))))
          {add-to-locals (@ self execution-context) variable-name variable-value}))

      (['require! variable-name]
        (match {resolve-variable self variable-name}
          (#t (void))
          (else
            (error (string-append "Assertion failed: " variable-name " is not #t")))))

      (['return ['@tuple]]
        (void))

      (['@label name]
        (set! (@ (@ self execution-context) current-label) name)))))

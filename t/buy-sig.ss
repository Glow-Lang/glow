;;; TODO: THIS PROGRAM IS INCOMPLETE. COMPLETE IT!

;; TODO: start the off-chain interaction with a one-sided proposal, and
;; use libp2p to do the off-chain communication.
;; Divide the agreement in two parts, the one that must agree exactly at all times,
;; the one that has "monotonic" information as the references and timer are filled in.


(export #t)

(import
  :std/sugar
  :clan/base :clan/files :clan/json :clan/path-config :clan/shell :clan/versioning
  :clan/poo/poo :clan/poo/io :clan/poo/brace
  :clan/crypto/keccak
  :clan/persist/content-addressing :clan/persist/db
  :mukn/ethereum/types :mukn/ethereum/ethereum :mukn/ethereum/network-config :mukn/ethereum/json-rpc
  :mukn/glow/runtime/participant-runtime :mukn/glow/runtime/reify-contract-parameters

  ;; TODO: make sure there's a one-stop-shop to all bindings required for the runtime to work.
  :mukn/ethereum/signing :mukn/ethereum/assets
  :mukn/glow/compiler/syntax-context
  )

;; : Quantity
(define-persistent-variable agreement-index Nat "agreementIndex" 0)

;; Hash the secret key to get a seed that only he who has the secret key knows,
;; but that doesn't actually leak secret key data.
(def (secret-hash keypair)
  (keccak256<-bytes (secp256k1-seckey-data (keypair-secret-key keypair))))

;; Marshal semi-structured information from x to either display to user or digest.
(def (marshalify x port)
  (cond
   ((member x '(#f #t () #!void #!eof)) (void))
   ((or (string? x) (symbol? x) (number? x)) (display x port))
   ((keypair? x) (write-bytes (secret-hash x)))
   ((keyword? x) (display (keyword->string x) port))
   ((bytes? x) (write-bytes x port))
   ((pair? x) (marshalify (car x) port) (marshalify (cdr x) port))
   ((vector? x) (marshalify (vector->list x) port))
   ((AST? x) (marshalify (stx-e x) port))
   (else (void))))

(def (hash<-context context)
  (keccak256<-bytes (call-with-output-u8vector (cut marshalify context <>))))

(def (make-agreement-reference address)
  (def index (with-committed-tx (tx) (post-increment! agreement-index)))
  (json<- Digest (hash<-context (cons (keypair<-address address) agreement-index))))

(def (make-buy-sig-agreement
      buyer: buyer seller: seller price: price digest: digest
      buyer-reference: (buyer-reference (void))
      seller-reference: (buyer-reference (void))
      maxInitialBlock: (maxInitialBlock #f))
  (set! block (or block (eth_blockNumber)))
  {;; To be filled "manually" from the user inputs
   interaction: "mukn/glow/examples/buy_sig#payForSignature"
   participants: (.o Buyer: buyer Seller: seller)
   parameters: (hash
                (digest0 (json<- Digest digest))
                (price (json<- Ether price)))

   ;; To be filled in automatically by the DApp infrastructure
   reference: (.o Buyer: buyer-reference Seller: seller-reference)

   ;; Filled in automatically by Glow
   glow-version: (software-identifier)
   code-digest: (digest<-file (source-path "examples/buy_sig.glow"))

   ;; Filled in automatically by the DApp from its blockchain configuration
   options: {blockchain: (.@ (ethereum-config) name)
             escrowAmount: (void)
             timeoutInBlocks: (ethereum-timeout-in-blocks)
             maxInitialBlock: (cond
                               ((number? maxInitialBlock) maxInitialBlock)
                               ((not maxInitialBlock) (+ (eth_blockNumber timeoutInBlocks)))
                               (else (void)))}})

(define-entry-point (make-buy-sig-agreement . arguments)
  "Start an interaction based on an agreement"
  )



(def (buy-sig role buyer: buyer seller: seller price: price digest: digest)
  (def agreement (make-buy-sig-agreement buyer: buyer seller: seller price: price digest: digest))

  ;; TODO: replace the statements below by off-chain communication between buyer and seller
  (create-directory* (run-path "t"))
  (clobber-file agreement-path (cut write-json-ln (json<- InteractionAgreement agreement) <>))
  (displayln
   "Here is an agreement, also saved to file " agreement-path "\n"
   agreement-string "\n\n"
   "Please start a Seller on another terminal or machine (with agreement copy) with:\n"
   "  ./glow start-interaction --database run/testdb-bob Seller " agreement-path "\n"
   "or\n"
   "  ./glow start-interaction --database run/testdb-bob Seller " (escape-shell-token agreement-string) "\n"
   "then press ENTER to start the Buyer in this terminal.")
  (read-line)

  ;; Run the Buyer program, get the final environment object from it.
  (def environment (run 'Buyer agreement))

  (def signature-tvp (hash-get environment 'signature))
  (def signature-t (car signature-tvp))
  (def signature-v (cdr signature-tvp))
  (display-poo-ln
   ["Signature extracted from DApp execution: " signature-t signature-v
    "valid?: " (message-signature-valid? bob signature-v digest)]))))

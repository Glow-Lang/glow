(export #t)

(import
  :std/getopt :std/format :std/iter :std/misc/hash :std/srfi/13 :std/sugar
  :clan/base :clan/config :clan/files :clan/json :clan/multicall :clan/syntax :clan/crypto/secp256k1
  :clan/poo/brace :clan/poo/cli :clan/poo/io :clan/poo/mop :clan/poo/object :clan/poo/type
  :mukn/ethereum/cli :mukn/ethereum/hex :mukn/ethereum/ethereum :mukn/ethereum/known-addresses :mukn/ethereum/network-config
  :mukn/ethereum/json-rpc)

(def (secret-key-ring)
  (xdg-config-home "glow" "secret-key-ring.json"))

(def (load-identities from: (from (secret-key-ring)))
  (with-catch
   (lambda (e)
     (displayln (error-message e))
     (error "Failed to read and parse the secret key ring file" from))
   (lambda ()
     (if (file-exists? from)
      (hash-key-value-map
        (lambda (nickname keypair-json)
          (def keypair (import-keypair/json keypair-json))
          (register-keypair nickname keypair)
          (cons nickname (import-keypair/json keypair-json)))
        (read-file-json from))
      (make-hash-table)))))

(def (store-identities identities from: (from (secret-key-ring)))
  (def identities-json
    (hash-key-value-map
      (lambda (nickname keypair)
        (cons nickname (export-keypair/json keypair)))
      identities))
  (clobber-file from (string<-json identities-json) salt?: #t))

(def (with-identities f from: (from (secret-key-ring)))
  (def identities
    (if (file-exists? from)
      (load-identities from: from)
      (make-hash-table)))
  (f identities)
  (store-identities identities from: from))

(def options/identities
  (make-options
    [(option 'identities "-I" "--identities" default: (secret-key-ring)
             help: "file to load and store identities")] []))

(define-entry-point (add-identity . arguments)
  "Add identity"
  (def options/add
    (make-options
      ;; TODO: Consolidate with contact options
      [(option 'nickname "-N" "--nickname"
               help: "nickname of identity")
       (option 'address "-A" "--address"
               help: "address of identity")
       (option 'public-key "-P" "--public-key"
               help: "public key of identity")
       (option 'secret-key "-S" "--secret-key"
               help: "secret key of identity")]
      []
      [options/test options/identities]))
  (def options (process-options options/add arguments))
  (def nickname (hash-get options 'nickname))
  (def new-keypair
    (keypair (<-string Address (hash-get options 'address))
             (<-string PublicKey (hash-get options 'public-key))
             (import-secret-key/bytes (<-string Bytes32 (hash-get options 'secret-key)))))
  (with-identities (cut hash-put! <> (string-downcase nickname) new-keypair)
    from: (hash-get options 'identities))
  (displayln "Added identity: " nickname " [ " (string<- Address (keypair-address new-keypair)) " ]"))

(define-entry-point (generate-identity . arguments)
  "Generate identity"
  (def options/generate
    (make-options
      [(option 'nickname "-N" "--nickname"
               help: "nickname of identity")
       (option 'prefix "-P" "--prefix" default: #f
               help: "hex prefix of generated address")]
      []
      [options/test options/identities]))
  (def options (process-options options/generate arguments))
  (def nickname (hash-get options 'nickname))
  (def scoring
    (if-let (prefix (hash-get options 'prefix))
      (scoring<-prefix prefix)
      trivial-scoring))
  (def keypair (generate-keypair scoring: scoring))
  (with-identities (cut hash-put! <> (string-downcase nickname) keypair)
    from: (hash-get options 'identities))
  (displayln "Generated identity: " nickname " [ " (string<- Address (keypair-address keypair)) " ]"))

(define-entry-point (remove-identity . arguments)
  "Remove identity"
  (def options/remove
    (make-options
      [(option 'nickname "-N" "--nickname")] [] [options/identities]))
  (def options (process-options options/remove arguments))
  (def nickname (hash-get options 'nickname))
  (with-identities (cut hash-remove! <> (string-downcase nickname))
    from: (hash-get options 'identities))
  (displayln "Removed identity " nickname))

(define-entry-point (list-identities . arguments)
  "List identities"
  (def options (process-options options/identities arguments))
  (def identities (load-identities from: (hash-get options 'identities)))
  (for-each
    (match <>
      ([nickname . keypair]
        (displayln nickname " [ " (string<- Address (keypair-address keypair)) " ]")))
    (hash->list identities)))

(define-entry-point (faucet . arguments)
  "Fund some accounts from the network faucet"
  (def options/faucet
    (make-options [] [] [options/identities options/to]))
  (def options (process-options options/faucet arguments))
  (def identities (load-identities from: (hash-get options 'identities)))
  (defrule ($ x) (hash-get options 'x))
  ;; TODO: find the faucet, use it.
  (def network (.@ (ethereum-config) network))
  (def faucets (.@ (ethereum-config) faucets))
  (cond
   ((equal? (.@ (ethereum-config) name) "Private Ethereum Testnet")
    (let ()
      (unless ($ to) (error "Missing recipient. Please use option --to"))
      (def to
        (if-let (address (keypair-address (hash-get identities ($ to))))
          address
          (parse-address ($ to))))
       ;; *after* the above, so we have croesus, but the user may have their own alice.
      (import-testing-module)
      (def value-in-ether 5)
      (def value (wei<-ether value-in-ether))
      (def token-symbol (.@ (ethereum-config) nativeCurrency symbol))
      (def from (address<-nickname "t/croesus"))
      (printf "\nSending ~a ~a from faucet ~a\n to ~a on network ~a:\n\n"
              value-in-ether token-symbol (0x<-address from) (0x<-address to) network)
      (cli-send-tx {from to value} confirmations: 0)
      (printf "\nFinal balance: ~a ~a\n\n" (decimal-string-ether<-wei (eth_getBalance to)) token-symbol)))
   ((not (null? faucets))
    (printf "\nVisit the following URL to get ethers on network ~a:\n\n\t~a\n\n"
            (car faucets) network))
   (else
    (printf "\nThere is no faucet for network ~a - Go earn tokens the hard way.\n\n" network))))

(export #t)

(import
  :std/getopt :std/misc/hash :std/srfi/13
  :clan/base :clan/config :clan/files :clan/json :clan/multicall :clan/syntax :clan/crypto/secp256k1
  :clan/poo/brace :clan/poo/cli :clan/poo/io :clan/poo/mop :clan/poo/object :clan/poo/type
  :mukn/ethereum/cli :mukn/ethereum/hex :mukn/ethereum/ethereum :mukn/ethereum/known-addresses)

(def (load-identities identities-file)
  (def identities-json (with-catch (lambda (_) (make-hash-table)) (cut read-file-json identities-file)))
  (hash-key-value-map
    (lambda (nickname keypair-json)
      (cons nickname (import-keypair/json keypair-json)))
    identities-json))

(def (store-identities identities-file identities)
  (def identities-json
    (hash-key-value-map
      (lambda (nickname keypair)
        (cons nickname (export-keypair/json keypair)))
      identities))
  (clobber-file identities-file (string<-json identities-json) salt?: #t))

(def (with-identities identities-file f)
  (def identities
    (if (file-exists? identities-file)
      (load-identities identities-file)
      (make-hash-table)))
  (f identities)
  (store-identities identities-file identities))

(def options/identities
  (make-options
    [(option 'identities "-I" "--identities" default: (xdg-config-home "glow" "secret-key-ring.json")
             help: "file to load and store identities")] []))

(def options/identity-metadata
  (make-options
    [(option 'type "-T" "--type" default: 'kecakk-256
               help: "type of identity address")
     (option 'blockchain "-B" "--blockchain" default: 'ethereum
               help: "blockchain of the address")] []))

(define-entry-point (add-identity . arguments)
  "Add identity"
  (def options/add
    (make-options
      [(option 'nickname "-N" "--nickname"
               help: "nickname of identity")
       (option 'address "-A" "--address"
               help: "address of identity")
       (option 'public-key "-P" "--public-key"
               help: "public key of identity")
       (option 'secret-key "-S" "--secret-key"
               help: "secret key of identity")]
      []
      [options/test options/identities options/identity-metadata]))
  (def options (process-options options/add arguments))
  (def nickname (hash-get options 'nickname))
  (def new-keypair
    (keypair (<-string Address (hash-get options 'address))
             (<-string PublicKey (hash-get options 'public-key))
             (import-secret-key/bytes (<-string Bytes32 (hash-get options 'secret-key)))))
  (with-identities (hash-get options 'identities)
    (cut hash-put! <> (string-downcase nickname) new-keypair))
  (displayln "Added identity: " nickname " [ " (string<- Address (keypair-address new-keypair)) " ]"))

(define-entry-point (generate-identity . arguments)
  "Generate identity"
  (def options/generate
    (make-options
      [(option 'nickname "-N" "--nickname"
               help: "nickname of identity")
       (option 'prefix "-P" "--prefix" default: #f
               help: "hex prefix of generated generated address")]
      []
      [options/test options/identities options/identity-metadata]))
  (def options (process-options options/generate arguments))
  (def nickname (hash-get options 'nickname))
  (def scoring
    (if-let (prefix (hash-get options 'prefix))
      (scoring<-prefix prefix)
      (trivial-scoring)))
  (def keypair (generate-keypair scoring: scoring))
  (with-identities (hash-get options 'identities)
    (cut hash-put! <> (string-downcase nickname) keypair))
  (displayln "Generated identity: " nickname " [ " (string<- Address (keypair-address keypair)) " ]"))

(define-entry-point (remove-identity . arguments)
  "Remove identity"
  (def options/remove
    (make-options
      [(option 'nickname "-N" "--nickname")] [] [options/identities]))
  (def options (process-options options/remove arguments))
  (def identities-file (hash-get options 'identities))
  (def nickname (hash-get options 'nickname))
  (with-identities identities-file
    (cut hash-remove! <> (string-downcase nickname)))
  (displayln "Removed identity " nickname))

(define-entry-point (list-identities . arguments)
  "List identities"
  (def options (process-options options/identities arguments))
  (def identities-file (hash-get options 'identities))
  (def identities (load-identities identities-file))
  (for-each
    (match <>
      ([nickname . keypair]
        (displayln nickname " [ " (string<- Address (keypair-address keypair)) " ]")))
    (hash->list identities)))

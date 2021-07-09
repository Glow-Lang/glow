(export #t)

(import
  :std/getopt :std/format :std/iter :std/misc/hash :std/srfi/13 :std/sugar
  :clan/base :clan/config :clan/files :clan/hash :clan/json :clan/multicall :clan/path :clan/syntax
  :clan/crypto/secp256k1
  :clan/poo/brace :clan/poo/cli :clan/poo/io :clan/poo/mop :clan/poo/object :clan/poo/type
  :mukn/ethereum/cli :mukn/ethereum/hex :mukn/ethereum/ethereum :mukn/ethereum/known-addresses :mukn/ethereum/network-config
  :mukn/ethereum/json-rpc
  (rename-in :mukn/glow-contacts/contacts (add-identity add-identity-db)))

;; TODO:
;; - always populate contacts as well as identity and/or check consistency between nicknames of the two
;; - store only the secret-key, not address and public-key
;; - store they key type (ethereum, bitcoin, some HD wallet, etc.) -- BIP32 path?
;; - store some version schema identifier in the file or its name, and
;;   automatically (or at least manually) migrate from one version to the other.

(def (default-secret-key-ring)
  (xdg-config-home "glow" "secret-key-ring.json"))

(define-type Identity
  (.+
   (Record
    nickname: [String]
    network: [Symbol]
    address: [Address]
    public-key: [String]
    keypair: [Keypair])
   {.make: (lambda (nickname (network 'ethereum) (address #f) (public-key #f) (keypair #f))
             { nickname network address public-key keypair })
    .json<-: (lambda (identity)
               (with-slots (nickname network address public-key keypair) identity
                 (hash (nickname nickname)
                       (network network)
                       (address (0x<-address address))
                       (public_key public-key)
                       (secret_key_path (if (and keypair
                                                 (keypair-consistent? keypair)
                                                 (string= (0x<-address address) (0x<-address (keypair-address keypair)))
                                                 (string= public-key (string<- PublicKey (keypair-public-key keypair))))
                                            (format "glow:~a" nickname)
                                            (void))))))}))

(def (load-identities from: (from #f))
  (unless (string? from) (set! from (default-secret-key-ring)))
  (with-catch
   (lambda (e)
     (displayln (error-message e))
     (error "Failed to read and parse the secret key ring file" from))
   (lambda ()
     (when (file-exists? from)
       (hash-for-each
        (lambda (nickname keypair-json)
          (def keypair (import-keypair/json keypair-json))
          (register-keypair nickname keypair))
        (read-file-json from))))))

(def options/identities
  (make-options
    [(option 'identities "-I" "--identities" ;;default: #f
             help: "file to load and store identities")
     (flag 'json "-J" "--json"
           help: "write identities as JSON")]
    []))

(define-entry-point (add-identity
                     identities: (identities #f)
                     json: (json #f)
                     nickname: (nickname #f)
                     secret-key: (secret-key #f))
  (help: "Add identity"
   getopt: (make-options
            ;; TODO: Consolidate with contact options
            [(option 'nickname "-N" "--nickname"
                     help: "nickname of identity")
             (option 'secret-key "-S" "--secret-key"
                     help: "secret key of identity")]
            []
            [options/test options/identities]))
  (unless secret-key (error "missing secret-key"))
  (unless nickname (error "missing nickname"))
  (def new-keypair (keypair<-secret-key (<-string Bytes32 secret-key)))
  ;; (call-with-identities
  ;;  from: identities
  ;;  (cut hash-put! <> (string-downcase nickname) new-keypair))
  (if json
      (display (string<-json (.call Identity .json<- (.call Identity .make nickname new-keypair)))) ; WRONG
      (printf "Added identity: ~a [~a]~%"
              nickname
              (string<- Address (keypair-address new-keypair)))))

(define-entry-point (generate-identity
                     identities: (identities #f)
                     json: (json #f)
                     nickname: (nickname #f)
                     prefix: (prefix #f))
  (help: "Generate identity"
   getopt: (make-options
            [(option 'nickname "-N" "--nickname"
                     help: "nickname of identity")
             (option 'prefix "-P" "--prefix" default: #f
                     help: "desired hex prefix of generated address")]
            []
            [options/test options/identities]))
  (unless nickname (error "missing nickname option"))
  (def scoring (if prefix (scoring<-prefix prefix) trivial-scoring))
  (def keypair (generate-keypair scoring: scoring))
  ;; (call-with-identities
  ;;  from: identities
  ;;  (cut hash-put! <> (string-downcase nickname) keypair))
  (if json
      (display (string<-json (.call Identity .json<- (.call Identity .make nickname keypair)))) ; WRONG
      (printf "Generated identity: ~a [~a]~%"
              nickname
              (string<- Address (keypair-address keypair)))))

(define-entry-point (remove-identity
                     json: (json #f)
                     nickname: (nickname #f))
  (help: "Remove identity"
   getopt: (make-options
            [(option 'nickname "-N" "--nickname")] [] [options/identities]))
  (unless nickname (error "missing nickname option"))
  (delete-identity-by-nickname nickname)
  (if json
      (display (string<-json (hash (removed nickname))))
      (printf "Removed identity ~a~%" nickname)))

(export #t)

(import
  :std/getopt :std/format :std/iter :std/misc/hash :std/srfi/13 :std/sugar
  :clan/base :clan/config :clan/files :clan/hash :clan/json :clan/multicall :clan/path :clan/syntax
  :clan/crypto/secp256k1
  :clan/poo/brace :clan/poo/cli :clan/poo/io :clan/poo/mop :clan/poo/object :clan/poo/type
  :mukn/ethereum/cli :mukn/ethereum/hex :mukn/ethereum/ethereum :mukn/ethereum/known-addresses :mukn/ethereum/network-config
  :mukn/ethereum/json-rpc
  (rename-in ../contacts/db (add-contact add-contact.db) (add-identity add-identity.db)))

;; TODO:
;; - store only the secret-key, not address and public-key
;; - store they key type (ethereum, bitcoin, some HD wallet, etc.) -- BIP32 path?
;; - store some version schema identifier in the file or its name, and
;;   automatically (or at least manually) migrate from one version to the other.

(define-type Identity
  (.+
   (Record
    nickname: [String]
    network: [Symbol]
    address: [Address]
    public-key: [String]
    keypair: [Keypair])
   {.make: (lambda (nickname (network 'eth) (address #f) (public-key #f) (keypair #f))
             (when keypair
               (unless (keypair-consistent? keypair)
                 (error "Inconsistent keypair for" nickname))
               (if address
                   (unless (equal? address (keypair-address keypair))
                     (error "Inconsistent address and keypair for" nickname))
                   (set! address (keypair-address keypair)))
               (if public-key
                   (unless (equal? (bytes<- PublicKey public-key)
                                   (bytes<- PublicKey (keypair-public-key keypair)))
                     (error "Inconsistent public key and keypair for" nickname))
                   (set! public-key (keypair-public-key keypair))))
             { nickname network address public-key keypair })
    .<-json: (lambda (identity)
               (let* ((nickname (hash-ref identity 'nickname))
                      (network (make-symbol (hash-ref identity 'network)))
                      (address (address<-0x (hash-ref identity 'address)))
                      (public-key (let ((public_key (hash-get identity 'public_key)))
                                    (and (string? public_key)
                                         (<-string PublicKey public_key))))
                      (keypair (hash-get keypair-by-address address)))
                 (.call Identity .make nickname network address public-key keypair)))
    .json<-: (lambda (identity)
               (with-slots (nickname network address public-key keypair) identity
                 (hash (nickname nickname)
                       (network network)
                       (address (0x<-address address))
                       (public_key (if public-key
                                       (string<- PublicKey public-key)
                                       (void)))
                       (secret_key_path (if keypair
                                            (format "glow:~a" nickname)
                                            (void))))))}))

(def (default-secret-key-ring)
  (xdg-config-home "glow" "secret-key-ring.json"))

(def (load-keypairs from: (from #f))
  (unless (string? from) (set! from (default-secret-key-ring)))
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
            (cons nickname keypair))
          (read-file-json from))
         (make-hash-table)))))

(def (store-keypairs keypairs to: (to #f))
  (unless (string? to) (set! to (default-secret-key-ring)))
  (def keypairs-json
    (hash-key-value-map
     (lambda (nickname keypair)
       (cons nickname (export-keypair/json keypair)))
     keypairs))
  (create-directory* (path-parent to))
  ;; TODO: Ensure the new file has mode 0600.
  (clobber-file to (string<-json keypairs-json) salt?: #t))

(def (call-with-keypairs f from: (from #f) to: (to #f))
  (def keypairs (load-keypairs from: from))
  (f keypairs)
  (store-keypairs keypairs to: (or to from)))

(def options/keypairs
  (make-options
   [(option 'keypairs "-K" "--keypairs" help: "file to load and store keypairs")]))

(def options/identities
  (make-options
    [(option 'cid "-C" "--cid" help: "contact ID of identity, #f for a new contact")
     (option 'network "-E" "--evm-network" help: "name of EVM network")
     (flag 'json "-J" "--json" help: "write identities as JSON")
     (option 'nickname "-N" "--nickname" help: "nickname of identity")]
    []
    [options/keypairs]))

(define-entry-point (add-identity
                     cid: (cid #f)
                     json: (json #f)
                     keypair: (keypair #f)
                     keypairs: (keypairs #f)
                     network: (network #f)
                     nickname: (nickname #f)
                     secret-key: (secret-key #f))
  (help: "Add an identity from a secret key to a (new, anonymous) contact."
   getopt: (make-options
            [(option 'secret-key "-S" "--secret-key"
                     help: "secret key of identity")]
            []
            [options/identities]))
  (unless cid (set! cid (add-contact.db (void) [])))
  (unless network (error "missing EVM network name"))
  (unless nickname (error "missing nickname"))
  (unless (or keypair secret-key) (error "missing secret-key"))
  (def new-keypair (or keypair (keypair<-secret-key (<-string Bytes32 secret-key))))
  (def new-address (keypair-address new-keypair))
  (def new-public-key (keypair-public-key new-keypair))
  (def new-identity
    (.call Identity .make nickname network new-address new-public-key new-keypair))
  (call-with-keypairs
   (cut hash-put! <> (string-downcase nickname) new-keypair)
   from: keypairs)
  (add-identity.db cid (json<- Identity new-identity))
  (if json
      (displayln (string<-json (json<- Identity new-identity)))
      (printf "Added identity: ~a [~a]~%" nickname (0x<-address new-address))))

(define-entry-point (generate-identity
                     cid: (cid #f)
                     json: (json #f)
                     keypairs: (keypairs #f)
                     network: (network #f)
                     nickname: (nickname #f)
                     prefix: (prefix #f))
  (help: "Generate a new identity for a (new, anonymous) contact."
   getopt: (make-options
            [(option 'prefix "-P" "--prefix" default: #f
                     help: "desired hex prefix of generated address")]
            []
            [options/identities]))
  (def scoring (if prefix (scoring<-prefix prefix) trivial-scoring))
  (def keypair (generate-keypair scoring: scoring))
  (add-identity cid: cid
                json: json
                keypairs: keypairs
                network: network
                nickname: nickname
                keypair: keypair))

(define-entry-point (remove-identity
                     json: (json #f)
                     keypairs: (keypairs #f)
                     nickname: (nickname #f))
  (help: "Remove identity"
   getopt: (make-options
            [(flag 'json "-J" "--json"
                   help: "write ouptut as JSON")
             (option 'nickname "-N" "--nickname"
                     help: "nickname of identity to remove")]
            []
            [options/keypairs]))
  (unless nickname (error "missing nickname"))
  (call-with-keypairs
   (cut hash-remove! <> (string-downcase nickname))
   from: keypairs)
  (delete-identity-by-nickname nickname)
  (if json
      (displayln (string<-json (hash (removed nickname))))
      (printf "Removed identity ~a~%" nickname)))

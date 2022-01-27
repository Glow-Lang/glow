(export #t)

(import
  :std/crypto :std/getopt :std/format :std/iter :std/misc/hash :std/srfi/13 :std/sugar :std/text/hex
  :clan/base :clan/files :clan/hash :clan/json :clan/multicall :clan/path :clan/syntax
  :clan/crypto/random :clan/crypto/secp256k1
  :clan/poo/brace :clan/poo/cli :clan/poo/io :clan/poo/mop :clan/poo/object :clan/poo/type
  :mukn/ethereum/cli :mukn/ethereum/hex :mukn/ethereum/ethereum :mukn/ethereum/known-addresses :mukn/ethereum/network-config
  :mukn/ethereum/json-rpc
  (rename-in ../contacts/db (add-contact add-contact.db) (add-identity add-identity.db))
  (only-in ../contacts/keys decrypt-secret-key encrypt-secret-key secret-key-cipher))

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
   ;; NOTE: This also derives and registers the Identity instance's address
   ;; as a key-value mapping, from the Identity's nickname to the address.
   ;; TODO: Edit this to also generate peerId.
   {.make: (lambda (nickname (network 'eth) (address #f) (public-key #f) (keypair #f))
             (when (and keypair (keypair-public-key keypair))
               (if (keypair-consistent? keypair)
                   (register-keypair nickname keypair)
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
                      (secret-key (let ((secret_key (hash-get identity 'secret_key))
                                        (secret_key_cipher (hash-get identity 'secret_key_cipher))
                                        (secret_key_iv (hash-get identity 'secret_key_iv)))
                                    (and (string? secret_key)
                                         (string? secret_key_cipher)
                                         (string? secret_key_iv)
                                         (secp256k1-seckey
                                          (decrypt-secret-key secret_key_cipher
                                                              (hex-decode secret_key_iv)
                                                              (hex-decode secret_key))))))
                      (kp (keypair address public-key secret-key)))
                 (.call Identity .make nickname network address public-key kp)))
    .json<-: (lambda (identity)
               (with-slots (nickname network address public-key keypair) identity
                 (let-values (((encrypted-secret-key iv)
                               (encrypt-secret-key (export-secret-key/bytes
                                                    (keypair-secret-key keypair)))))
                   (hash (nickname nickname)
                         (network network)
                         (address (0x<-address address))
                         (public_key (if public-key
                                         (string<- PublicKey public-key)
                                         (void)))
                         (secret_key (if encrypted-secret-key
                                         (hex-encode encrypted-secret-key)
                                         (void)))
                         (secret_key_cipher (if encrypted-secret-key
                                                (cipher-name secret-key-cipher)
                                                (void)))
                         (secret_key_iv (if iv
                                            (hex-encode iv)
                                            (void)))))))}))

(def options/identities
  (make-options
    [(option 'cid "-C" "--cid" help: "contact ID of identity, #f for a new contact")
     (option 'network "-E" "--evm-network" help: "name of EVM network")
     (flag 'json "-J" "--json" help: "write identities as JSON")
     (option 'nickname "-N" "--nickname" help: "nickname of identity")]))

(define-entry-point (add-identity
                     cid: (cid #f)
                     json: (json #f)
                     keypair: (keypair #f)
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
  (register-keypair nickname new-keypair)
  (add-identity.db cid (json<- Identity new-identity))
  (if json
      (displayln (string<-json (json<- Identity new-identity)))
      (printf "Added identity: ~a [~a]~%" nickname (0x<-address new-address))))

(define-entry-point (generate-identity
                     cid: (cid #f)
                     json: (json #f)
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
                network: network
                nickname: nickname
                keypair: keypair))

(define-entry-point (remove-identity
                     json: (json #f)
                     nickname: (nickname #f))
  (help: "Remove identity"
   getopt: (make-options
            [(flag 'json "-J" "--json"
                   help: "write ouptut as JSON")
             (option 'nickname "-N" "--nickname"
                     help: "nickname of identity to remove")]))
  (unless nickname (error "missing nickname"))
  (unregister-keypair nickname)
  (delete-identity-by-nickname nickname)
  (if json
      (displayln (string<-json (hash (removed nickname))))
      (printf "Removed identity ~a~%" nickname)))

(export #t)

(import
  :std/iter
  :clan/config :clan/json
  :mukn/ethereum/known-addresses)

;; TODO: Support having the secret key ring as an encrypted database
;; - Encrypt the entire private key database with gnupg and decrypt it with gnupg-agent?
;; - Or encrypt individual entries with gnupg instead?
;; - Use memory-hardening to make sure secret key information is overwritten in memory
;;   immediately after use and never copied around by the Garbage Collector while live
;;   (which should last as short as possible).
;;   This requires support in gerbil-crypto, gerbil, OpenSSL, etc.
;; - Support having (master) keys in hardware wallets, trusted platform module, or yubikey.
;; TODO: Support many types of keys.
;; TODO: Support BIP-32 path.
;; TODO: Move that to gerbil-ethereum ? Nah, secrets and HD wallets go across networks.
;; TODO: have a database, not just a json file, so we can write ephemeral keys?
;;   Or have a separate database for that?
(def (secret-key-ring)
  (xdg-config-home "glow" "secret-key-ring.json"))

(def (load-secret-key-ring)
  (def ring (secret-key-ring))
  (with-catch
   (lambda (_) (error "Failed to read and parse the secret key ring file" ring))
   (lambda ()
     (for (((values nickname j) (in-hash (read-file-json ring))))
       (register-keypair nickname (import-keypair/json j))))))

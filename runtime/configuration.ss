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
;; TODO: Support BIP-32 / BIP-39 / BIP-43 / BIP-44 HD wallet.
;;   https://en.bitcoin.it/wiki/BIP_0032
;;   https://en.bitcoin.it/wiki/BIP_0039
;;   https://en.bitcoin.it/wiki/BIP_0043
;;   https://en.bitcoin.it/wiki/BIP_0044
;;   https://github.com/satoshilabs/slips/blob/master/slip-0044.md
;;   https://wolovim.medium.com/ethereum-201-hd-wallets-11d0c93c87f7
;;   https://medium.com/mycrypto/the-journey-from-mnemonic-phrase-to-address-6c5e86e11e14
;; TODO: For Cardano, use
;;   https://github.com/cardano-foundation/CIPs/blob/master/CIP-0005/CIP-0005.md
;;   https://github.com/cardano-foundation/CIPs/blob/master/CIP-1852/CIP-1852.md
;;   https://github.com/Emurgo/EmIPs/blob/master/specs/emip-003.md
;;   ED25519BIP32 https://raw.githubusercontent.com/input-output-hk/adrestia/master/user-guide/static/Ed25519_BIP.pdf
;; TODO: Move that to gerbil-ethereum ? Nah, secrets and HD wallets go across networks.
;; TODO: have a database, not just a json file, so we can write ephemeral keys?
;;   Or have a separate database for that?
;; TODO: On iOS and Android, use secure storage. On Browser, use IndexedDB.
(def (secret-key-ring)
  (xdg-config-home "glow" "secret-key-ring.json"))

(def (load-secret-key-ring)
  (def ring (secret-key-ring))
  (with-catch
   (lambda (_) (error "Failed to read and parse the secret key ring file" ring))
   (lambda ()
     (for (((values nickname j) (in-hash (read-file-json ring))))
       (register-keypair nickname (import-keypair/json j))))))

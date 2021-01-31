(export #t)

(import
  :std/iter
  :clan/config :clan/json
  :mukn/ethereum/known-addresses)

(def (secret-key-ring)
  (xdg-config-home "glow" "secret-key-ring.json"))

(def (load-secret-key-ring)
  (def ring (secret-key-ring))
  (with-catch
   (lambda (_) (error "Failed to read and parse the secret key ring file" ring))
   (lambda ()
     (for (((values nickname j) (in-hash (read-file-json ring))))
       (register-keypair nickname (import-keypair/json j))))))

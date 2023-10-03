(export #t)

(import
  :clan/debug
  :clan/persist/db
  :clan/ethereum/json-rpc
  :clan/ethereum/testing
  ../runtime/participant-runtime)

(def (setup-test-env)
  (delete-agreement-handshake)
  (ensure-ethereum-connection "pet")
  (ensure-db-connection "testdb")
  (register-test-keys)
  (DBG "Ensure participants funded")
  (ensure-addresses-prefunded)
  (DBG "DONE"))

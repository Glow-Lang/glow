(export #t)

(import
  :std/format :std/test
  :clan/utils/base :clan/utils/concurrency :clan/utils/path-config
  :clan/runtime/db
  ../types ../config ../signing ../known-addresses ../json-rpc ../transaction
  ./signing-test)

;; Use the Private Ethereum Testnet
(ensure-ethereum-config "pet")

;; Poll for ethereum server
(retry retry-window: 0.05 max-window: 1.0 max-retries: 10
       (cut eth_blockNumber timeout: 1.0))

;; Use the test database
(ensure-db-connection (run-path "testdb"))

;; Create a key for the initial have-it-all user of the test network
(def croesus (get-first-account))

(register-keypair
 "Croesus"
 (keypair ;; KLUGE: Fake public and secret key data. We only use the address via Geth.
  croesus
  (<-json PublicKey "0x020000000000000000000000000000000000000000000000000000000000000001")
  (<-json SecretKey "0x0000000000000000000000000000000000000000000000000000000000000001")
  ""))

;; Ensure Geth can issue transactions for all test accounts
(for-each (lambda (nkp) (ensure-secret-key (cadr nkp) log: printf)) test-keypairs)

(def transaction-integrationtest
  (test-suite "integration test for glow/ethereum/transaction"
    (test-case "Nothing left to do"
      (void))))

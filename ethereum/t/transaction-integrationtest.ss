(export #t)

(import
  :std/format :std/test
  :clan/utils/base :clan/utils/concurrency
  ../config ../json-rpc ../transaction
  ./signing-test)

(def (poll-for-testnet)
  (retry retry-window: 0.05 max-window: 1.0 max-retries: 10
         (cut eth_blockNumber timeout: 1.0))
  (void))

(def transaction-integrationtest
  (test-suite "integration test for glow/ethereum/transaction"
    (ensure-ethereum-config)
    (test-case "poll-for-testnet"
      (poll-for-testnet))
    (test-case "ensure passwords"
      (for-each (lambda (nkp) (ensure-secret-key (cadr nkp) log: printf)) test-keypairs))))

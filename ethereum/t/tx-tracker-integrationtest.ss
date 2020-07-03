(export #t)

(import
  :std/test
  ../ethereum ../json-rpc ../transaction ../tx-tracker
  ./signing-test ./transaction-integrationtest)

(def tx-tracker-integrationtest
  (test-suite "integration test for glow/ethereum/tx-tracker"
    (test-case "Simple transfer"
      (def before (eth_getBalance alice-address 'latest))
      (def value (* 1/100 one-ether-in-wei))
      (def _receipt (post-transaction (transfer-tokens from: croesus to: alice-address value)))
      (def after (eth_getBalance alice-address 'latest))
      (check-equal? (- after before) value))))

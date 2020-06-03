(def json-rpc-integrationtest
  (test-suite "integration test for glow/ethereum/json-rpc"
    (test-case "eth-latest-block get the current latest block"
      ;; Just checks that the block number is non-negative
      (check (cut <= 0 <>) (eth-block-number timeout: 1.0 log: false)))

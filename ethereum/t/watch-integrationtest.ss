(export #t)

(import
  :std/srfi/1
  ../watch ../json-rpc)

(def watch-integrationtest
  (test-suite "integration test for glow/ethereum/watch"
  ))

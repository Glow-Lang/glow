(export #t)

(import
  :std/test :mukn/ethereum/assembly
  ../ethereum-runtime
  ../ethereum-contract
  ../program)

(def test-contract (make-Contract
  participants: (make-hash-table)
  arguments: (make-hash-table)))

(def test-runtime (make-Runtime
  role: 'Test
  contract: test-contract
  current-code-block-label: 'begin
  current-label: 'begin0))

(def (switch-participant-statement value)
  ['switch value
    [0 ['def 'result 0]]
    [1 ['def 'result 1]]
    [2 ['def 'result 2]]])

(def (switch-consensus-statement value)
  (assemble
    (&switch value
      [[0 [0]]
       [1 [1]]
       [2 [2]]])))

(def runtime-interpreter-test
  (test-suite "Test runtime interpreter"

    (test-case "interpret-participant-switch-statement"
      (def expected-result 0)
      {interpret-participant-statement test-runtime (switch-participant-statement expected-result)}
      (check-equal? expected-result (hash-get (@ test-runtime environment) 'result)))

    (test-case "interpret-consensus-switch-statement"
      (def expected-result 1)
      (displayln (switch-consensus-statement expected-result))
      (check-equal? #t #t))))

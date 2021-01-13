(export #t)

(import
  :std/test :mukn/ethereum/assembly
  :std/format :std/sugar
  :clan/poo/poo :clan/persist/content-addressing :clan/versioning
  ../ethereum-runtime
  ../ethereum-contract
  ../program)

(def test-contract (make-Contract
  participants: (make-hash-table)
  arguments: (make-hash-table)))

(def test-agreement
  (.o
    glow-version: (software-identifier)
    interaction: (format "mukn/glow/examples/test#~a" (@ test-contract program name))
    participants: (.o)
    parameters: (hash)
    reference: (.o)
    options: (.o blockchain: "Private Ethereum Testnet" ;; the `name` field of an entry in `config/ethereum_networks.json`
                 escrowAmount: (void) ;; not meaningful for buy_sig in particular
                 timeoutInBlocks: (@ test-contract timeout)
                 maxInitialBlock: (+ (eth_blockNumber) timeout))
    code-digest: (digest<-string (@ test-contract program name))))

(def test-runtime (make-Runtime
  role: 'Test
  agreement: test-agreement
  contract: test-contract
  current-code-block-label: 'begin
  current-label: 'begin0))

(def (switch-participant-statement value)
  ['switch value
    [0 ['def 'result 0]]
    [1 ['def 'result 1]]
    [2 ['def 'result 2]]])

(def runtime-interpreter-test
  (test-suite "Test runtime interpreter"

    (test-case "interpret-participant-switch-statement"
      (def expected-result 0)
      {interpret-participant-statement test-runtime (switch-participant-statement expected-result)}
      (check-equal? expected-result (hash-get (@ test-runtime environment) 'result)))))

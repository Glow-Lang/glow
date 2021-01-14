(export #t)

(import
  :gerbil/gambit/os :gerbil/gambit/ports :gerbil/gambit/threads
  :std/format :std/srfi/1 :std/test :std/sugar :std/iter :std/text/json :std/misc/ports
  :clan/base :clan/concurrency :clan/debug :clan/decimal :clan/exception
  :clan/io :clan/json :clan/path-config :clan/ports
  :clan/poo/poo :clan/poo/io :clan/poo/debug
  :clan/crypto/keccak
  :clan/persist/content-addressing
  :clan/versioning
  :mukn/ethereum/types :mukn/ethereum/ethereum :mukn/ethereum/known-addresses :mukn/ethereum/json-rpc
  :mukn/ethereum/batch-send :mukn/ethereum/network-config :mukn/ethereum/assets
  :mukn/ethereum/signing :mukn/ethereum/hex :mukn/ethereum/transaction :mukn/ethereum/types
  :mukn/ethereum/t/signing-test
  :mukn/ethereum/t/50-batch-send-integrationtest
  ../compiler/passes
  ../compiler/multipass
  ../compiler/syntax-context
  ../runtime/program
  ../runtime/ethereum-runtime)

(def a-address alice)
(def b-address bob)

;; Should `timeout` be the value of `(ethereum-timeout-in-blocks)`,
;; or should it be the `timeoutInBlocks` field of the entry in `config/ethereum_networks.json`?
(def timeout (ethereum-timeout-in-blocks))
(def initial-timer-start (+ (eth_blockNumber) timeout))

(def coin_flip.glow (source-path "examples/coinflip.glow"))

(def agreement
  (.o
    glow-version: (software-identifier)
    interaction: "mukn/glow/examples/coinflip#coinFlip"
    participants: (.o A: a-address B: b-address)
    parameters: (hash
                  (wagerAmount (json<- Ether one-gwei-in-wei))
                  (escrowAmount (json<- Ether one-gwei-in-wei)))
    reference: (.o )
    options: (.o blockchain: "Private Ethereum Testnet" ;; the `name` field of an entry in `config/ethereum_networks.json`
                 escrowAmount: (void) ;; not meaningful for buy_sig in particular
                 timeoutInBlocks: timeout ; should be the `timeoutInBlocks` field of the same entry in `config/ethereum_networks.json`
                 maxInitialBlock: initial-timer-start)
    code-digest: (digest<-file coin_flip.glow)))

(def compiler-output (run-passes coin_flip.glow pass: 'project show?: #f))

(def coin-flip-integrationtest
  (test-suite "integration test for ethereum/coin-flip"
    (DBG "Ensure participants funded")
    (ensure-addresses-prefunded)
    (DBG "DONE")
    (def program (parse-compiler-output compiler-output))

    (test-case "coin flip executes"
      (ignore-errors (delete-file (run-path "contract-handshake.json"))) ;; TODO: do it better

      (displayln "\nEXECUTING A THREAD ...")
      (def a-thread
        (spawn/name/logged "A"
         (lambda ()
           (def a-runtime
             (make-Runtime role: 'A
                           agreement: agreement
                           current-code-block-label: 'begin0 ;; TODO: grab the start label from the compilation output, instead of 'begin0
                           current-label: 'begin ;; TODO: grab the start label from the compilation output, instead of 'begins
                           program: program))
           {execute a-runtime}
           (displayln "a finished")
           (@ a-runtime environment))))

      (displayln "\nEXECUTING B THREAD ...")
      (def b-thread
        (spawn/name/logged "B"
         (lambda ()
           (def b-runtime
             (make-Runtime role: 'B
                           agreement: agreement
                           current-code-block-label: 'begin0 ;; TODO: grab the start label from the compilation output, instead of 'begin0
                           current-label: 'begin ;; TODO: grab the start label from the compilation output, instead of 'begins
                           program: program))
           {execute b-runtime}
           (displayln "b finished"))))
      (def environment (thread-join! b-thread))
      (displayln (hash-keys environment))
      (check-equal? #t #t))))

(export #t)

(import
  :gerbil/gambit/bits :gerbil/gambit/os :gerbil/gambit/ports :gerbil/gambit/threads
  :std/format :std/srfi/1 :std/test :std/sugar :std/iter :std/text/json :std/misc/ports :std/misc/hash
  :clan/base :clan/concurrency :clan/debug :clan/decimal :clan/exception
  :clan/io :clan/json :clan/path-config :clan/ports
  :clan/poo/object :clan/poo/io :clan/poo/debug
  :clan/crypto/keccak
  :clan/persist/content-addressing :clan/persist/db
  :clan/versioning
  :mukn/ethereum/types :mukn/ethereum/ethereum :mukn/ethereum/known-addresses :mukn/ethereum/json-rpc
  :mukn/ethereum/simple-apps :mukn/ethereum/network-config :mukn/ethereum/assets
  :mukn/ethereum/signing :mukn/ethereum/hex :mukn/ethereum/transaction :mukn/ethereum/types
  :mukn/ethereum/testing
  ../compiler/passes
  ../compiler/multipass
  ../compiler/syntax-context
  ../runtime/program
  ../runtime/participant-runtime
  ../runtime/reify-contract-parameters)

(def a-address alice)
(def b-address bob)
(def wagerAmount (wei<-ether .5))
(def escrowAmount (wei<-ether .01))

(def coin_flip.glow (source-path "examples/coin_flip.glow"))

(def (make-agreement)
  ;; Should `timeout` be the value of `(ethereum-timeout-in-blocks)`,
  ;; or should it be the `timeoutInBlocks` field of the entry in `config/ethereum_networks.json`?
  (def timeout (ethereum-timeout-in-blocks))
  (def initial-timer-start (+ (eth_blockNumber) timeout))
  (.o
    interaction: "mukn/glow/examples/coin_flip#coinFlip"
    participants: (.o A: a-address B: b-address)
    parameters: (hash
                  (wagerAmount (json<- Ether wagerAmount))
                  (escrowAmount (json<- Ether escrowAmount)))
    reference: (.o)
    options: (.o blockchain: "Private Ethereum Testnet" ;; the `name` field of an entry in `config/ethereum_networks.json`
                 escrowAmount: (void) ;; manually done for the current coinflip
                 timeoutInBlocks: timeout ; should be the `timeoutInBlocks` field of the same entry in `config/ethereum_networks.json`
                 maxInitialBlock: initial-timer-start)
    glow-version: (software-identifier)
    code-digest: (digest<-file coin_flip.glow)))

(def coin-flip-integrationtest
  (test-suite "integration test for ethereum/coin-flip"
    (delete-agreement-handshake)
    (ensure-ethereum-connection "pet")
    (ensure-db-connection (run-path "testdb"))
    (DBG "Ensure participants funded")
    (ensure-addresses-prefunded)
    (DBG "DONE")
    (def compiler-output (run-passes coin_flip.glow pass: 'project show?: #f))
    (def program (parse-compiler-output compiler-output))

    (test-case "coin flip executes"
      (def a-balance-before (eth_getBalance a-address 'latest))
      (def b-balance-before (eth_getBalance b-address 'latest))

      (def agreement (make-agreement))

      (displayln "\nEXECUTING A THREAD ...")
      (def a-thread
        (spawn/name/logged "A"
         (lambda () (run:special-file 'A agreement))))

      (displayln "\nEXECUTING B THREAD ...")
      (def b-thread
        (spawn/name/logged "B"
         (lambda () (run:special-file 'B agreement))))

      (def a-environment (thread-join! a-thread))
      (def b-environment (thread-join! b-thread))

      (def a-balance-after (eth_getBalance a-address 'latest))
      (def b-balance-after (eth_getBalance b-address 'latest))
      (def randA (cdr (hash-get a-environment 'randA)))
      (def randB (cdr (hash-get a-environment 'randB)))
      (def a-wins? (even? (bitwise-xor randA randB)))

      (DDT "DApp completed"
           UInt256 randA
           UInt256 randB
           Bool a-wins?
           (.@ Ether .string<-) a-balance-before
           (.@ Ether .string<-) b-balance-before
           (.@ Ether .string<-) a-balance-after
           (.@ Ether .string<-) b-balance-after)

      (def gas-allowance (wei<-ether .01))
      (def a-outcome (if a-wins? wagerAmount (- wagerAmount)))
      (def b-outcome (if a-wins? (- wagerAmount) wagerAmount))
      (assert! (<= 0 (- (+ a-balance-before a-outcome) a-balance-after) gas-allowance))
      (assert! (<= 0 (- (+ b-balance-before b-outcome) b-balance-after) gas-allowance))

      ;; TODO: check the financial outcome of the interaction, too.
      (check-equal?
        (hash->list/sort a-environment symbol<?)
        (hash->list/sort b-environment symbol<?)))))

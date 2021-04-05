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
  :mukn/ethereum/hex :mukn/ethereum/transaction :mukn/ethereum/types
  :mukn/ethereum/testing
  ../compiler/passes
  ../compiler/multipass
  ../compiler/syntax-context
  ../runtime/program
  ../runtime/participant-runtime
  ../runtime/reify-contract-parameters
  ./cli-integration)

(register-test-keys)
(def a-address alice)
(def b-address bob)
(def wagerAmount (wei<-ether .5))
(def escrowAmount (wei<-ether .01))

(def coin-flip-integrationtest
  (test-suite "integration test for ethereum/coin-flip"
    (delete-agreement-handshake)
    (ensure-ethereum-connection "pet")
    (ensure-db-connection "testdb")
    (DBG "Ensure participants funded")
    (ensure-addresses-prefunded)
    (DBG "DONE")

    (test-case "coin flip executes"
      (def a-balance-before (eth_getBalance a-address 'latest))
      (def b-balance-before (eth_getBalance b-address 'latest))

      (def proc-a
        (open-process
          [path: "./glow"
           arguments: ["start-interaction"
                       "--evm-network" "pet"
                       "--test"
                       "--handshake" "nc -l 3232"]]))

      (def peer-command
        (with-io-port proc-a
          (lambda ()
            (answer-questions
              [["Choose application:"
                "coin_flip"]
               ["Choose your identity:"
                (lambda (id) (string-prefix? "t/alice " id))]
               ["Choose your role:"
                "A"]
               ["Select address for B:"
                (lambda (id) (string-prefix? "t/bob " id))]])
            (supply-parameters
              [["wagerAmount" wagerAmount]
               ["escrowAmount" escrowAmount]])
            (set-initial-block)
            (read-peer-command))))

      (def proc-b
        (open-process
          [path: "/bin/sh"
           arguments:
            ["-c" (string-append
                    "./" peer-command
                      " --evm-network pet"
                      " --database /tmp/alt-glow-db"
                      " --test"
                      " --handshake 'nc localhost 3232'")]]))

      (def b-environment
        (with-io-port proc-b
          (lambda ()
            (answer-questions
              [["Choose your identity:"
                (lambda (id) (string-prefix? "t/bob " id))]
               ["Choose your role:"
                "B"]])
            (read-environment))))

      (def a-environment
        (with-io-port proc-a read-environment))

      (close-port proc-a)
      (close-port proc-b)

      (assert! (equal? a-environment b-environment))

      (def a-balance-after (eth_getBalance a-address 'latest))
      (def b-balance-after (eth_getBalance b-address 'latest))
      (def randA (hash-get a-environment 'randA))
      (def randB (hash-get a-environment 'randB))
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

(export #t)

(import
  :gerbil/gambit
  :std/assert :std/format :std/iter
  :std/misc/hash :std/misc/ports
  :std/srfi/1
  :std/sugar :std/test
  :std/text/json
  :clan/base :clan/concurrency :clan/debug :clan/exception
  :clan/io :clan/json :clan/path-config :clan/ports :clan/ffi
  :clan/poo/object :clan/poo/io :clan/poo/debug
  :clan/crypto/keccak
  :clan/persist/content-addressing :clan/persist/db
  :clan/versioning
  :clan/ethereum/types :clan/ethereum/ethereum :clan/ethereum/known-addresses :clan/ethereum/json-rpc
  :clan/ethereum/simple-apps :clan/ethereum/network-config :clan/ethereum/assets
  :clan/ethereum/hex :clan/ethereum/transaction :clan/ethereum/types
  :clan/ethereum/testing
  ../compiler/passes
  ../compiler/multipass
  ../compiler/syntax-context
  ../runtime/program
  ../runtime/participant-runtime
  ../runtime/reify-contract-parameters
  ./cli-integration
  ./utils)

(def coin-flip-integrationtest
  (test-suite "integration test for ethereum/coin-flip"
    (setup-test-env)

    (def a-address alice)
    (def b-address bob)
    (def wagerAmount (wei<-ether .5))
    (def escrowAmount (wei<-ether .01))


    (test-case "coin flip executes"
      (def a-balance-before (eth_getBalance a-address 'latest))
      (def b-balance-before (eth_getBalance b-address 'latest))

      (def proc-a #f)
      (def proc-b #f)
      (try
       (set! proc-a
         (open-process
          [path: "./glow"
           arguments: ["start-interaction"
                       "--glow-path" (source-path "dapps")
                       "--evm-network" "pet"
                       "--test"
                       "--tcp" "{\"listen\": 10337}"]]))

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
                (lambda (id) (string-prefix? "t/bob " id))]
               ["Select asset for DefaultToken:"
                (lambda (id) (string-prefix? "PET " id))]])
             (supply-parameters
              [["wagerAmount" wagerAmount]
               ["escrowAmount" escrowAmount]])
             (set-initial-block)
             (read-peer-command))))

       (set! proc-b
         (open-process
          [path: "/bin/sh"
           arguments:
            ["-c" (string-append
                    "./" peer-command
                      " --glow-path " (source-path "dapps")
                      " --evm-network pet"
                      " --database /tmp/alt-glow-db"
                      " --test"
                      " --tcp '{\"connect\": \"localhost:10337\"}'")]]))

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
        (hash->list/sort b-environment symbol<?))

       (finally
        (ignore-errors (close-port proc-a))
        (ignore-errors (close-port proc-b))
        (ignore-errors (kill (process-pid proc-a)))
        (ignore-errors (kill (process-pid proc-b))))))))

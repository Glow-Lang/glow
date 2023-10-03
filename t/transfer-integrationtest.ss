(export #t)

(import
  :gerbil/gambit
  :std/assert :std/format :std/iter
  :std/misc/hash :std/misc/ports
  :std/srfi/1
  :std/sugar :std/test
  :std/text/json
  :clan/base :clan/concurrency :clan/debug :clan/decimal :clan/exception
  :clan/io :clan/json :clan/path-config :clan/ports :clan/ffi
  :clan/poo/object :clan/poo/io :clan/poo/debug
  :clan/crypto/keccak
  :clan/persist/content-addressing :clan/persist/db
  :clan/versioning
  :clan/assert
  :clan/ethereum/types :clan/ethereum/ethereum :clan/ethereum/known-addresses :clan/ethereum/json-rpc
  :clan/ethereum/simple-apps :clan/ethereum/network-config :clan/ethereum/assets
  :clan/ethereum/hex :clan/ethereum/transaction :clan/ethereum/types
  :clan/ethereum/testing :clan/ethereum/test-contracts
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
(def gas-allowance (wei<-ether .01))
(def amount (wei<-ether .02))

(def transfer-integrationtest
  (test-suite "integration test for ethereum/transfer"
    (delete-agreement-handshake)
    (ensure-ethereum-connection "pet")
    (ensure-db-connection "testdb")
    (DBG "Ensure participants funded")
    (ensure-addresses-prefunded)
    (DBG "DONE")

    (test-case "transfer executes"
      (def a-pet-before (eth_getBalance a-address 'latest))
      (def b-pet-before (eth_getBalance b-address 'latest))

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
                "transfer"]
               ["Choose your identity:"
                (lambda (id) (string-prefix? "t/alice " id))]
               ["Choose your role:"
                "From"]
               ["Select address for To:"
                (lambda (id) (string-prefix? "t/bob " id))]
               ["Select asset for DefaultToken:"
                (lambda (id) (string-prefix? "PET " id))]])
             (supply-parameters
              [["amount" amount]])
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
                "To"]])
             (read-environment))))

       (def a-environment
         (with-io-port proc-a read-environment))
       (assert-equal! a-environment b-environment)

       (def a-pet-after (eth_getBalance a-address 'latest))
       (def b-pet-after (eth_getBalance b-address 'latest))

       ;; TODO: balances in both assets PET and QASPET
       (def PET (lookup-asset 'PET))
       (DDT "DApp completed"
            (.@ PET .string<-) a-pet-before
            (.@ PET .string<-) b-pet-before
            (.@ PET .string<-) a-pet-after
            (.@ PET .string<-) b-pet-after)

       ;; in PET, a loses t, b gains t
       (assert! (<= 0 (- (- a-pet-before amount) a-pet-after) gas-allowance))
       (assert! (<= 0 (- (+ b-pet-before amount) b-pet-after) gas-allowance))

       (finally
        (ignore-errors (close-port proc-a))
        (ignore-errors (close-port proc-b))
        (ignore-errors (kill (process-pid proc-a)))
        (ignore-errors (kill (process-pid proc-b))))))))

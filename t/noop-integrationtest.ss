(export #t)

(import
  :gerbil/gambit/bits :gerbil/gambit/os :gerbil/gambit/ports :gerbil/gambit/threads
  :std/format :std/srfi/1 :std/test :std/sugar :std/iter :std/text/json :std/misc/ports :std/misc/hash
  :clan/base :clan/concurrency :clan/debug :clan/decimal :clan/exception
  :clan/io :clan/json :clan/path-config :clan/ports :clan/ffi
  :clan/poo/object :clan/poo/io :clan/poo/debug
  :clan/crypto/keccak
  :clan/persist/content-addressing :clan/persist/db
  :clan/versioning
  :clan/assert
  :mukn/ethereum/types :mukn/ethereum/ethereum :mukn/ethereum/known-addresses :mukn/ethereum/json-rpc
  :mukn/ethereum/simple-apps :mukn/ethereum/network-config :mukn/ethereum/assets
  :mukn/ethereum/hex :mukn/ethereum/transaction :mukn/ethereum/types
  :mukn/ethereum/testing :mukn/ethereum/test-contracts
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

(def noop-integrationtest
  (test-suite "integration test for ethereum/noop"
    (delete-agreement-handshake)
    (ensure-ethereum-connection "pet")
    (ensure-db-connection "testdb")
    (DBG "Ensure participants funded")
    (ensure-addresses-prefunded)
    (DBG "DONE")

    (test-case "noop executes"
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
                       "--handshake" "nc -l 3232"]]))

       (def peer-command
         (with-io-port proc-a
           (lambda ()
             (answer-questions
              [["Choose application:"
                "noop"]
               ["Choose your identity:"
                (lambda (id) (string-prefix? "t/alice " id))]
               ["Choose your role:"
                "A"]
               ["Select address for B:"
                (lambda (id) (string-prefix? "t/bob " id))]
               ["Select asset for DefaultToken:"
                (lambda (id) (string-prefix? "PET " id))]])
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
       (assert-equal! a-environment b-environment)

       (def a-pet-after (eth_getBalance a-address 'latest))
       (def b-pet-after (eth_getBalance b-address 'latest))

       (DDT "DApp completed"
            (.@ PET .string<-) a-pet-before
            (.@ PET .string<-) b-pet-before
            (.@ PET .string<-) a-pet-after
            (.@ PET .string<-) b-pet-after)

       (assert! (<= 0 (- a-pet-before a-pet-after) gas-allowance))
       (assert! (<= 0 (- b-pet-before b-pet-after) gas-allowance))

       (finally
        (ignore-errors (close-port proc-a))
        (ignore-errors (close-port proc-b))
        (ignore-errors (kill (process-pid proc-a)))
        (ignore-errors (kill (process-pid proc-b))))))))

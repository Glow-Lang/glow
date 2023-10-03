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

(def rps-simple-integrationtest
  (test-suite "integration test for ethereum/rps_simple"
    (setup-test-env)

    (def a-address alice)
    (def b-address bob)
    (def wagerAmount (wei<-ether .01))

    (test-case "rps_simple executes"
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
                "rps_simple"]
               ["Choose your identity:"
                (lambda (id) (string-prefix? "t/alice " id))]
               ["Choose your role:"
                "A"]
               ["Select address for B:"
                (lambda (id) (string-prefix? "t/bob " id))]
               ["Select asset for DefaultToken:"
                (lambda (id) (string-prefix? "PET " id))]])
             (supply-parameters
              [["wagerAmount" wagerAmount]])
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

       (with-io-port proc-b
         (lambda ()
           (answer-questions
            [["Choose your identity:"
              (lambda (id) (string-prefix? "t/bob " id))]
             ["Choose your role:"
              "B"]])))

       (with-io-port proc-a
         (lambda ()
           (displayln "2") ; Scissors
           (force-output)))

       (def b-environment
         (with-io-port proc-b
           (lambda ()
             (displayln "1") ; Paper
             (force-output)
             (read-environment))))

       (def a-environment
         (with-io-port proc-a read-environment))
       (assert! (equal? a-environment b-environment))

       ;; if A chose scissors, B chose paper, then outcome 2 (A_Wins)
       (def outcome (hash-get a-environment 'outcome))
       (display-object
        ["outcome extracted from contract logs, 0 (B_Wins), 1 (Draw), 2 (A_Wins):" UInt256 outcome])
       (check-equal? outcome 2)

       (finally
        (ignore-errors (close-port proc-a))
        (ignore-errors (close-port proc-b))
        (ignore-errors (kill (process-pid proc-a)))
        (ignore-errors (kill (process-pid proc-b))))))))

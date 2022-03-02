(export #t)

(import
  :gerbil/gambit/bits :gerbil/gambit/os :gerbil/gambit/ports :gerbil/gambit/threads
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
  ./cli-integration
  ./utils)

(def escrow-timeout-integrationtest
  (test-suite "integration test checking escrow return on timeout"
    (setup-test-env)

    (def wagerAmount (wei<-ether 1))

    (def balance-before (eth_getBalance alice 'latest))

    (test-case "rps_simple returns escrow on timeout"
      (def proc #f)
      (try
       (set! proc
        (open-process
          [path: "./glow"
           arguments: ["start-interaction"
                       "--timeout-in-blocks" "5"
                       "--glow-path" (source-path "dapps")
                       "--evm-network" "pet"
                       "--test"]]))

       (with-io-port proc
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
           (displayln "2") ; Scissors
           (force-output)
           (read-environment)))

       (finally
        (ignore-errors
          (begin
            (close-port proc)
            (kill (process-pid proc))
            (process-status proc)))))

       (def balance-after (eth_getBalance alice 'latest))

       ;; Allow some amount of skew for gas, but otherwise the balance should be the same.
       (def gas-allowance (wei<-ether .01))
       (assert! (<= 0 (- balance-before balance-after) gas-allowance)))))

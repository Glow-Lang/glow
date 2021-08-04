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
(def gas-allowance (wei<-ether .01))
(def t (wei<-ether .01))
(def u (wei<-ether .01))

(def asset-swap-integrationtest
  (test-suite "integration test for ethereum/asset_swap"
    (delete-agreement-handshake)
    (ensure-ethereum-connection "pet")
    (ensure-db-connection "testdb")
    (DBG "Ensure participants funded")
    ;; TODO: prefund on the ERC20s too
    (ensure-addresses-prefunded)
    (DBG "DONE")

    (test-case "asset_swap executes"
      ;; TODO: get balances on QASPET too
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
                       "--handshake" "nc -l 3232"]]))

       (def peer-command
         (with-io-port proc-a
           (lambda ()
             (answer-questions
              [["Choose application:"
                "asset_swap"]
               ["Choose your identity:"
                (lambda (id) (string-prefix? "t/alice " id))]
               ["Choose your role:"
                "A"]
               ["Select address for B:"
                (lambda (id) (string-prefix? "t/bob " id))]
               ["Select asset for T:"
                (lambda (id) (string-prefix? "PET " id))]
               ["Select asset for U:"
                (lambda (id) (string-prefix? "QASPET " id))]])
             (supply-parameters
              [["t" t]
               ["u" u]])
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
                "B"]]))))

       (def a-environment
         (with-io-port proc-a read-environment))
       (assert! (equal? a-environment b-environment))

       ;; TODO: get balances on QASPET too
       (def a-balance-after (eth_getBalance a-address 'latest))
       (def b-balance-after (eth_getBalance b-address 'latest))

       ;; TODO: balances in both assets PET and QASPET
       (DDT "DApp completed"
            (.@ Ether .string<-) a-balance-before
            (.@ Ether .string<-) b-balance-before
            (.@ Ether .string<-) a-balance-after
            (.@ Ether .string<-) b-balance-after)

       ;; in PET, a loses t, b gains t
       ;; TODO: in QASPET, b should lose u, a should gain u
       (assert! (<= 0 (- (- a-balance-before t) a-balance-after) gas-allowance))
       (assert! (<= 0 (- (+ b-balance-before t) b-balance-after) gas-allowance))

       (finally
        (ignore-errors (close-port proc-a))
        (ignore-errors (close-port proc-b))
        (ignore-errors (kill (process-pid proc-a)))
        (ignore-errors (kill (process-pid proc-b))))))))

(export #t)

(import
  :gerbil/gambit
  :std/assert :std/format :std/iter
  :std/misc/ports :std/misc/process
  :std/srfi/1
  :std/sugar :std/test
  :std/text/hex :std/text/json
  :clan/base :clan/concurrency :clan/debug :clan/exception
  :clan/io :clan/json :clan/path-config :clan/ports :clan/ffi
  :clan/poo/object :clan/poo/io :clan/poo/debug
  :clan/crypto/keccak :clan/crypto/secp256k1
  :clan/persist/content-addressing :clan/persist/db
  :clan/versioning
  :clan/ethereum/types :clan/ethereum/ethereum :clan/ethereum/known-addresses :clan/ethereum/json-rpc
  :clan/ethereum/simple-apps :clan/ethereum/network-config :clan/ethereum/assets
  :clan/ethereum/ethereum :clan/ethereum/hex :clan/ethereum/transaction :clan/ethereum/types
  :clan/ethereum/testing
  ../compiler/passes
  ../compiler/multipass
  ../compiler/syntax-context
  ../runtime/program
  ../runtime/participant-runtime
  ../runtime/reify-contract-parameters
  ./cli-integration
  ./utils)

(def (~s v) (format "~s" v))

(def buy-sig-short-timeout-integrationtest
  (test-suite "integration test for short timeouts in ethereum/buy-sig"
    (test-case "buy sig times out successfully with a short timeout"
      (setup-test-env)

      (def buyer-address alice)
      (def seller-address bob)
      (def document "abcdefghijklmnopqrstuvwxyz012345")
      (def digest (keccak256<-string document))
      (def price one-ether-in-wei)

      (def timeout (ethereum-timeout-in-blocks))
      (def initial-timer-start (+ (eth_blockNumber) timeout))

      (def buyer-balance-before (eth_getBalance alice 'latest))
      (def seller-balance-before (eth_getBalance bob 'latest))

      (def proc-buyer #f)
      (def proc-seller #f)

      ;; glow start-interaction --evm-network pet --test --max-initial-block '%10' --timeout-in-blocks 1 --glow-app buy_sig --my-identity t/alice --role Buyer --database alice --assets '{"DefaultToken": "PET"}' --participants '{"Buyer": "0xa71CEb0990dD1f29C2a064c29392Fe66baf05aE1", "Seller": "0xb0bb1ed229f5Ed588495AC9739eD1555f5c3aabD"}' --params '{"digest": "0xc5d2460186f7233c927e7db2dcc703c0e500b653ca82273b7bfad8045d85a470", "price": 123451234512345}'

      (try
       (set! proc-buyer
         (open-process
          [path: "./glow"
           arguments: ["start-interaction"
                       "--glow-path" (source-path "dapps")
                       "--evm-network" "pet"
                       "--test"
                       "--max-initial-block" "%10"
                       "--timeout-in-blocks" "1"
                       "--glow-app" "buy_sig"
                       "--my-identity" "t/alice"
                       "--role" "Buyer"
                       ;"--database" "alice"
                       "--params" (string-append "{\"price\": " (number->string price) ", \"digest\": " (~s (string-append "0x" (hex-encode digest))) "}")

                       ;; Similarly, specify one of the participants here. There are only two,
                       ;; so this test doesn't excercise the logic to read this from stdin,
                       ;; but the other integration tests do.
                       ;;
                       ;; N.B. this is bob's id.
                       "--participants" "{\"Seller\": \"0xb0bb1ed229f5Ed588495AC9739eD1555f5c3aabD\"}"
                       "--assets" "{\"DefaultToken\": \"PET\"}"
                       ]]))

       (def buyer-output
         (with-io-port proc-buyer (cut read-line (current-input-port) #f)))

       (def buyer-balance-after (eth_getBalance alice 'latest))

       (def gas-allowance (wei<-ether .01))

       ;(displayln "--- buyer-output ---")
       ;(displayln buyer-output)

       (assert! (<= 0 (- buyer-balance-before buyer-balance-after) gas-allowance))

       (finally
        (ignore-errors (close-port proc-buyer))
        (ignore-errors (kill (process-pid proc-buyer))))))))

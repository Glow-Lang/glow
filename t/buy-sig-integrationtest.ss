(export #t)

(import
  :gerbil/gambit/os :gerbil/gambit/ports :gerbil/gambit/threads
  :std/assert :std/format :std/iter
  :std/misc/ports :std/misc/process
  :std/srfi/1
  :std/sugar :std/test
  :std/text/hex :std/text/json
  :clan/base :clan/concurrency :clan/debug :clan/decimal :clan/exception
  :clan/io :clan/json :clan/path-config :clan/ports :clan/ffi
  :clan/poo/object :clan/poo/io :clan/poo/debug
  :clan/crypto/keccak :clan/crypto/secp256k1
  :clan/persist/content-addressing :clan/persist/db
  :clan/versioning
  :mukn/ethereum/types :mukn/ethereum/ethereum :mukn/ethereum/known-addresses :mukn/ethereum/json-rpc
  :mukn/ethereum/simple-apps :mukn/ethereum/network-config :mukn/ethereum/assets
  :mukn/ethereum/ethereum :mukn/ethereum/hex :mukn/ethereum/transaction :mukn/ethereum/types
  :mukn/ethereum/testing
  ../compiler/passes
  ../compiler/multipass
  ../compiler/syntax-context
  ../runtime/program
  ../runtime/participant-runtime
  ../runtime/reify-contract-parameters
  ./cli-integration
  ./utils)

(def buy-sig-integrationtest
  (test-suite "integration test for ethereum/buy-sig"
    (test-case "buy sig runs successfully"
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

      (try
       (set! proc-buyer
         (open-process
          [path: "./glow"
           arguments: ["start-interaction"
                       "--glow-path" (source-path "dapps")
                       "--evm-network" "pet"
                       "--test"
                       "--handshake" "nc -l 3232"
                       ;; For the sake of testing both the cli flag and the
                       ;; console prompt, we supply one parameter here and the
                       ;; other below.
                       "--params" (string-append "{\"price\": " (number->string price) "}")

                       ;; Similarly, specify one of the participants here. There are only two,
                       ;; so this test doesn't excercise the logic to read this from stdin,
                       ;; but the other integration tests do.
                       ;;
                       ;; N.B. this is bob's id.
                       "--participants" "{\"Seller\": \"0xb0bb1ed229f5Ed588495AC9739eD1555f5c3aabD\"}"
                       "--assets" "{\"DefaultToken\": \"PET\"}"
                       ]]))

       (def peer-command
         (with-io-port proc-buyer
           (lambda ()
             (answer-questions
              [["Choose application:"
                "buy_sig"]
               ["Choose your identity:"
                (lambda (id)
                  (string-prefix? "t/alice " id))]
               ["Choose your role:"
                "Buyer"]])
             (supply-parameters
              [["digest" (string-append "0x" (hex-encode digest))]])
             (set-initial-block 1000) ; Provides an offset from the current-block,
                                      ; so we have ample time (in blocks) to create a contract
                                      ; and for other active participants to run side
                                      ; of the interaction before timeout.
                                      ;
                                      ; Also used for regression testing against:
                                      ; https://gitlab.com/mukn/glow/-/issues/195
             (read-peer-command))))

       (set! proc-seller
         (open-process
          [path: "/bin/sh"
           arguments:
            ["-c" (string-append
                    "./" peer-command
                      " --evm-network pet"
                      " --database /tmp/alt-glow-db"
                      " --glow-path " (source-path "dapps")
                      " --test"
                      " --handshake 'nc localhost 3232'")]]))

       (def seller-environment
         (with-io-port proc-seller
           (lambda ()
             (answer-questions
              [["Choose your identity:"
                (lambda (id) (string-prefix? "t/bob " id))]
               ["Choose your role:"
                "Seller"]])
             (read-environment))))

       (def buyer-environment
         (with-io-port proc-buyer read-environment))

       (assert! (equal? buyer-environment seller-environment))

       (def signature
         (match (hash-ref buyer-environment 'signature)
           (['<-json 'Signature signature] (<-json Signature signature))
           (value (error "Unexpected value for signature: " value))))
       (DBG "parsed Signature: " signature)
       (def valid? (message-signature-valid? bob signature digest))
       (def buyer-balance-after (eth_getBalance alice 'latest))
       (def seller-balance-after (eth_getBalance bob 'latest))

       (DDT "DApp completed"
            Signature signature
            Bool valid?
            (.@ Ether .string<-) price
            (.@ Ether .string<-) buyer-balance-before
            (.@ Ether .string<-) seller-balance-before
            (.@ Ether .string<-) buyer-balance-after
            (.@ Ether .string<-) seller-balance-after)

       ;; Check that the signature was valid and that the money transfer happened, modulo gas allowance
       (def gas-allowance (wei<-ether .01))
       (assert! valid?)
       (assert! (<= 0 (- buyer-balance-before buyer-balance-after price) gas-allowance))
       (assert! (<= 0 (- (+ seller-balance-before price) seller-balance-after) gas-allowance))

       (finally
        (ignore-errors (close-port proc-buyer))
        (ignore-errors (close-port proc-seller))
        (ignore-errors (kill (process-pid proc-buyer)))
        (ignore-errors (kill (process-pid proc-seller))))))))

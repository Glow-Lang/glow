(export #t)

(import
  :gerbil/gambit/os :gerbil/gambit/ports :gerbil/gambit/threads
  :std/assert :std/format :std/iter
  :std/misc/ports :std/misc/process
  :std/srfi/1 :std/srfi/13
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
  :vyzo/libp2p
  ../compiler/passes
  ../compiler/multipass
  ../compiler/syntax-context
  ../runtime/program
  ../runtime/participant-runtime
  ../runtime/p2p
  ../runtime/reify-contract-parameters
  ./cli-integration
  ./utils)

;; To run manually:
;; Open 3 terminals: Buyer, Seller, and Bootstrap
;; On Bootstrap:
;;   gxi
;;   > (import :vyzo/libp2p :mukn/glow/runtime/p2p)
;;   > (defvalues (boot-c boot-d) (do-bootstrap "/ip4/127.0.0.1/tcp/10330"))
;;   > (def boot-addr (peer-info->string (libp2p-identify boot-c)))
;;   > boot-addr
;;   copy result as <boot-addr>
;; On Buyer:
;;   BOOT_ADDR=<boot-addr>
;;   ./glow start-interaction --evm-network pet --test --host-address /ip4/127.0.0.1/tcp/10331 --off-chain-channel libp2p --params '{"price": 1}' --participants '{"Seller": "0xb0bb1ed229f5Ed588495AC9739eD1555f5c3aabD"}' --assets '{"DefaultToken": "PET"}' --circuit-relay-address $BOOT_ADDR --pubsub-node $BOOT_ADDR
;;   Choose identity: 1 = alice
;;   Choose application: 2 = buy_sig
;;   Choose role: 1 = Buyer
;;   Enter digest: 0x7a6f737e43d8df6b957106ab38e3ee7356f9640efbd029c659cfb25aa1f033a1
;;   Enter initial-block: exsting plus at least 1000
;; On Seller:
;;   BOOT_ADDR=<boot-addr>
;;   ./glow start-interaction --evm-network pet --database /tmp/alt-glow-db --test --host-address /ip4/127.0.0.1/tcp/10302 --off-chain-channel libp2p --circuit-relay-address $BOOT_ADDR --pubsub-node $BOOT_ADDR
;;   Choose identity: 2 = bob
;;   Choose role: 2 = Seller

(def buy-sig-libp2p-without-wait-for-agreement-integrationtest
  (test-suite "integration test for ethereum/buy-sig over libp2p channel without wait-for-agreement"
    (test-case "buy sig over libp2p runs successfully without wait-for-agreement"
     (parameterize ((current-libp2p-daemon #f))
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

      ;;Start the glow Bootstrap node and store the client info

      (defvalues (boot-c boot-d) (do-bootstrap "/ip4/127.0.0.1/tcp/10330"))
      (def boot-addr (peer-info->string (libp2p-identify boot-c))) ;;get the addr of boot-c

      (try
       (DBG "Starting buyer thread")

       (set! proc-buyer
         (open-process
          [path: "./glow"
           arguments: ["start-interaction"
                       "--glow-path" (source-path "dapps")
                       "--evm-network" "pet"
                       "--test"
                       ;; For the sake of testing both the cli flag and the
                       ;; console prompt, we supply one parameter here and the
                       ;; other below.
                       "--params" (string-append "{\"price\": " (number->string price) "}")

                       "--host-address" "/ip4/127.0.0.1/tcp/10302"
                       "--circuit-relay-address" boot-addr
                       "--pubsub-node" boot-addr
                       "--off-chain-channel" "libp2p"


                       ;; Similarly, specify one of the participants here. There are only two,
                       ;; so this test doesn't excercise the logic to read this from stdin,
                       ;; but the other integration tests do.
                       ;;
                       ;; N.B. this is bob's id.
                       "--participants" "{\"Seller\": \"0xb0bb1ed229f5Ed588495AC9739eD1555f5c3aabD\"}"
                       "--assets" "{\"DefaultToken\": \"PET\"}"
                       ]
           ]))

       (DBG "Spawning seller proc")

       (set! proc-seller
         (open-process
          [path: "./glow"
           arguments: ["start-interaction"
                       "--glow-path" (source-path "dapps")
                       "--evm-network" "pet"
                       "--test"

                       "--host-address" "/ip4/127.0.0.1/tcp/10331"
                       "--circuit-relay-address" boot-addr
                       "--pubsub-node" boot-addr
                       "--off-chain-channel" "libp2p"
                       ;; Similarly, specify one of the participants here. There are only two,
                       ;; so this test doesn't excercise the logic to read this from stdin,
                       ;; but the other integration tests do.
                       ;;
                       ;; N.B. this is bob's id.
                       "--participants" "{\"Seller\": \"0xb0bb1ed229f5Ed588495AC9739eD1555f5c3aabD\"}"
                       "--assets" "{\"DefaultToken\": \"PET\"}"

                       "--database" "/tmp/alt-glow-db"
                       ]]))

       (DBG "Filling up buyer prompt")

       ;; Fill up buyer prompt
       (with-io-port proc-buyer
         (lambda ()
           (answer-questions
            [["Choose your identity:"
              (lambda (id)
                (string-prefix? "t/alice " id))]
             ["Choose application:"
              "buy_sig"]
             ["Choose your role:"
              "Buyer"]])
            (supply-parameters
              [["digest" (string-append "0x" (hex-encode digest))]])
            (set-initial-block/round-up 1000)
            (find-first-line (cut string-contains <> "Sending agreement to multiaddr..."))))

       (DBG "Filled up buyer prompt")

       (thread-sleep! 30)

       (DBG "Filling up seller prompt")

       ;; reply to seller prompts
       (with-io-port proc-seller
         (lambda ()
           (answer-questions
             [["Choose your identity:"
               (lambda (id) (string-prefix? "t/bob " id))]])))

       (DBG "Choosing role")

       (with-io-port proc-seller
         (lambda ()
           (answer-questions
             [["Choose your role:"
               "Seller"]])))

       (DBG "Reading end environment for seller")

       (def seller-environment
         (with-io-port proc-seller
           (lambda ()
             (read-environment))))


       (DBG "Reading end environment for buyer")

       (def buyer-environment
         (with-io-port proc-buyer read-environment))

       (DBG "Checking buyer and seller environment")

       (assert! (equal? buyer-environment seller-environment))

       (DBG "Verifying signature")

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


       (DBG "Verify gas balance")

       ;; Check that the signature was valid and that the money transfer happened, modulo gas allowance
       (def gas-allowance (wei<-ether .01))
       (assert! valid?)
       (assert! (<= 0 (- buyer-balance-before buyer-balance-after price) gas-allowance))
       (assert! (<= 0 (- (+ seller-balance-before price) seller-balance-after) gas-allowance))

       (finally
        (libp2p-close boot-c)
        (stop-libp2p-daemon! boot-d)
        (ignore-errors (close-port proc-buyer))
        (ignore-errors (close-port proc-seller))
        (ignore-errors (kill (process-pid proc-buyer)))
        (ignore-errors (kill (process-pid proc-seller)))))))))

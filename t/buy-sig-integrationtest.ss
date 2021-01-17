(export #t)

(import
  :gerbil/gambit/os :gerbil/gambit/ports :gerbil/gambit/threads
  :std/format :std/srfi/1 :std/test :std/sugar :std/iter :std/text/json :std/misc/ports
  :clan/base :clan/concurrency :clan/debug :clan/decimal :clan/exception
  :clan/io :clan/json :clan/path-config :clan/ports
  :clan/poo/poo :clan/poo/io :clan/poo/debug
  :clan/crypto/keccak
  :clan/persist/content-addressing
  :clan/versioning
  :mukn/ethereum/types :mukn/ethereum/ethereum :mukn/ethereum/known-addresses :mukn/ethereum/json-rpc
  :mukn/ethereum/batch-send :mukn/ethereum/network-config :mukn/ethereum/assets
  :mukn/ethereum/signing :mukn/ethereum/hex :mukn/ethereum/transaction :mukn/ethereum/types
  :mukn/ethereum/t/signing-test
  :mukn/ethereum/t/50-batch-send-integrationtest
  ../compiler/passes
  ../compiler/multipass
  ../compiler/syntax-context
  ../runtime/program
  ../runtime/ethereum-runtime)

(def buyer-address alice)
(def seller-address bob)
(def digest (keccak256<-string "abcdefghijklmnopqrstuvwxyz012345"))

;; Should `timeout` be the value of `(ethereum-timeout-in-blocks)`,
;; or should it be the `timeoutInBlocks` field of the entry in `config/ethereum_networks.json`?
(def timeout (ethereum-timeout-in-blocks))
(def initial-timer-start (+ (eth_blockNumber) timeout))

(def buy_sig.glow (source-path "examples/buy_sig.glow"))

(def agreement
  (.o
    glow-version: (software-identifier)
    interaction: "mukn/glow/examples/buy_sig#payForSignature"
    participants: (.o Buyer: buyer-address Seller: seller-address)
    parameters: (hash
                  (digest0 (json<- Digest digest))
                  (price (json<- Ether one-ether-in-wei)))
    reference: (.o Buyer: "Purchase #42"
                   Seller: "Sale #101")
    options: (.o blockchain: "Private Ethereum Testnet" ;; the `name` field of an entry in `config/ethereum_networks.json`
                 escrowAmount: (void) ;; not meaningful for buy_sig in particular
                 timeoutInBlocks: timeout ; should be the `timeoutInBlocks` field of the same entry in `config/ethereum_networks.json`
                 maxInitialBlock: initial-timer-start)
    code-digest: (digest<-file buy_sig.glow)))

(def compiler-output (run-passes buy_sig.glow pass: 'project show?: #f))

(def buy-sig-integrationtest
  (test-suite "integration test for ethereum/buy-sig"
    (DBG "Ensure participants funded")
    (ensure-addresses-prefunded)
    (DBG "DONE")
    (test-case "buy sig parses"
      (def program (parse-compiler-output compiler-output))

    ;; TODO: run buyer and seller step in separate threads, using posted transactions to progress their state
    (test-case "buy sig executes"
      (ignore-errors (delete-file (run-path "contract-handshake.json"))) ;; TODO: do it better

      (displayln "\nEXECUTING BUYER THREAD ...")
      (def buyer-thread
        (spawn/name/logged "Buyer"
         (lambda ()
           (def buyer-runtime
             (make-Runtime role: 'Buyer
                           agreement: agreement
                           current-code-block-label: 'begin0 ;; TODO: grab the start label from the compilation output, instead of 'begin0
                           current-label: 'begin ;; TODO: grab the start label from the compilation output, instead of 'begins
                           program: program))
           {execute buyer-runtime}
           (displayln "buyer finished")
           (@ buyer-runtime environment))))

      (displayln "\nEXECUTING SELLER THREAD ...")
      (def seller-thread
        (spawn/name/logged "Seller"
         (lambda ()
           (def seller-runtime
             (make-Runtime role: 'Seller
                           agreement: agreement
                           current-code-block-label: 'begin0 ;; TODO: grab the start label from the compilation output, instead of 'begin0
                           current-label: 'begin ;; TODO: grab the start label from the compilation output, instead of 'begins
                           program: program))
           {execute seller-runtime}
           (displayln "seller finished"))))

      (def environment (thread-join! buyer-thread))

      (def signature (hash-get environment 'signature))
      (display-poo-ln
          ["Signature extracted from contract logs: " Signature (json<- Signature signature)
           "valid?: " (message-signature-valid? seller-address signature digest)])))))

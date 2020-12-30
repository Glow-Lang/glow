(export #t)

(import
  :gerbil/gambit/os :gerbil/gambit/ports :gerbil/gambit/threads
  :std/format :std/srfi/1 :std/test :std/sugar :std/iter :std/text/json :std/misc/ports
  :clan/base :clan/concurrency :clan/debug :clan/decimal :clan/exception
  :clan/io :clan/json :clan/path-config :clan/ports
  :clan/poo/poo :clan/poo/io (only-in :clan/poo/mop display-poo-ln) :clan/crypto/keccak
  :mukn/ethereum/ethereum :mukn/ethereum/known-addresses :mukn/ethereum/json-rpc
  :mukn/ethereum/batch-send :mukn/ethereum/network-config :mukn/ethereum/assets
  :mukn/ethereum/signing :mukn/ethereum/hex :mukn/ethereum/transaction :mukn/ethereum/types
  :mukn/ethereum/t/signing-test
  :mukn/ethereum/t/50-batch-send-integrationtest
  ../compiler/passes
  ../compiler/multipass
  ../compiler/syntax-context
  ../runtime/program
  ../runtime/ethereum-contract
  ../runtime/ethereum-runtime)

(def buyer-address alice)
(def seller-address bob)
(def digest (keccak256<-string "abcdefghijklmnopqrstuvwxyz012345"))

(def participants
  (hash
    (Buyer buyer-address)
    (Seller seller-address)))
(def arguments
  (hash
    (digest0 [Digest . digest])
    (price [Ether . one-ether-in-wei])))

(def compiler-output (run-passes (source-path "examples/buy_sig.glow") pass: 'project show?: #f))

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
      (def initial-timer-start (+ (eth_blockNumber) (ethereum-timeout-in-blocks)))
      (def timeout 20)

      (displayln "\nEXECUTING BUYER THREAD ...")
      (def buyer-thread
        (spawn/name/logged "Buyer"
         (lambda ()
           (def contract (make-Contract
                          program: program
                          participants: participants
                          arguments: arguments
                          initial-timer-start: initial-timer-start
                          timeout: timeout))
           (def buyer-runtime
             (make-Runtime role: 'Buyer
                           contract: contract
                           current-code-block-label: 'begin0 ;; TODO: grab the start label from the compilation output, instead of 'begin0
                           current-label: 'begin)) ;; TODO: grab the start label from the compilation output, instead of 'begins
           {execute buyer-runtime}
           (displayln "buyer finished")
           (@ buyer-runtime environment))))

      (displayln "\nEXECUTING SELLER THREAD ...")
      (def seller-thread
        (spawn/name/logged "Seller"
         (lambda ()
           (def contract (make-Contract
                          program: program
                          participants: participants
                          arguments: arguments
                          initial-timer-start: initial-timer-start
                          timeout: timeout))
           (def seller-runtime
             (make-Runtime role: 'Seller
                           contract: contract
                           current-code-block-label: 'begin0 ;; TODO: grab the start label from the compilation output, instead of 'begin0
                           current-label: 'begin)) ;; TODO: grab the start label from the compilation output, instead of 'begins
           {execute seller-runtime}
           (displayln "seller finished"))))

      (def environment (thread-join! buyer-thread))

      (def signature (hash-get environment 'signature))
      (display-poo-ln
          ["Signature extracted from contract logs: " Signature (json<- Signature signature)
           "valid?: " (message-signature-valid? seller-address signature digest)])))))

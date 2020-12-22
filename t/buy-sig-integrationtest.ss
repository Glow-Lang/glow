(export #t)

(import
  :gerbil/gambit/ports :gerbil/gambit/threads
  :std/format :std/srfi/1 :std/test :std/sugar :std/iter :std/text/json :std/misc/ports
  :clan/poo/poo :clan/poo/io (only-in :clan/poo/mop display-poo-ln) :clan/crypto/keccak
  :clan/base :clan/decimal :clan/ports :clan/io :clan/path-config :clan/json
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
    (ensure-addresses-prefunded)
    (test-case "buy sig parses"
      (def program (parse-compiler-output compiler-output))

    ;; TODO: run buyer and seller step in separate threads, using posted transactions to progress their state
    (test-case "buy sig executes"
      (def contract (make-Contract
        program: program
        participants: participants
        arguments: arguments
        timeout: 20))

      (def environment #f)

      ;; TODO: erase run/contract-handshake.json from filesystem

      (displayln "\nEXECUTING BUYER THREAD ...")
      (spawn-thread (lambda ()
        (def buyer-runtime (make-Runtime 'Buyer contract))
        {execute buyer-runtime}
        (displayln "buyer finished")
        (set! environment (@ buyer-runtime environment))))

      (while (not (file-exists? "run/contract-handshake.json"))
        (displayln "waiting for contract handshake ...")
        (thread-sleep! 1))

      (displayln "\nEXECUTING SELLER THREAD ...")
      (spawn-thread (lambda ()
        (def seller-runtime (make-Runtime 'Seller contract))
        {execute seller-runtime}
        (displayln "seller finished")))

      (while (not environment)
        (displayln "waiting for buyer to finish ...")
        (thread-sleep! 1))

      (def signature (hash-get environment 'signature))
      (display-poo-ln
          ["Signature extracted from contract logs: " Signature signature
           "valid?: " (message-signature-valid? seller-address signature digest)])))))

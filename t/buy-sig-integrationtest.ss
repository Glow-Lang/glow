#|
$ gxi
> (add-load-path (path-normalize "../gerbil-ethereum"))
> (def d (current-directory))
> (import :mukn/ethereum/scripts/run-geth-test-net)
> (current-directory d)
> (import "t/buy-sig-integrationtest.ss")
|#
(export #t)

(import
  :gerbil/gambit/ports
  :std/format :std/srfi/1 :std/test :std/sugar :std/iter :std/text/json :std/misc/ports
  :clan/poo/poo :clan/poo/io (only-in :clan/poo/mop display-poo) :clan/crypto/keccak
  :clan/base :clan/decimal :clan/ports :clan/io :clan/path-config :clan/json
  :mukn/ethereum/ethereum :mukn/ethereum/known-addresses :mukn/ethereum/json-rpc
  :mukn/ethereum/batch-send :mukn/ethereum/network-config :mukn/ethereum/assets
  :mukn/ethereum/signing :mukn/ethereum/hex :mukn/ethereum/transaction :mukn/ethereum/types
  :mukn/ethereum/t/signing-test
  :mukn/ethereum/t/transaction-integrationtest
  :mukn/ethereum/t/batch-send-integrationtest
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
    (test-case "buy sig parses"
      (def program (parse-compiler-output compiler-output))

    ;; TODO: run buyer and seller step in separate threads, using posted transactions to progress their state
    (test-case "buy sig executes"
      (def contract (make-Contract
        program: program
        participants: participants
        arguments: arguments))

      (displayln "\nEXECUTING BUYER STEP 1 ...")
      (def runtime-buyer-1 (make-Runtime contract 'Buyer))
      {execute runtime-buyer-1}

      (def handshake-json (json<-string (string<-json (read-file-json "contract-handshake.json"))))
      (def handshake (<-json ContractHandshake handshake-json))

      (displayln "\nEXECUTING SELLER STEP 1 ...")
      (def runtime-seller-1 (make-Runtime contract  'Seller handshake 'cp0))
      {execute runtime-seller-1}

      (def tx-receipt-json (json<-string (string<-json (read-file-json "tx-receipt.json"))))
      (def tx-receipt (<-json TransactionReceipt tx-receipt-json))

      (displayln "\nEXECUTING BUYER STEP 2 ...")
      (def runtime-buyer-2 (make-Runtime contract 'Buyer tx-receipt 'cp0))
      {execute runtime-buyer-2}
      (def signature (hash-get (@ runtime-buyer-2 environment) 'signature))
      (display-poo
          ["Signature extracted from contract logs: " Signature signature
          "valid?: " (message-signature-valid? seller-address signature digest)])))))
(export #t)

(import
  :gerbil/gambit/ports
  :std/format :std/srfi/1 :std/test :std/sugar :std/iter :std/text/json
  :clan/poo/poo :clan/poo/io (only-in :clan/poo/mop display-poo)
  :clan/base :clan/decimal :clan/ports :clan/io :clan/path-config
  :clan/crypto/keccak
  :clan/persist/db
  :mukn/ethereum/ethereum :mukn/ethereum/known-addresses :mukn/ethereum/json-rpc
  :mukn/ethereum/batch-send :mukn/ethereum/network-config :mukn/ethereum/assets
  :mukn/ethereum/known-addresses :mukn/ethereum/signing :mukn/ethereum/hex :mukn/ethereum/transaction
  :mukn/ethereum/t/signing-test
  :mukn/ethereum/t/transaction-integrationtest
  :mukn/ethereum/t/batch-send-integrationtest
  :mukn/ethereum/types
  ../compiler/passes
  ../compiler/multipass
  ../compiler/syntax-context
  ../runtime/ethereum-interpreter
  )

(def buyer-address alice-address)
(def seller-address bob-address)
(def digest (keccak256<-string "abcdefghijklmnopqrstuvwxyz012345"))

(def participants
  (hash
    (Buyer buyer-address)
    (Seller seller-address)))
(def arguments
  (hash
    (digest0 [Digest . digest])
    (price [Ether . one-ether-in-wei])))

;; TODO: Instead, let the language interpreter parse the logs and return a first-class environment,
;; then extract the signature from the environment.
;; Signature <- TransactionReceipt
(def (extract-signature receipt)
  (!> receipt
      (cut .@ <> logs)
      car
      (cut .@ <> data)
      (<-bytes<-unmarshal
       (lambda (port)
         (begin0 (unmarshal-signature port)
           (assert! (= 1 (read-u8 port))))))))

(def state (run-passes "examples/buy_sig.glow" pass: 'project show?: #f))

(def buy-sig-integrationtest
  (test-suite "integration test for ethereum/buy-sig"
    (test-case "buy sig parses"
      (def program (parse-compiler-output state))
    (test-case "buy sig executes"
      (def interpreter (make-Interpreter
        program: program
        participants: participants
        arguments: arguments))
      (displayln "\nEXECUTING BUYER STEP 1...")
      (def contract-handshake {execute-buyer interpreter buyer-address})

      (displayln "\nEXECUTING SELLER STEP 1...")
      (def result {execute-seller interpreter contract-handshake seller-address})

      (displayln "\nEXECUTING BUYER STEP 2...")
      (def signature (extract-signature result))
      (display-poo ["Signature extracted from contract logs: " Signature signature
                    "valid?: " (message-signature-valid? seller-address signature digest) "\n\n"])))))

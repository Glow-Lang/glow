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
  :clan/poo/poo :clan/poo/io (only-in :clan/poo/mop display-poo)
  :clan/base :clan/decimal :clan/ports :clan/io :clan/path-config :clan/json
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
  ../runtime/ethereum-execution-context
  )

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

(def state (run-passes (source-path "examples/buy_sig.glow") pass: 'project show?: #f))

; (defclass ExecutionContext (role contract current-code-block current-label locals message)
;  transparent: #t)

(def buy-sig-integrationtest
  (test-suite "integration test for ethereum/buy-sig"
    (test-case "buy sig parses"
      (def program (parse-compiler-output state))
    (test-case "buy sig executes"
      (def interpreter (make-Contract
        program: program
        participants: participants
        arguments: arguments))

      (displayln "\nEXECUTING BUYER ...")
      (set! (@ interpreter execution-context) (make-ExecutionContext 'Buyer))
      {execute interpreter}

      (def handshake-json (json<-string (string<-json (read-file-json "contract-handshake.json"))))
      (displayln (hash-keys handshake-json))
      (def handshake (<-json ContractHandshake handshake-json))
      (displayln handshake)

      (displayln "\nEXECUTING SELLER ...")
      (set! (@ interpreter execution-context) (make-ExecutionContext 'Seller handshake 'cp0))
      {execute interpreter}))))

      ; (displayln "\nEXECUTING SELLER STEP 1...")
      ; (def result {execute-seller interpreter contract-handshake seller-address})

      ; (displayln "\nEXECUTING BUYER STEP 2...")
      ; (def signature (extract-signature result))
      ; (display-poo ["Signature extracted from contract logs: " Signature signature
      ;               "valid?: " (message-signature-valid? seller-address signature digest) "\n\n"])))))

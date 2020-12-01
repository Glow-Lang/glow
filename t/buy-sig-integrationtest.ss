(export #t)

(import
  :std/format :std/srfi/1 :std/test :std/sugar :std/iter
  :clan/persist/db :clan/decimal :clan/poo/poo :clan/poo/io :clan/path-config
  :mukn/ethereum/ethereum :mukn/ethereum/known-addresses :mukn/ethereum/json-rpc
  :mukn/ethereum/batch-send :mukn/ethereum/network-config :mukn/ethereum/assets
  :mukn/ethereum/known-addresses :mukn/ethereum/signing :mukn/ethereum/hex :mukn/ethereum/transaction
  :mukn/ethereum/t/signing-test
  :mukn/ethereum/t/transaction-integrationtest
  :mukn/ethereum/t/batch-send-integrationtest
  ../runtime/ethereum-interpreter
  )

(ensure-db-connection (run-path "testdb"))

(def participants
  (hash
    (Buyer (address<-0x "0xC54e86DFFb87B9736E2E35DD85c775358F1c31CE"))
    (Seller (address<-0x "0xF47408143d327e4bc6A87EF4a70A4E0aF09b9A1C"))))
(def arguments
  (hash
    (digest0 [(string->bytes "abcdefghijklmnopqrstuvwxyz012345") Digest])
    (price [10000000 Ether])))

(for ((values name address) (in-hash participants))
  (register-keypair
    (symbol->string name)
    (keypair ;; KLUGE: Fake public and secret key data. We only use the address via Geth.
      address
      (<-json PublicKey "0x020000000000000000000000000000000000000000000000000000000000000001")
      (<-json SecretKey "0x0000000000000000000000000000000000000000000000000000000000000001")
      ""))
  (ensure-eth-signing-key address))

(def buy-sig-integrationtest
  (test-suite "integration test for ethereum/buy-sig"
    (test-case "buy sig parses"
      (def program (parse-project-output "./examples/buy_sig.project.sexp"))
    (test-case "buy sig executes"
      (def interpreter (make-Interpreter
        program: program
        participants: participants
        arguments: arguments))
      {execute-buyer interpreter}))))


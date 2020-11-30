(export #t)

(import
  :std/format :std/srfi/1 :std/test :std/sugar :std/iter
  :clan/persist/db :clan/decimal :clan/poo/poo :clan/poo/io :clan/path-config
  :mukn/ethereum/ethereum :mukn/ethereum/known-addresses :mukn/ethereum/json-rpc
  :mukn/ethereum/batch-send :mukn/ethereum/network-config :mukn/ethereum/assets
  :mukn/ethereum/known-addresses :mukn/ethereum/signing :mukn/ethereum/hex :mukn/ethereum/transaction
  :mukn/ethereum/t/signing-test
  :mukn/ethereum/t/transaction-integrationtest
  ; :mukn/ethereum/t/batch-send-integrationtest
  ../runtime/ethereum-interpreter
  )

(ensure-db-connection (run-path "testdb"))

(def participants
  (hash
    (Buyer #u8(197 78 134 223 251 135 185 115 110 46 53 221 133 199 117 53 143 28 49 206))
    (Seller #u8(244 116 8 20 61 50 126 75 198 168 126 244 167 10 78 10 240 155 154 28))))
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


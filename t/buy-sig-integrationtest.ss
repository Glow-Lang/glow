(export #t)

(import
  :gerbil/gambit/ports
  :std/format :std/srfi/1 :std/test :std/sugar :std/iter :std/text/json
  :clan/poo/poo :clan/poo/io (only-in :clan/poo/mop display-poo)
  :clan/decimal :clan/ports :clan/io :clan/path-config
  :clan/crypto/keccak
  :clan/persist/db
  :mukn/ethereum/ethereum :mukn/ethereum/known-addresses :mukn/ethereum/json-rpc
  :mukn/ethereum/batch-send :mukn/ethereum/network-config :mukn/ethereum/assets
  :mukn/ethereum/known-addresses :mukn/ethereum/signing :mukn/ethereum/hex :mukn/ethereum/transaction
  :mukn/ethereum/t/signing-test
  :mukn/ethereum/t/transaction-integrationtest
  :mukn/ethereum/t/batch-send-integrationtest
  :mukn/ethereum/types
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
    (digest0 [digest Digest])
    (price [10000000 Ether])))

;; TODO: in general, properly parse the log-data
;; Signature <- TransactionReceipt
(def (extract-signature receipt)
  (def logs (.@ receipt logs))
  (def first-log (car logs))
  (def log-data (.@ first-log data))
  ((<-bytes<-unmarshal
    (lambda (port)
      (begin0 (unmarshal-signature port)
        (assert! (= 1 (read-u8 port))))))
   log-data))

(def buy-sig-integrationtest
  (test-suite "integration test for ethereum/buy-sig"
    (test-case "buy sig parses"
      (def program (parse-project-output "./examples/buy_sig.project.sexp"))
    (test-case "buy sig executes"
      (def interpreter (make-Interpreter
        program: program
        participants: participants
        arguments: arguments))
      (displayln "Executing buyer move ...")
      (def contract-handshake {execute-buyer interpreter buyer-address})
      (display-poo ["Handshake: " ContractHandshake contract-handshake "\n"])
      (displayln "Executing seller move ...")
      (def result {execute-seller interpreter contract-handshake seller-address})
      (def signature (extract-signature result))
      (displayln "valid?: "
                 (message-signature-valid? seller-address signature digest))))))

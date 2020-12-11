(export #t)

(import
  :std/format :std/srfi/1 :std/test :std/sugar :std/iter :std/text/json
  :clan/persist/db :clan/decimal :clan/poo/poo :clan/poo/io :clan/path-config :clan/ports
  :mukn/ethereum/ethereum :mukn/ethereum/known-addresses :mukn/ethereum/json-rpc
  :mukn/ethereum/batch-send :mukn/ethereum/network-config :mukn/ethereum/assets
  :mukn/ethereum/known-addresses :mukn/ethereum/signing :mukn/ethereum/hex :mukn/ethereum/transaction
  :mukn/ethereum/t/signing-test
  :mukn/ethereum/t/transaction-integrationtest
  :mukn/ethereum/t/batch-send-integrationtest
  ../runtime/ethereum-interpreter
  )

(ensure-db-connection (run-path "testdb"))

(def parameter-types
  (hash
    (digest0 Digest)
    (price Ether)))

#|
> (add-load-path (path-normalize "../gerbil-ethereum"))
> (def d (current-directory))
> (import :mukn/ethereum/scripts/run-ethereum-test-net)
> (current-directory d)
> (0x<-bytes (string->bytes "abcdefghijklmnopqrstuvwxyz012345"))
"0x6162636465666768696a6b6c6d6e6f707172737475767778797a303132333435"
> (import "./t/buy-sig-demo.ss")
|#

#|
enter JSON agreement or handshake:
["agreement",
 { "interaction": "payForSignature",
   "participants": { "Buyer": "0xC54e86DFFb87B9736E2E35DD85c775358F1c31CE",
                     "Seller": "0xF47408143d327e4bc6A87EF4a70A4E0aF09b9A1C" },
   "parameters": { "digest0": "0x6162636465666768696a6b6c6d6e6f707172737475767778797a303132333435",
                   "price": 10000000 },
   "options": {} }]
|#

(displayln "enter JSON agreement or handshake:")
(def aj (read-json))
(def ajo (with ((["agreement" ajo] aj)) ajo))
(def interaction-name (hash-ref ajo 'interaction))
(def participant-name-jaddresses (hash-ref ajo 'participants))
(def parameter-name-jvalues (hash-ref ajo 'parameters))

(def participants
  (list->hash-table
   (for/collect ((p (hash->list participant-name-jaddresses)))
     (with (([k . v] p)) (cons k (address<-0x v))))))
(def arguments
  (list->hash-table
   (for/collect ((p (hash->list parameter-name-jvalues)))
     (with (([k . v] p))
       (def t (hash-ref parameter-types k))
       (cons k [(<-json t v) t])))))

(def buyer-address (hash-ref participants 'Buyer))
(def seller-address (hash-ref participants 'Seller))

(for ((values name address) (in-hash participants))
  (register-keypair
    (symbol->string name)
    (keypair ;; KLUGE: Fake public and secret key data. We only use the address via Geth.
      address
      (<-json PublicKey "0x020000000000000000000000000000000000000000000000000000000000000001")
      (<-json SecretKey "0x0000000000000000000000000000000000000000000000000000000000000001")
      ""))
  (ensure-eth-signing-key address))

(def program (parse-project-output "./examples/buy_sig.project.sexp"))
(def interpreter
  (make-Contract
    program: program
    participants: participants
    arguments: arguments))
(displayln "executing buyer move ...")
(def contract-handshake {execute-buyer interpreter buyer-address})
(def message-string (json-object->string ["handshake" (json<- ContractHandshake contract-handshake)]))
(displayln "copy and send the following JSON handshake to other participants:")
(displayln message-string)
(displayln "enter JSON agreement or handshake:")
(def hj (read-json))
(def hjo (with ((["handshake" hjo] hj)) hjo))
(def contract-handshake-2 (<-json ContractHandshake hjo))
(displayln "executing seller move ...")
{execute-seller interpreter contract-handshake-2 seller-address}


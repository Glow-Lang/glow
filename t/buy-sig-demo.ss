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
> (json<- Bytes20 alice-address)
"0xc54e86dffb87b9736e2e35dd85c775358f1c31ce"
> (json<- Bytes20 bob-address)
"0x9ccaed210ce8c0cb49c5ad1c4f583406c264ba69"
> (0x<-bytes (string->bytes "abcdefghijklmnopqrstuvwxyz012345"))
"0x6162636465666768696a6b6c6d6e6f707172737475767778797a303132333435"
|#

#|
enter JSON agreement or handshake:
["agreement",
 { "interaction": "payForSignature",
   "participants": { "Buyer": "0xc54e86dffb87b9736e2e35dd85c775358f1c31ce",
                     "Seller": "0x9ccaed210ce8c0cb49c5ad1c4f583406c264ba69" },
   "parameters": { "digest0": "0x6162636465666768696a6b6c6d6e6f707172737475767778797a303132333435",
                   "price": 10000000 },
   "options": {} }]
|#

(def aj (read-json))
(def interaction-name (hash-ref aj 'interaction))
(def participant-name-jaddresses (hash-ref aj 'participants))
(def parameter-name-jvalues (hash-ref aj 'parameters))

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
  (make-Interpreter
    program: program
    participants: participants
    arguments: arguments))
(displayln "executing buyer move ...")
(def contract-handshake {execute-buyer interpreter buyer-address})
(def message-string (json-object->string (json<- ContractHandshake contract-handshake)))
(displayln message-string)
(displayln "executing seller move ...")
{execute-seller interpreter contract-handshake seller-address}


;; Ethereum JSON-RPC API
;;
;; The reference documentation is now at
;;   https://github.com/ethereum/wiki/wiki/JSON-RPC
;;   https://geth.ethereum.org/docs/rpc/server
;;
;; See also in CL:
;;   https://github.com/imnisen/web3.lisp
;;   https://github.com/tsikov/ethi
;;
;; TODO:
;; - Make this work
;; - More systematically lift entire namespaces, document which are left out, and
;;   what version of the reference documents were used.

(export #t)

(import
  :gerbil/gambit/threads
  (for-syntax :std/format)
  :std/format :std/lazy :std/sugar
  :clan/net/json-rpc
  :clan/poo/poo :clan/poo/brace
  :clan/utils/json :clan/utils/maybe :clan/utils/path-config
  ./types ./signing ./ethereum ./config
  )

;; Use a mutex for access to geth, not to overload it and get timeouts.
;; have a pool of a small number of connections to geth rather than just one.
(def ethereum-mutex (make-mutex 'ethereum))

(def (ethereum-json-rpc method-name result-decoder param-encoder
                        timeout: (timeout #f) log: (log #f)
                        params)
  (when log
    (log "ETH json rpc method-name=~a" method-name))
  (with-lock ethereum-mutex
             (cut json-rpc (ethereum-rpc-config) method-name params
                  result-decoder: result-decoder
                  param-encoder: param-encoder
                  timeout: timeout log: log)))

(defsyntax (define-ethereum-api stx)
  (syntax-case stx (<-)
    ((_ namespace method result-type <- argument-type ...)
     (let* ((method-name (apply format "~a_~a" (syntax->datum #'(namespace method))))
            (fun-id (datum->syntax (stx-car stx) (string->symbol method-name))))
       (with-syntax ((method-name method-name) (fun-id fun-id))
         #'(begin
             (def params-type (Tuple argument-type ...))
             (def (fun-id timeout: (timeout #f) log: (log #f) . a)
                 (ethereum-json-rpc method-name
                                    (.@ result-type methods .<-json)
                                    (.@ params-type methods .json<-) (list->vector a)
                                    timeout: timeout log: log))))))))

(define-ethereum-api web3 clientVersion String <-)

(define-type BlockParameter
  (Union
   Quantity ;; block number as 0x string. In practice, should fit 32-bit
   (Enum latest earliest pending)))

(define-type TransactionCondition
  (Union
   (Record block: [Quantity]) ;; block number as 0x string.
   (Record time: [Quantity]) ;; time in seconds-since-epoch as 0x string
   Unit)) ;; JSON null, isomorphic to unit, but its own thing for faithful FFI purposes. #!void in Gerbil?

(define-type TransactionParameters
  (Record
   from: [Address]
   to: [(Maybe Address) default: null]
   gas: [(Maybe Quantity) default: null] ; in gas
   gasPrice: [(Maybe Quantity) default: null] ; in wei/gas
   value: [(Maybe Quantity) default: null] ; in wei
   data: [(Maybe Bytes) default: null]
   nonce: [(Maybe Quantity) default: null]
   condition: [(Maybe TransactionCondition) default: null]))

(def ToData<-Operation
  (match <>
    ((TransferTokens recipient) (values recipient null))
    ((CreateContract code) (values null code))
    ((CallFunction recipient data) (values recipient data))))

(def (TransactionParameters<-Operation from operation value)
  (defvalues (to data) (ToData<-Operation operation))
  ;; TransactionParameters
  {(from) (to) (value) (data)
   gas: null gasPrice: null nonce: null condition: null})

(def (Transactionparameters<-PreTransaction sender pretx)
  (defrule (.pretx x ...) (begin (def x (.@ pretx x)) ...)) (.pretx operation gas value)
  {(:: @ (TransactionParameters<-Operation sender operation value)) (gas)})

(def (TransactionParameters<-Transaction tx)
  (defrule (.tx x ...) (begin (def x (.@ tx x)) ...)) (.tx operation tx-header)
  (defrule (.th x ...) (begin (def x (.@ tx-header x)) ...)) (.th sender nonce gas gasPrice value)
  {(:: @ (TransactionParameters<-Operation sender operation value)) (gas) (gasPrice) (value) (nonce)})

(define-type TransactionResult
  (Record
   hash: [Digest]
   nonce: [Quantity]
   blockHash: [(Maybe Digest) default: null]
   blockNumber: [(Maybe Quantity) default: null]
   transactionIndex: [(Maybe Quantity) default: null]
   from: [(Maybe Address) default: null] ;; TODO: can it actually be null???
   to: [(Maybe Address) default: null]
   value: [Quantity] ; in wei
   gasPrice: [Quantity] ; in wei/gas
   gas: [Quantity] ; in gas
   input: [Bytes]))

(define-type TransactionInformation
  (Record
   hash: [Digest]
   nonce: [Quantity]
   blockHash: [(Maybe Digest) default: null]
   blockNumber: [(Maybe Quantity) default: null]
   transactionIndex: [(Maybe Quantity) default: null]
   from: [(Maybe Address) default: null]
   to: [(Maybe Address) default: null]
   value: [Quantity]
   gasPrice: [Quantity]
   gas: [Quantity]
   input: [Bytes]
   v: [(Maybe Quantity) default: null]
   standard-v: [(Maybe Quantity) default: null]
   r: [(Maybe UInt256) default: null]
   s: [(Maybe UInt256) default: null]
   raw: [(Maybe Data) default: null]
   publicKey: [(Maybe PublicKey) default: null]
   networkID: [(Maybe Quantity) default: null]
   creates: [(Maybe Digest) default: null]
   condition: [Json default: null])) ;; is this any JSON, or a TransactionCondition above??

(define-type EthObject
  (Record
   fromBlock: [(Maybe BlockParameter)]
   toBlock: [(Maybe BlockParameter)]
   address: [(Maybe Address) default: null]
   topics: [(Maybe (List Bytes32)) default: null]
   blockhash: [(Maybe Digest) default: null]))

(define-type SignTransactionResult
  (Record
   data: [Bytes]
   signed: [TransactionInformation]))

(define-type LogObject
  (Record
   removed: [Bool]
   logIndex: [(Maybe Quantity) default: null]
   transactionIndex: [(Maybe Quantity) default: null]
   transactionHash: [(Maybe Digest) default: null]
   blockNumber: [(Maybe Quantity) default: null]
   blockHash: [(Maybe Digest) default: null]
   address: [Address]
   data: [Bytes]
   topics: [(List Digest)]))

(define-type Bloom Bytes32)

(define-type TransactionReceipt
  (Record
   blockHash: [Digest]
   blockNumber: [Quantity]
   contractAddress: [(Maybe Address) default: null]
   cumulativeGasUsed: [Quantity]
   from: [Address]
   to: [(Maybe Address) default: null]
   gasUsed: [Quantity]
   logs: [(List LogObject)]
   logsBloom: [Bloom]
   status: [Quantity]
   transactionHash: [Digest]
   transactionIndex: [Quantity]))

(def (Confirmation<-TransactionReceipt tr)
  (defrule (.tr x ...) (begin (def x (.@ tr x)) ...))
  (.tr transactionHash transactionIndex blockNumber blockHash status)
  (if (zero? status)
    (error "receipt indicates transaction failed" transactionHash)
    {(transactionHash) (transactionIndex) (blockNumber) (blockHash)})) ;; Confirmation

(define-type EthListLogObjects (List LogObject))

;; Returns a list of address owned by the client
(define-ethereum-api eth accounts (List Address) <-)


(define-type CallParameters
  (Record
   from: [Address]
   to: [(Maybe Address) default: null]
   gas: [(Maybe Quantity) default: null]
   gasPrice: [(Maybe Quantity) default: null]
   value: [(Maybe Quantity) default: null]
   data: [(Maybe Bytes) default: null]))

(def (CallParameter<-Operation from operation)
  (defvalues (to data) (ToData<-Operation operation))
  {(from) (to) (data) gas: null gasPrice: null value: null}) ;; CallParameter

(def (CallParameter<-PreTransaction pretx)
  (defrule [x ...] (begin (def x (.@ pretx x)) ...))
  [sender operation gas value]
  {from: sender to: (operation-to operation) data: (operation-data operation)
   gasPrice: null (gas) (value)})

(def (CallParameter<-Transaction tx)
  (defrule (.tx x ...) (begin (def x (.@ tx x)) ...))
  (.tx operation tx-header)
  (defrule (.th x ...) (begin (def x (.@ tx-header x)) ...))
  (.th sender gas gasPrice value)
  {(:: @ (CallParameter<-Operation sender operation)) (gas) (gasPrice) (value)})

;; TODO: Looks like the Geth page says it has extension for it?
(define-ethereum-api eth call Data <- CallParameters BlockParameter)

(define-ethereum-api eth chainId (Maybe UInt256) <-)

(def this-chainId (lazy (eth_chainId)))

(def (v-of-chain-id (chainId (force this-chainId)))
  (maybe-get (force this-chainId) 0))

(define-type SignedTx
  (Record
   nonce: [Quantity]
   gasPrice: [Quantity]
   gas: [Quantity]
   to: [(Maybe Address) default: null]
   value: [Quantity]
   input: [Bytes]
   v: [(Maybe UInt256) default: null]
   r: [(Maybe UInt256) default: null]
   s: [(Maybe UInt256) default: null]
   hash: [Digest]))

(define-type SignedTransaction
  (Record
   raw: [Bytes]
   tx: [SignedTx]))

(def (TransactionData<-SignedTransaction st)
  (defrule (.st x ...) (begin (def x (.@ st x)) ...))
  (.st tx)
  (defrule (.tx x ...) (begin (def x (.@ tx x)) ...))
  (.tx nonce gasPrice gas to value input v r s)
  (let ((to (maybe-get to (.@ Address methods zero)))
        (data input)
        (v (v-of-chain-id v))
        (r (maybe-get r 0))
        (s (maybe-get s 0)))
    {(to) (value) (data) (v) (r) (s)}))

;; Returns estimate of gas needed for transaction
(define-ethereum-api eth estimateGas Quantity <- TransactionParameters)

;; Get the current gas price in wei
(define-ethereum-api eth gasPrice Quantity <-)

;; Returns the balance of the account of given address (and block)
(define-ethereum-api eth getBalance Quantity <- Address BlockParameter)

;; Returns the code of given address (and block)
(define-ethereum-api eth getCode Bytes <- Address BlockParameter)

;;;; Returns a transaction (big object) by the hash code
(define-ethereum-api eth getTransaction TransactionResult <- Digest)

;; Returns a transaction (big object) by the hash code *)
(define-ethereum-api eth getTransactionByHash TransactionInformation <- Digest)

;; Returns the number of transaction at address (and transaction)
(define-ethereum-api eth getTransactionCount Quantity <- Address BlockParameter)

;; Returns a receipt of transaction by transaction hash (not available if transaction still pending)
(define-ethereum-api eth getTransactionReceipt TransactionReceipt <- Digest)

;; Create new message call transaction or a contract creation for signed transaction
(define-ethereum-api eth sendRawTransaction Digest <- Data)

;; TODO: Check that it is coherent
;; Get a list of matchings blocks
(define-ethereum-api eth getLogs EthListLogObjects <- EthObject)

;; NB: Not to be used in our code, it's too flaky wrt buyout attacks.
;; Creates new message call transaction or a contract creation if the datafield contains code.
(define-ethereum-api eth sendTransaction Digest <- TransactionParameters)

;; Computes an eth signature
(define-ethereum-api eth sign Data <- Address Data)

;;(define-ethereum-api eth signTransaction SignTransactionResult <- TransactionParameters)

(define-ethereum-api eth blockNumber Quantity <-)


;;;; Geth extensions, Personal Namespace https://geth.ethereum.org/docs/rpc/ns-personal

;; Arguments: (1) SecretKey as hex string, no 0x prefix, (2) passphrase.
(define-ethereum-api personal importRawKey Address <- String String)

(define-ethereum-api personal listAccounts (List Address) <-)

(define-ethereum-api personal lockAccount Bool <- Address) ;; returns true if account found (?)

(define-ethereum-api personal newAccount Address <- String) ;; argument is passphrase

(define-ethereum-api personal unlockAccount Bool <- ;; returns true if found?
  Address
  String ;; passphrase
  (Maybe Real)) ;; duration in seconds (default 300)

(define-ethereum-api personal sendTransaction
  Digest <- TransactionParameters String) ;; passphrase

;;; The sign method calculates an Ethereum specific signature with:
;;; sign(keccack256("\x19Ethereum Signed Message:\n" + len(message) + message))).
;;; TODO: how exactly is the length encoded and separated from the message? Fixed length??? See geth source
(define-ethereum-api personal sign Signature <- String Address String) ;; message address passphrase

;;; Recover the signer of a message signed with personal_sign
(define-ethereum-api personal ecRecover Address <- String Signature) ;; message signature

;; https://github.com/ethereum/go-ethereum/pull/15971/files
(define-ethereum-api personal signTransaction SignedTransaction <- TransactionParameters String)


;; txpool namespace https://geth.ethereum.org/docs/rpc/ns-txpool
(define-type TxPoolEntry
  (Record
   blockHash: [Digest]
   blockNumber: [(Maybe Quantity)]
   from: [Address]
   gas: [Quantity]
   gasPrice: [Quantity]
   hash: [Digest]
   input: [Bytes]
   nonce: [Quantity]
   to: [Address]
   transactionIndex: [(Maybe Quantity)]
   value: [Quantity]))

#;
(define-type TxPoolContent
  (Record
   pending: [(Hash Address -> (Hash Decimal -> TxPoolEntry))] ; Decimal is a Nonce in decimal
   queued: [(Hash Address -> (Hash Decimal -> TxPoolEntry))]))

;; https://github.com/ethereum/go-ethereum/wiki/Management-APIs#txpool-content
#;(define-ethereum-api txpool content TxPoolContent <-)

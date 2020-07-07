;; Ethereum JSON-RPC API
;;
;; The reference documentation for the Ethereum JSON RPC protocol is now at:
;;   https://eth.wiki/json-rpc/API
;; Geth extensions are documented here:
;;   https://geth.ethereum.org/docs/rpc/server
;; We support all non-deprecated methods in the standard protocol as of 2020-07-05,
;; but only a few of the Geth extensions.
;;
;; TODO:
;; - Resolve all the dark spots.
;; - Systematically lift all the Geth extensions.
;; - Add plenty of tests everywhere.
;; - Support multiple eth-like networks from a same client.

(export #t)

(import
  :gerbil/gambit/threads
  (for-syntax :std/format)
  :std/format :std/lazy :std/sugar
  :clan/net/json-rpc
  :clan/utils/json :clan/utils/logger :clan/utils/maybe :clan/utils/path-config
  :clan/poo/poo :clan/poo/brace :clan/poo/io
  (only-in :clan/poo/mop define-type)
  (only-in :clan/poo/number Real)
  (only-in :clan/poo/type List Maybe Unit Or Map Json False)
  ./types ./signing ./ethereum ./config
  )

(def geth-rpc-logger (json-logger "geth-rpc"))

;; Use a mutex for access to geth, not to overload it and get timeouts.
;; have a pool of a small number of connections to geth rather than just one.
(def ethereum-mutex (make-mutex 'ethereum))

(def (ethereum-json-rpc method-name result-decoder param-encoder
                        timeout: (timeout #f) log: (log geth-rpc-logger)
                        params)
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
             (def (fun-id timeout: (timeout #f) log: (log geth-rpc-logger) . a)
                 (ethereum-json-rpc method-name
                                    (.@ result-type .<-json)
                                    (.@ params-type .json<-) (list->vector a)
                                    timeout: timeout log: log))))))))

(define-ethereum-api web3 clientVersion String <-)
(define-ethereum-api web3 sha3 Bytes32 <- Bytes) ;; keccak256

;; "1": Ethereum Mainnet, "3": Ropsten Testnet, "4": Rinkeby Testnet, "42": Kovan Testnet...
(define-ethereum-api net version String <-) ;; a decimal number
(define-ethereum-api net listening Bool <-)
(define-ethereum-api net peerCount Quantity <-)


(define-ethereum-api eth protocolVersion String <-) ;; a decimal number

(define-type SyncingStatus
  (Record startingBlock: [Quantity]
          currentBlock: [Quantity]
          highestBlock: [Quantity]))
(define-ethereum-api eth syncing (Or SyncingStatus False) <-)

(define-ethereum-api eth coinbase Address <-)
(define-ethereum-api eth mining Bool <-)
(define-ethereum-api eth hashrate Quantity <-)



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
   to: [(Maybe Address) optional: #t default: null]
   gas: [(Maybe Quantity) optional: #t default: null] ; in gas
   gasPrice: [(Maybe Quantity) optional: #t default: null] ; in wei/gas
   value: [(Maybe Quantity) optional: #t default: null] ; in wei
   data: [(Maybe Bytes) optional: #t default: null]
   nonce: [(Maybe Quantity) optional: #t default: null]
   condition: [(Maybe TransactionCondition) optional: #t default: null]))

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

(define-type TransactionInformation
  (Record
   hash: [Digest]
   nonce: [Quantity]
   blockHash: [(Maybe Digest) optional: #t default: null]
   blockNumber: [(Maybe Quantity) optional: #t default: null]
   transactionIndex: [(Maybe Quantity) optional: #t default: null]
   from: [(Maybe Address) optional: #t default: null]
   to: [(Maybe Address) optional: #t default: null]
   value: [Quantity]
   gasPrice: [Quantity]
   gas: [Quantity]
   input: [Bytes]
   v: [(Maybe Quantity) optional: #t default: null]
   standard-v: [(Maybe Quantity) optional: #t default: null]
   r: [(Maybe UInt256) optional: #t default: null]
   s: [(Maybe UInt256) optional: #t default: null]
   raw: [(Maybe Data) optional: #t default: null]
   publicKey: [(Maybe PublicKey) optional: #t default: null]
   networkID: [(Maybe Quantity) optional: #t default: null]
   creates: [(Maybe Digest) optional: #t default: null]
   condition: [Json optional: #t default: null])) ;; is this any JSON, or a TransactionCondition above??

(define-type SignTransactionResult
  (Record
   data: [Bytes]
   signed: [TransactionInformation]))

(define-type Bloom Bytes256)

(define-type LogObject
  (Record
   removed: [Bool]
   logIndex: [(Maybe Quantity) optional: #t default: null]
   transactionIndex: [(Maybe Quantity) optional: #t default: null]
   transactionHash: [(Maybe Digest) optional: #t default: null]
   blockNumber: [(Maybe Quantity) optional: #t default: null]
   blockHash: [(Maybe Digest) optional: #t default: null]
   address: [Address]
   data: [Bytes]
   topics: [(List Bytes32)]))
(define-type LogObjectList (List LogObject))

(define-type TransactionReceipt
  (Record
   blockHash: [Digest]
   blockNumber: [Quantity]
   contractAddress: [(Maybe Address) optional: #t default: null]
   cumulativeGasUsed: [Quantity]
   from: [Address]
   to: [(Maybe Address) optional: #t default: null]
   gasUsed: [Quantity]
   logs: [LogObjectList]
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

;; Returns a list of address owned by the client
(define-ethereum-api eth accounts (List Address) <-)


(define-type CallParameters
  (Record
   from: [Address]
   to: [(Maybe Address) optional: #t default: null]
   gas: [(Maybe Quantity) optional: #t default: null]
   gasPrice: [(Maybe Quantity) optional: #t default: null]
   value: [(Maybe Quantity) optional: #t default: null]
   data: [(Maybe Bytes) optional: #t default: null]))

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

(define-type StateOverrideSet ;; contract data to override before executing the call
  (Record
   balance: [Quantity optional: #t]
   nonce: [Quantity optional: #t]
   code: [Bytes optional: #t]
   state: [(Map Bytes32 <- Quantity) optional: #t] ;; keys are 0x quantity
   stateDiff: [(Map Bytes32 <- Quantity) optional: #t])) ;; override individual slots in account storage

;; TODO: Geth has an optional third parameter StateOverrideSet
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
   to: [(Maybe Address) optional: #t default: null]
   value: [Quantity]
   input: [Bytes]
   v: [(Maybe UInt256) optional: #t default: null]
   r: [(Maybe UInt256) optional: #t default: null]
   s: [(Maybe UInt256) optional: #t default: null]
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
  (let ((to (maybe-get to (.@ Address .zero)))
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

;; Returns the content of storage in given contract at given memory position, given block
(define-ethereum-api eth getStorageAt Bytes32 <- Address Quantity BlockParameter)

;; Returns the code of given address (and block)
(define-ethereum-api eth getCode Bytes <- Address BlockParameter)

;; Returns a transaction by the hash code
(define-ethereum-api eth getTransactionByHash TransactionInformation <- Digest)

;; Returns a transaction by block hash and transaction index position
(define-ethereum-api eth getTransactionByBlockHashAndIndex TransactionInformation <- Digest Quantity)

;; Returns a transaction by block height and transaction index position
(define-ethereum-api eth getTransactionByBlockNumberAndIndex TransactionInformation <- BlockParameter Quantity)

;; Returns the number of transaction at address (and transaction)
(define-ethereum-api eth getTransactionCount Quantity <- Address BlockParameter)

;; Returns the number of transactions in a block found by its hash code
(define-ethereum-api eth getTransactionCountByHash Quantity <- Digest)

;; Returns the number of transactions in a block found by its height
(define-ethereum-api eth getTransactionCountByNumber Quantity <- BlockParameter)

;; Returns the number of uncles in a block found by its hash
(define-ethereum-api eth getUncleCountByBlockHash Quantity <- Digest)

;; Returns the number of uncles in a block found by its height
(define-ethereum-api eth getUncleCountByNumber Quantity <- BlockParameter)

;; Returns uncle information
(define-ethereum-api eth getUncleByBlockHashAndIndex BlockInformation <- Digest Quantity)
(define-ethereum-api eth getUncleByBlockNumberAndIndex BlockInformation <- BlockParameter Quantity)



;; Returns a receipt of transaction by transaction hash (not available if transaction still pending)
(define-ethereum-api eth getTransactionReceipt TransactionReceipt <- Digest)

;; Create new message call transaction or a contract creation for signed transaction
(define-ethereum-api eth sendRawTransaction Digest <- Data)

;; NB: Not to be used in our code, it's too flaky wrt various attacks.
;; Creates new message call transaction or a contract creation if the datafield contains code.
(define-ethereum-api eth sendTransaction Digest <- TransactionParameters)

;; Computes an eth signature of (eth-sign-prefix message)
(define-ethereum-api eth sign Data <- Address Data)
(def (eth-sign-prefix message)
  (format "\x19;Ethereum Signed Message:\n~a~a" (string-length message) message))

;; TODO: Which of these is it?
;;(define-ethereum-api eth signTransaction SignTransactionResult <- TransactionParameters)
;;(define-ethereum-api eth signTransaction Bytes <- TransactionParameters)


(define-type BlockInformation
  (Record number: [(Maybe Quantity)]
          hash: [(or Digest null)]
          parentHash: [Digest]
          nonce: [(Maybe Bytes8)]
          sha3Uncles: [Digest]
          logsBloom: [Bloom]
          transactionsRoot: [Digest]
          stateRoot: [Digest]
          receiptsRoot: [Digest]
          miner: [Address]
          difficulty: [Quantity]
          totalDifficulty: [Quantity]
          extraData: [Bytes]
          size: [Quantity]
          gasLimit: [Quantity]
          gasUsed: [Quantity]
          timestamp: [Quantity] ;; unix timestamp
          transactions: [(Or (List TransactionInformation) (List Digest))]
          gasUsed: [Quantity]
          uncles: [(List Digest)]))

;; boolean: true for full tx objects, false for txhashes only
(define-ethereum-api eth getblockByHash (Maybe BlockInformation) <- Digest Bool)
(define-ethereum-api eth getblockByNumber (Maybe BlockInformation) <- BlockParameter Bool)

(define-ethereum-api eth blockNumber Quantity <-)

(define-type newFilterOptions ;; for newFilter
  (Record fromBlock: [BlockParameter optional: #t default: 'latest]
          toBlock: [BlockParameter optional: #t default: 'latest]
          address: [(Or Address (List Address) Unit) optional: #t default: null]
          topics: [(Maybe (List (Maybe (Or Bytes32 (List Bytes32))))) optional: #t default: null]))
(define-type getLogsFilterOptions ;; for getLogs
  (Record fromBlock: [(Maybe BlockParameter) optional: #t default: 'latest]
          toBlock: [(Maybe BlockParameter) optional: #t default: 'latest]
          address: [(Or Address (List Address) Unit) optional: #t default: null]
          topics: [(Maybe (List (Or Bytes32 Unit (List Bytes32)))) optional: #t default: null]
          blockhash: [(Maybe Digest) optional: #t default: null]))

(define-ethereum-api eth newFilter Quantity <- newFilterOptions)
(define-ethereum-api eth newBlockFilter Quantity <-)
(define-ethereum-api eth newPendingTransactionFilter Quantity <-)
(define-ethereum-api eth uninstallFilter Bool <- Quantity)
(define-ethereum-api eth getFilterChanges
  (Or (List Digest) ;; for newBlockFilter (block hashes), newPendingTransactionFilter (tx hashes)
      LogObjectList) ;; for newFilter
  <- Quantity)
(define-ethereum-api eth getFilterLogs
  (Or (List Digest) ;; for newBlockFilter (block hashes), newPendingTransactionFilter (tx hashes)
      LogObjectList) ;; for newFilter
  <- Quantity)
;; TODO: Check that it is coherent
;; Get a list of matchings blocks
(define-ethereum-api eth getLogs LogObjectList <- getLogsFilterOptions)

;; returns: 1. current block header pow-hash, 2. seed hash used for the DAG,
;; 3. boundary condition (“target”), 2^256 / difficulty.
(define-ethereum-api eth getWork (Tuple Bytes32 Bytes32 Bytes32) <-)
(define-ethereum-api eth submitWork Bool <- Bytes32 Bytes32 Bytes32)


(define-ethereum-api shh version String <-)
(define-type ShhMessageSent
  (Record
   from: [(Maybe Bytes60)]
   to: [(Maybe Bytes60)]
   topics: [(List Bytes)]
   payload: [Bytes]
   priority: [Quantity]
   ttl: [Quantity])) ;; time to live in seconds.
(define-ethereum-api shh post Bool <- ShhMessageSent)
(define-ethereum-api shh newIdentity Bytes60 <-)
(define-ethereum-api shh hasIdentity Bool <- Bytes60)
(define-ethereum-api shh newGroup Bytes60 <-)
(define-ethereum-api shh addToGroup Bool <- Bytes60)
(define-type ShhFilter
  (Record
   to: [(Maybe Bytes60)]
   topics: [(List (Or Bytes Unit (List Bytes)))]))
(define-ethereum-api shh newFilter Quantity <- ShhFilter)
(define-ethereum-api shh uninstallFilter Bool <- Quantity)
(define-type ShhMessageReceived
  (Record
   hash: [Digest]
   from: [(Maybe Bytes60)]
   to: [(Maybe Bytes60)]
   expiry: [Quantity] ;; Integer of the time in seconds when this message should expire (?).
   ttl: [Quantity] ;; Integer of the time the message should float in the system in seconds (?).
   sent: [Quantity] ;; Integer of the unix timestamp when the message was sent.
   topics: [(List Bytes)] ;; Array of DATA topics the message contained.
   payload: [Bytes] ;; The payload of the message.
   workProved: [Quantity])) ;; Integer of the work this message required before it was send (?).
(define-ethereum-api shh getFilterChanges (List ShhMessageReceived) <- Quantity)
(define-ethereum-api shh getMessages (List ShhMessageReceived) <- Quantity)



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

;; https://geth.ethereum.org/docs/rpc/pubsub -- we need use websocket for that.
;; eth_subscribe, eth_unsubscribe

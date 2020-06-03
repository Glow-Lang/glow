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
  :clan/utils/maybe
  :clan/net/json-rpc
  :glow/ethereum/types
  )
#|

;; TODO: move to another file.
;; TODO: for end-user reporting, use error contexts as ported from quux or cl-scripting.
;; Maybe also port that to Gerbil main, in a way backward compatible with std/test ?
(def (parse-file file parser (description #f))
  (call-with-input-file file parser))

;; TODO: a wrapper to defstruct that generate a descriptor from which
;; we automatically derive JSON/SEXP/Binary encodings?
(define-type EthereumRpcConfig
  (Record
   scheme: Symbol
   host: String
   port: UInt16))

(def parse-ethereum-rpc-config/sexp read) ;; TODO: autoextract a validating parser

(def current-ethereum-rpc-config (make-parameter #f))

(def (load-ethereum-rpc-config (file (get-config-file "ethereum_config.sexp")))
  (parse-file file parse-ethereum-rpc-config/sexp))

(* Use a mutex for access to geth, not to overload it and get timeouts.
   TODO: have a pool of a small number of connections to geth rather than just one.
   *)
(def ethereum-mutex (make-mutex 'ethereum))

(def (ethereum-json-rpc method-name result-decoder param-encoder
                        timeout: (timeout #f) log: (log #f)
                        params)
  (when +rpc-log+
    (log! "ETH json rpc method-name=%s" method-name))
  (with-lock ethereum-mutex
    (json-rpc ethereum-rpc-config method-name result-decoder param-encoder
              timeout: timeout log: log params)))

(defsyntax (define-ethereum-api stx)
  (syntax-case stx (<-)
    ((_ namespace method result-type <- argument-type ...)
     XXX))) ;; TODO: fill in the blanks

(define-type BlockParameter
  (Union
   Quantity ;; block number as 0x string. In practice, should fit 32-bit
   'latest
   'earliest
   'pending))

(define-type TransactionCondition
  (Union
   (Record (block: Quantity)) ;; block number as 0x string.
   (Record (time: Quantity)) ;; time in seconds-since-epoch as 0x string
   Unit)) ;; JSON null, isomorphic to unit, but its own thing for faithful FFI purposes. #!void in Gerbil?

(define-type TransactionParameters
  (Record
   (from: Address)
   (to: (Maybe Address) default: null)
   (gas: (Maybe Quantity) default: null) ; in gas
   (gasPrice: (Maybe Quantity) default: null) ; in wei/gas
   (value: (Maybe Quantity) default: null) ; in wei
   (data: (Maybe Bytes) default: null)
   (nonce: (Maybe Quantity) default: null)
   (condition: (Maybe TransactionCondition) default: null)))

(def ToData<-Operation
  (match <>
    ((TransferTokens recipient) (values recipient null))
    ((CreateContract code) (values null code))
    ((CallFunction recipient data) (values recipient data))))

(def (Transactionparameters<-Operation sender operation value)
  (defvalues (to data) (ToData<-Operation operation))
  (TransactionParameters$
   from: sender to: to gas: null gasPrice: null
   value: value data: data nonce: null condition: null))

(def (Transactionparameters<-PreTransaction sender pretx)
  (match pretx
    ((PreTransaction$ operation gas value) ; PreTransaction
     (record+ (TransactionParameters<-Operation sender operation value) (record gas value)))))

(def TransactionParameters<-Transaction
  (match <>
    ((Transaction$ tx-header: (TxHeader$ sender nonce gas gasPrice value) operation)
     (record+ (TransactionParameters<-Operation sender operation value)
              (record gas gasPrice value nonce)))))

(define-type TransactionResult
  (Record
   (hash: Digest)
   (nonce: Quantity)
   (blockHash: (Maybe Digest) default: null)
   (blockNumber: (Maybe Quantity) default: null)
   (transactionIndex: (Maybe Quantity) default: null)
   (from: (Maybe Address) default: default) ;; TODO: can it actually be null???
   (to: (Maybe Address) default: null)
   (value: Quantity) ; in wei
   (gasPrice: Quantity) ; in wei/gas
   (gas: Quantity) ; in gas
   (input: Bytes)))

(define-type TransactionInformation
  (Record
   (hash: Digest)
   (nonce: Nonce)
   (blockHash: (Maybe Digest) default: null)
   (blockNumber: (Maybe Quantity) default: null)
   (transactionIndex: (Maybe Quantity) default: null)
   (from: (Maybe Address) default: null)
   (to: (Maybe Address) default: null)
   (value: Quantity)
   (gasPrice: Quantity)
   (gas: Quantity)
   (input: Bytes)
   (v: (Maybe Quantity) default: null)
   (standard-v: (Maybe Quantity) default: null)
   (r: (Maybe UInt256) default: null)
   (s: (Maybe UInt256) default: null)
   (raw: (Maybe Data) default: null)
   (publicKey: (Maybe PublicKey) default: null)
   (networkID: (Maybe Quantity) default: null)
   (creates: (Maybe Digest) default: null)
   (condition: Json option default: null))) ;; is this any JSON, or a TransactionCondition above???

(define-type EthObject
  (Record
   (fromBlock: (Maybe BlockParameter))
   (toBlock: (Maybe BlockParameter))
   (address: (Maybe Address) default: null)
   (topics: (Maybe (List Bytes)) default: null) ;; Bytes32 ??? Digest ???
   (blockhash: (Maybe Digest) default: null)))

(define-type ParitySignedTransaction
  (Record
   (raw: Bytes)
   (tx: TransactionInformation)))

(define-type LogObject
  (Record
   (removed: Bool)
   (logIndex: (Maybe Quantity) default: null)
   (transactionIndex: (Maybe Quantity) default: null)
   (transactionHash: (Maybe Digest) default: null)
   (blockNumber: (Maybe Quantity) default: null)
   (blockHash: (Maybe Digest) default: null)
   (address: Address)
   (data: Bytes)
   (topics: (List Digest))))

(define-type Bloom Bytes32)

(define-type TransactionReceipt
  (Record
   (blockHash: Digest)
   (blockNumber: Quantity)
   (contractAddress: (Maybe Address) default: null)
   (cumulativeGasUsed: Quantity)
   (from: Address)
   (to: (Maybe Address) default: null)
   (gasUsed: Quantity)
   (logs: (List LogObject))
   (logsBloom: Bloom)
   (status: Quantity)
   (transactionHash: Digest)
   (transactionIndex: Quantity)))

(def Confirmation<-TransactionReceipt
  (match <>
    ((TransactionReceipt$ transactionHash transactionIndex blockNumber blockHash status)
     (if (zero? status)
       (error "receipt indicates transaction failed" transactionHash)
       (Confirmation$ transactionHash transactionIndex blockNumber blockHash)))))

(define-type EthListLogObjects (List LogObject))

;; Returns a list of address owned by the client
(define-ethereum-api eth accounts Address <-)


(define-type CallParameters
  (Record
   (from: Address)
   (to: (Maybe Address) default: null)
   (gas: (Maybe Quantity) default: null)
   (gasPrice: (Maybe Quantity) default: null)
   (value: (Maybe Quantity) default: null)
   (data: (Maybe Bytes) default: null)))

(define-type (CallParameter<-Operation sender operation)
  (defvalues (to data) (ToData<-Operation operation))
  (CallParameter$ from: sender to gas: null gasPrice: null value: null data))

(def (CallParameter<-PreTransaction sender pretx)
  (match pretx
    ((PreTransaction$ operation gas value)
     (record+ (Pretransaction<-Operation sender operation)
              (record gas value)))))

(def CallParameter<-Transaction
  (match <>
    ((Transaction$ tx-header: (TxHeader$ sender gas gasPrice value) operation)
     (record+ (CallParameter<-Operation sender operation)
              (record gas gasPrice value)))))

;; TODO: Looks like the Geth page says it has extension for it?
(define-ethereum-api eth call Data <- CallParameters BlockParameter)

(define-ethereum-api eth chainId (Maybe UInt256) <-)

(def this-chainId (lazy (eth_chainId)))

(def (v-of-chain-id (chainId (this-chain-id)))
  (maybe-get (force this-chainId) 0))

(def SignedTx
  (Record
   (nonce: Quantity)
   (gasPrice: Quantity)
   (gas: Quantity)
   (to: (Maybe Address) default: null)
   (value: Quantity)
   (input: Bytes)
   (v: (Maybe UInt256) default: null)
   (r: (Maybe UInt256) default: null)
   (s: (Maybe UInt256) default: null)
   (hash: Digest)))

(def SignedTransaction
  (Record
   (raw: Bytes)
   (tx: SignedTx)))

(def TransactionData<-SignedTransaction
  (match <>
    ((SignedTransaction$ tx: (SignedTx$ nonce gasPrice gas to value input v r s))
     (SignedTransactionData$
      nonce
      gasPrice
      gas
      to: (maybe-get to (Address<-nat 0))
      value
      data: input
      v: (v-of-chain-id v)
      r: (maybe-get r 0)
      s: (maybe-get s 0)))))

;; Returns estimate of gas needed for transaction
(define-ethereum-api eth estimateGas Quantity <- TransactionParameters)

;; Get the current gas price in wei
(define-ethereum-api eth gasPrice Quantity <-)

;; Returns the balance of the account of given address (and block)
(define-ethereum-api eth getBalance Quantity <- Address BlockParameter)

;; Returns the code of given address (and block)
(define-ethereum-api eth getCode Bytes <- Address BlockParameter)

;;;; Returns a transaction (big object) by the hash code
;;(define-ethereum-api eth getTransaction TransactionResult <- Digest)

;; Returns a transaction (big object) by the hash code *)
;;(define-ethereum-api eth getTransactionByHash TransactionInformation <- Digest)

;; Returns the number of transaction at address (and transaction)
(define-ethereum-api eth getTransactionCount Quantity <- Address BlockParameter)

;; Returns a receipt of transaction by transaction hash (not available if transaction still pending)
(define-ethereum-api eth getTransactionReceipt TransactionReceipt <- Digest)

;; Create new message call transaction or a contract creation for signed transaction
(define-ethereum-api eth-sendRawTransaction Digest <- Data)

;; TODO: Check that it is coherent
;; Get a list of matchings blocks
(define-ethereum-api eth getLogs EthListLogObjects <- EthObject)

;; NB: Not to be used in our code, it's too flaky wrt buyout attacks.
;; Creates new message call transaction or a contract creation if the datafield contains code.
(define-ethereum-api eth-sendTransaction Digest <- TransactionParameters)

;; Computes an eth signature
(define-ethereum-api eth sign Data <- Address Data)

;; ???
(define-ethereum-api eth-signTransaction ParitySignedTransaction <- TransactionParameters)

(define-ethereum-api eth blockNumber Quantity <-)


;;;; Geth extensions, Personal Namespace https://geth.ethereum.org/docs/rpc/ns-personal

;; PrivateKey is in hex, no 0x (??? double check that). The String is a passphrase
(define-ethereum-api personal importRawKey Address <- PrivateKey String)

(define-ethereum-api personal listAccounts (List Address) <-)

(define-ethereum-api personal lockAccount Bool <- Address) ;; returns true if account found (?)

(define-ethereum-api personal newAccount Address <- String) ;; argument is passphrase

(define-ethereum-api personal unlockAccount Bool <- ;; returns true if found?
  Address
  String ;; passphrase
  (Maybe Real)) ;; duration in seconds (default 300)

(define-ethereum-api personal sendTransaction
  SignedTransaction <- TransactionParameters String) ;; passphrase

;;; The sign method calculates an Ethereum specific signature with:
;;; sign(keccack256("\x19Ethereum Signed Message:\n" + len(message) + message))).
;;; TODO: how exactly is the length encoded and separated from the message? Fixed length??? See geth source
(define-ethereum-api personal sign Signature <- String Address String) ;; message address passphrase

;;; Recover the signer of a message signed with personal_sign
(define-ethereum-api personal ecRecover Address <- String Signature) ;; message signature



;; txpool namespace https://geth.ethereum.org/docs/rpc/ns-txpool
(define-type TxPoolEntry
  (Record
   (blockHash: Digest)
   (blockNumber: (Maybe Quantity))
   (from: Address)
   (gas: Quantity)
   (gasPrice: Quantity)
   (hash: Digest)
   (input: Bytes)
   (nonce: Nonce)
   (to: Address)
   (transactionIndex: (Maybe Quantity))
   (value: Quantity)))

(define-type TxPoolContent
  (Record
   (pending: (Hash Address -> (Hash Decimal -> TxPoolEntry))) ; Decimal is a Nonce in decimal
   (queued: (Hash Address -> (Hash Decimal -> TxPoolEntry)))))

(* https://github.com/ethereum/go-ethereum/wiki/Management-APIs#txpool-content *)
(define-ethereum-api txpool content TxPoolContent <-)
|#
(export #t)

(import
  :std/sugar
  :clan/utils/maybe
  :clan/poo/poo :clan/poo/brace (only-in :clan/poo/mop Type.)
  ./types ./hex)

;; TODO: implement and use a "newtype"
(define-type Quantity UInt256)
(define-type UInt UInt256)
(define-type Digest Bytes32)
(define-type Data Bytes)
(define-type Address
  {(:: @ Bytes20)
   ethabi: "address"
   methods: =>.+ {.json<-: 0x<-address}})

;; TODO: mixin the prototype with a formula that caches the abi bytes4 selector.
(define-type EthFunction
  (.mix (Record contract: [Address] selector: [Bytes4])
        {ethabi: "function"}))


(def one-ether-in-wei (expt 10 18)) ;; 1 ETH = 10^18 wei
(def one-gwei-in-wei (expt 10 9)) ;; 1 gwei = 10^9 wei

(define-type Confirmation
  (Record
   transactionHash: [Digest]
   transactionIndex: [Quantity]
   blockNumber: [Quantity]
   blockHash: [Digest]))

(define-type TxHeader
  (Record
   sender: [Address]
   nonce: [Quantity]
   gasPrice: [Quantity] ;; in wei per gas
   gas: [Quantity] ;; upper limit, there will be a refund
   value: [Quantity])) ;; in wei}

;; Three kinds of operations that may be posted
(defstruct $Operation ())
(defstruct (TransferTokens $Operation)
  (to) ;; Address
  transparent: #t)
(defstruct (CreateContract $Operation)
  (data) ;; Bytes
  transparent: #t)
(defstruct (CallFunction $Operation)
  (to ;; Address
   data) ;; Bytes
  transparent: #t)
(def operation-to
  (match <>
    ((TransferTokens to) to)
    ((CreateContract _) #f)
    ((CallFunction to _) to)))
(def operation-data
  (match <>
    ((TransferTokens _) #f)
    ((CreateContract data) data)
    ((CallFunction _ data) data)))
(define-type Operation
  {(:: @ Type.)
   .element?: $Operation?
   methods: =>.+ {
     .json<-: (match <>
                ((TransferTokens to) (hash ("to" (json<- Address to))))
                ((CreateContract data) (hash ("data" (json<- Bytes data))))
                ((CallFunction to data) (hash ("to" (json<- Address to)) ("data" (json<- Bytes data)))))
     .<-json: (lambda (h)
                (def to (map/maybe (cut <-json Address <>) (hash-get h "to")))
                (def data (map/maybe (cut <-json Bytes <>) (hash-get h "data")))
                (cond
                 ((and to (not data)) (TransferTokens to))
                 ((and data (not to)) (CreateContract data))
                 (else (CallFunction to data))))
   }})

(define-type PreTransaction
  (Record
   operation: [Operation]
   value: [Quantity] ;; in wei
   gas: [Quantity]))

;; Transaction (to be) posted to the chain Ethereum
;; TODO: merge the fields of tx-header and operation instead, just with a new type tag.
(define-type Transaction
  (Record
   tx-header: [TxHeader]
   operation: [Operation]))

(def (PreTransaction<-Transaction tx)
  {operation: (.@ tx operation)
   value: (.@ tx tx-header value)
   gas: (.@ tx tx-header gas)})

(define-type SignedTransactionData
  (Record
   nonce: [Quantity]
   gasPrice: [Quantity]
   gas: [Quantity]
   to: [Address]
   value: [Quantity]
   data: [Bytes]
   v: [Quantity] ;; actually UInt8... plus offset from chainId!
   r: [Quantity] ;; UInt256
   s: [Quantity])) ;; UInt256

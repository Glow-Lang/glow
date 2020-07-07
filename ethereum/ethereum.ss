(export #t)

(import
  :std/sugar
  :clan/utils/maybe
  :clan/poo/poo :clan/poo/io :clan/poo/brace
  (prefix-in :clan/poo/type poo.)
  ./types ./hex)

;; TODO: implement and use a "newtype"
(define-type Quantity UInt256)
(define-type UInt UInt256)
(define-type Digest Bytes32)
(define-type Data Bytes)
(define-type Address
  {(:: @ Bytes20)
   ethabi: "address"
   .json<-: 0x<-address
   .sexp<-: (lambda (x) `(address<-0x ,(0x<-address x)))
  })
(register-simple-eth-type Address)

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
  {(:: @ [poo.methods.string&bytes&marshal<-json Type.])
   .element?: $Operation?
   .String: String
   .sexp<-: (match <>
              ((TransferTokens to) ['TransferTokens (sexp<- Address to)])
              ((CreateContract data) ['CreateContract (sexp<- Bytes data)])
              ((CallFunction to data) ['CallFunction (sexp<- Address to) (sexp<- Bytes data)]))
   .json<-: (match <>
              ((TransferTokens to) (hash ("to" (json<- Address to))))
              ((CreateContract data) (hash ("data" (json<- Bytes data))))
              ((CallFunction to data) (hash ("to" (json<- Address to)) ("data" (json<- Bytes data)))))
   .<-json: (lambda (h)
              (def to (map/maybe (.@ Address .<-json) (hash-ref h "to" null)))
              (def data (map/maybe (.@ Bytes .<-json) (hash-ref h "data" null)))
              (cond
               ((and to (eq? data null)) (TransferTokens to))
               ((and data (eq? to null)) (CreateContract data))
               (else (CallFunction to data))))
   })

(define-type PreTransaction
  (Record
   sender: [Address]
   operation: [Operation]
   value: [Quantity] ;; in wei
   gas: [Quantity])) ;; in gas

;; Transaction (to be) posted to the chain Ethereum
;; TODO: merge the fields of tx-header and operation instead, just with a new type tag.
(define-type Transaction
  (Record
   tx-header: [TxHeader]
   operation: [Operation]))

(def (PreTransaction<-Transaction tx)
  {sender: (.@ tx tx-header sender)
   operation: (.@ tx operation)
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

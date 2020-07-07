;; Somewhat higher-level wrappers around the basic functionality in ./json-rpc
(export #t)

(import
  :std/error :std/sugar :std/text/hex
  :clan/utils/assert :clan/utils/failure :clan/utils/option
  :clan/net/json-rpc
  :clan/poo/poo :clan/poo/io :clan/poo/brace
  :clan/runtime/db
  ./hex ./ethereum ./config ./signing ./known-addresses ./types ./json-rpc ./nonce-tracker)

(defstruct (TransactionRejected exception) (receipt))
(defstruct (StillPending exception) ())
(defstruct (ReplacementTransactionUnderpriced exception) ())
(defstruct (IntrinsicGasTooLow exception) ())

;; : Unit <- Address timeout:?(OrFalse Real) log:?(Fun Unit <- Json)
(def (ensure-eth-signing-key timeout: (timeout #f) log: (log #f) address)
  (when log (log "ethereum-transaction: ensure-secret-key ~a" (0x<-address address)))
  (def keypair (keypair<-address address))
  (unless keypair
    (error "No registered keypair for address" 'ensure-eth-signing-key address))
  (try
   (personal_importRawKey (hex-encode (keypair-secret-key keypair)) (keypair-password keypair)
                          timeout: timeout log: log)
   (catch (json-rpc-error? e)
     (unless (equal? (json-rpc-error-message e) "account already exists")
       (raise e)))))

;; : Address <-
(def (get-first-account) (car (personal_listAccounts)))

;; : Unit <- Address duration:?Real log:?(Fun Unit <- Json)
(def (unlock-account address duration: (duration 5) log: (log #f))
  (when log (log "ethereum-transaction: unlock-account ~a ~a" address duration))
  (def keypair (keypair<-address address))
  (personal_unlockAccount address (keypair-password keypair) duration))

;; : Bool <- TransactionReceipt
(def (successful-receipt? receipt)
  (and (poo? receipt)
       (= (.@ receipt status) 1)))

;; : Unit <- TransactionReceipt
(def (check-transaction-receipt-status receipt)
  (unless (successful-receipt? receipt)
    (raise (TransactionRejected receipt))))

;; Count the number of confirmations for a transaction given by its hash.
;; Return -1 if the transaction is (yet) unconfirmed, -2 if it is failed.
;; : Integer <- TransactionReceipt
(def (confirmations<-receipt receipt (block-number (eth_blockNumber)))
  (cond
   ((successful-receipt? receipt)
    (- block-number (.@ receipt blockNumber)))
   ((poo? receipt)
    -2)
   (else -1)))

;; Count the number of confirmations for a transaction given by its hash.
;; Return -1 if the transaction is yet unconfirmed.
;; : Integer <- TxHash
(def (confirmations<-transaction txHash)
  (confirmations<-receipt (eth_getTransactionReceipt txHash)))

;; : Bool <- TransactionReceipt Quantity
(def (receipt-sufficiently-confirmed? receipt (block-number (eth_blockNumber)))
  (>= (confirmations<-receipt receipt block-number)
      (.@ (current-ethereum-config) confirmationsWantedInBlocks)))

;; Get the number of the latest confirmed block
;; : Quantity <-
(def (confirmed-block-number (block-number (eth_blockNumber)))
  (- block-number (.@ (current-ethereum-config) confirmationsWantedInBlocks)))

;; : Unit <- TransactionReceipt
(def (check-receipt-sufficiently-confirmed receipt (block-number (eth_blockNumber)))
  (unless (receipt-sufficiently-confirmed? receipt block-number)
    (raise (StillPending))))

;; Given a putative sender, some transaction data, and a confirmation,
;; make sure that it all matches.
;; TODO: Make sure we can verify the confirmation from the Ethereum contract,
;; by checking the merkle tree and using e.g. Andrew Miller's contract to access old
;; block hashes https://github.com/amiller/ethereum-blockhashes
;; : Unit <- sender: Address recipient: (or Address Unit) SignedTransactionData Confirmation
(def (check-transaction-confirmation sender: sender recipient: recipient txdata confirmation)
  ;; TODO: Use RLP marshaling for SignedTransactionData, then we can just check the hash ?
  (def hash (.@ confirmation transactionHash))
  (unless (equal? recipient (.@ txdata to))
    (error "Invalid transaction confirmation" "Recipient does not match Transaction data"))
  (unless (equal? (.@ txdata digest) hash)
    (error "Malformed request" "Transaction data digest does not match the provided confirmation"))
  (def receipt (eth_getTransactionReceipt hash))
  (check-transaction-receipt-status receipt)
  (unless (receipt-sufficiently-confirmed? receipt)
    (raise (StillPending)))
  (unless (and (equal? sender (.@ receipt from))
               (equal? recipient (.@ receipt to))
               (equal? hash (.@ receipt transactionHash))
               (<= (.@ receipt gasUsed) (.@ txdata gas)))
    (error "receipt doesn't match transaction sent"))
  (unless (and (equal? (.@ confirmation transactionHash) hash)
               (equal? (.@ confirmation transactionIndex) (.@ receipt transactionIndex))
               (equal? (.@ confirmation blockNumber) (.@ receipt blockNumber))
               (equal? (.@ confirmation blockHash) (.@ receipt blockHash)))
    (error "confirmation doesn't match transaction information")))

;; : SignedTransaction <- Transaction
(def (sign-transaction transaction)
  (def address (.@ transaction tx-header sender))
  (def kp (keypair<-address address))
  (unless kp (error "Couldn't find registered keypair" (json<- Address address)))
  (personal_signTransaction (TransactionParameters<-Transaction transaction) (keypair-password kp)))

;; : TransactionReceipt <- Address Digest
(def (confirmed-or-known-issue sender hash)
  (cond
   ((eth_getTransactionReceipt hash) => check-transaction-receipt-status)
   (else (nonce-too-low sender))))

;; : TxHeader <- Address Quantity Quantity
(def (make-tx-header sender: sender value: value gas: gas
                     log: (log #f))
  ;; TODO: get gas price and nonce from geth
  (def gasPrice (eth_gasPrice))
  (def nonce (.call NonceTracker next sender))
  {(sender) (nonce) (gasPrice) (gas) (value)})

;; Prepare a signed transaction, that you may later issue onto Ethereum network,
;; from given address, with given operation, value and gas-limit
;; : Transaction SignedTransaction <- PreTransaction
(def (make-signed-transaction pre)
  (def tx-header (make-tx-header sender: (.@ pre sender) value: (.@ pre value) gas: (.@ pre gas)))
  (sign-transaction {(tx-header) operation: (.@ pre operation)}))

;; : Digest <- Address SignedTransaction
(def (send-raw-transaction sender signed)
  (def data (.@ signed raw))
  (def hash (.@ signed tx hash))
  (match (with-result (eth_sendRawTransaction data))
    ((some transaction-hash)
     (if (equal? transaction-hash hash)
       hash
       (error "eth-send-raw-transaction: invalid hash" transaction-hash hash)))
    ((failure (json-rpc-error code: -32000 message: "nonce too low"))
     (confirmed-or-known-issue sender hash)
     hash)
    ((failure (json-rpc-error code: -32000 message: "replacement transaction underpriced"))
     (raise (ReplacementTransactionUnderpriced)))
    ((failure (json-rpc-error code: -32000 message: "intrinsic gas too low"))
     (raise (IntrinsicGasTooLow)))
    ((failure (json-rpc-error code: -32000 message:
                              (? (let (m (string-append "known transaction: " (hex-encode hash)))
                                   (cut equal? <> m)))))
     hash)
    ((failure e)
     (raise e))))

;; : TransactionReceipt <- Transaction SignedTransaction
(def (send-and-confirm-transaction sender signed)
  (def hash (send-raw-transaction sender signed))
  (assert-equal! hash (.@ signed tx hash))
  (def receipt (eth_getTransactionReceipt hash))
  (if receipt
    (check-transaction-receipt-status receipt)
    (let ((nonce (.@ signed tx nonce)))
      (def sender-nonce (eth_getTransactionCount sender 'latest))
      (if (< nonce sender-nonce)
        (confirmed-or-known-issue sender hash)
        (raise (StillPending)))))
  (check-receipt-sufficiently-confirmed receipt)
  receipt)

;; Gas used for a transfer transaction. Hardcoded value defined in the Yellowpaper.
;; : Quantity
(def transfer-gas-used 21000)

;; : PreTransaction <- Address Quantity
(def (transfer-tokens from: sender to: recipient value: value)
  {(sender)
   operation: (TransferTokens recipient)
   (value)
   gas: transfer-gas-used})

;; : PreTransaction <- Address Operation Quantity gas: ?(Maybe Quantity)
(def (make-pre-transaction sender operation value gas: (gas #f))
  (def gas-limit (or gas
                     (eth_estimateGas (TransactionParameters<-Operation sender operation value))))
  ;; TODO: The multiplication by 2 is a hack that needs to be addressed
  (def gas-limit-n-fold (* 2 gas-limit))
  {(sender) (operation) (value) gas: gas-limit-n-fold})

;; : PreTransaction <- Address Bytes value: ?Quantity gas: ?(Maybe Quantity)
(def (create-contract sender code value: (value 0) gas: (gas #f))
  (make-pre-transaction sender (CreateContract code) value gas: gas))

;; : PreTransaction <- Address Address Bytes Quantity gas: ?(Maybe Quantity)
(def (call-function sender contract call value: (value 0) gas: (gas #f))
  (make-pre-transaction sender (CallFunction contract call) value gas: gas))

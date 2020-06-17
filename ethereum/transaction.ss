;; Somewhat higher-level wrappers around the basic functionality in ./json-rpc
(export #t)

(import
  :std/sugar :std/text/hex
  :clan/utils/option
  :clan/utils/failure
  :clan/net/json-rpc
  :clan/poo/poo :clan/poo/brace
  ./ethereum ./config ./signing ./types ./json-rpc ./known-addresses ./hex)

(def (ensure-secret-key timeout: (timeout #f) log: (log #f) keypair)
  (when log (log "ethereum-transaction: ensure-secret-key ~a" (0x<-bytes (keypair-address keypair))))
  (try
   (personal_importRawKey (hex-encode (keypair-secret-key keypair)) (keypair-password keypair)
                          timeout: timeout log: log)
   (catch (json-rpc-error? e)
     (if (equal? (json-rpc-error-message e) "account already exists")
       (keypair-address keypair)
       (raise e)))))

(def (ensure-eth-signing-address timeout: (timeout #f) log: (log #f) address)
  (def keypair (keypair<-address address))
  (unless keypair
    (error "No registered keypair for address" 'ensure-eth-signing-address address))
  (def actual-address (ensure-secret-key timeout: timeout log: log keypair))
  (unless (equal? actual-address address)
    (error "registered keypair has mismatched address" address actual-address)))

(def (get-first-account) (car (personal_listAccounts)))

(def (unlock-account address duration: (duration 5) log: (log #f))
  (when log (log "ethereum-transaction: unlock-account ~a ~a" address duration))
  (def keypair (keypair<-address address))
  (personal_unlockAccount address (keypair-password keypair) duration))

;; TODO: create exception classes for transaction rejected, still pending.

;; : Bool <- TransactionReceipt
(def (successful-receipt? receipt)
  (and (poo? receipt)
       (= (.@ receipt status) 1)))

;; : <- TransactionReceipt
(def (check-transaction-receipt-status receipt)
  (unless (successful-receipt? receipt)
    (error "Transaction Rejected" receipt)))

(def (receipt-sufficiently-confirmed? receipt block-number)
  (>= block-number
      (+ (.@ receipt blockNumber)
         (.@ (current-ethereum-config) confirmationsWantedInBlocks))))

(def (check-receipt-sufficiently-confirmed receipt)
  (unless (receipt-sufficiently-confirmed? receipt (eth_blockNumber))
    (error "Still Pending")))

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
  (def txinfo (eth_getTransactionByHash hash))
  (unless (and (equal? sender (.@ txinfo from))
               (equal? recipient (.@ txinfo to))
               (equal? recipient (.@ txdata to))
               (equal? (.@ txinfo nonce) (.@ txdata nonce))
               (equal? (.@ txinfo value) (.@ txdata value))
               (equal? (.@ txinfo gasPrice) (.@ txdata gasPrice))
               (equal? (.@ txinfo gas) (.@ txdata gas))
               (equal? (.@ txinfo input) (.@ txdata data)))
    (error "Invalid transaction confirmation" "transaction information doesn't match provided data"))
  (unless (and (equal? (.@ confirmation transactionHash) (.@ txinfo hash))
               (equal? (.@ confirmation transactionIndex) (.@ txinfo transactionIndex))
               (equal? (.@ confirmation blockNumber) (.@ txinfo blockNumber))
               (equal? (.@ confirmation blockHash) (.@ txinfo blockHash)))
    (error "Invalid transaction confirmation" "confirmation doesn't match transaction information"))
  (def receipt (eth_getTransactionReceipt hash))
  (unless (poo? receipt)
    (error "Invalid-transaction-confirmation" "Transaction not included in the blockchain"))
  (unless (receipt-sufficiently-confirmed? receipt (eth_blockNumber))
    (error "Invalid-transaction-confirmation" "Transaction still pending"))
  (unless (equal? sender (.@ receipt from))
    (error "Invalid transaction confirmation" "Transaction sender doesn't match")))

;; SignedTransaction <- Transaction
(def (sign-transaction transaction)
  (def address (.@ transaction tx-header sender))
  (def kp (keypair<-address address))
  (unless kp (error "Couldn't find registered keypair" (json<- Address address)))
  (personal_signTransaction (TransactionParameters<-Transaction transaction) (keypair-password kp)))

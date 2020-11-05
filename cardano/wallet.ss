; Derived from https://input-output-hk.github.io/cardano-wallet/api/edge/

(export #t)

(import
  :clan/json :clan/logger
  :gerbil/gambit/ports :gerbil/gambit/threads
  :std/logger :std/misc/ports :std/misc/shuffle :std/net/request :std/srfi/1 :std/text/json
  ./util)

;; CARDANO WALLET

;; WALLETS
(defstruct wallet (id address-pool-gap balance delegation name passphrase state tip) transparent: #t)

; POST /wallets
(def (create-wallet name mnemonic-sentence passphrase mnemonic-second-factor: (mnemonic-second-factor #f) address-pool-gap: (address-pool-gap #f))
  (let (data (list->json-string
    (list
      ["name" . name]
      ["mnemonic_sentence" . mnemonic-sentence]
      ["passphrase" . passphrase])))
    (webserver-post (make-url "/wallets") data)))

; GET /wallets
(def (list-wallets)
  (webserver-get (make-url "/wallets")
    response-parser: (cut map (cut hash-get <> 'id) <>)))

; GET /wallets/{walletId}
(def (get-wallet id)
  (webserver-get (make-url (string-join "/wallets/" id))))

; GET /wallets/{walletId}/statistics/utxos
; DELETE /wallets/{walletId}
; PUT /wallets/{walletId}
; PUT /wallets/{walletId}/passphrase

;; ADDRESSES
(defstruct address (id state) transparent: #t)

(def (hash-table->address a-hash-table)
  (make-address (hash-get a-hash-table 'id) (hash-get a-hash-table 'state)))

; GET /wallets/{walletId}/addresses
(def (list-addresses wallet-id state: (state #f))
  (webserver-get (string-append "/wallets/" wallet-id "/addresses")
    params: (if state (list ["state" . state]) '())
    response-parser: (cut map (cut hash-table->address <>) <>)))

; /addresses/{addressId}

;; COIN SELECTION
; POST /wallets/{walletId}/coin-selections/random

;; TRANSACTIONS

(defstruct payment (address amount) transparent: #t)

(def (payment->hash-table a-payment)
  (with ((payment address amount) a-payment)
    (let (amount-hash-table (amount->hash-table amount))
      (list->hash-table
        (list
          ["address" . address]
          ["amount" . amount-hash-table])))))

(defstruct amount (quantity unit) transparent: #t)

(def (amount->hash-table an-amount)
  (with ((amount quantity unit) an-amount)
    (list->hash-table
      (list
        ["quantity" . quantity]
        ["unit" . unit]))))

; POST /wallets/{walletId}/payment-fees
(def (estimate-transaction-fee wallet-id payments withdrawal: (withdrawal #f) metadata: (metadata #f))
  (let*
    ((payment-hash-tables (map (cut payment->hash-table <>) payments))
     (data (list->json-string (list
      ["payments" . payment-hash-tables]))))
    (webserver-post (make-url (string-append "/wallets/" wallet-id "/payment-fees")) data)))


; POST /wallets/{walletId}/transactions
(def (create-transaction wallet-id payments passphrase withdrawal: (withdrawal #f) metadata: (metadata #f))
  (let*
    ((payment-hash-tables (map (cut payment->hash-table <>) payments))
     (data (list->json-string (list
      ["payments" . payment-hash-tables]
      ["passphrase" . passphrase]))))
    (webserver-post (string-append "/wallets/" wallet-id "/transactions") data)))

; GET /wallets/{walletId}/transactions
(def (list-transactions wallet-id)
  (webserver-get (string-append "/wallets/" wallet-id "/transactions")))

; GET /wallets/{walletId}/transactions/{transactionId}
(def (get-transaction wallet-id transaction-id)
  (webserver-get (string-append "/wallets/" wallet-id "/transactions/")))

; DELETE /wallets/{walletId}/transactions/{transactionId}



(def (confirm-transaction wallet-id transaction-id)
  (thread-start! (make-thread (lambda ()
    (let poll ()
      (let* ((transaction-info (get-transaction wallet-id transaction-id))
             (transaction-status (hash-get transaction-info 'status)))
        (wallet-logger transaction-status)
        (if (string=? "in_ledger" transaction-status)
            (thread-yield!)
            (begin (thread-sleep! 10) (poll)))))))))

; NODE CONFIG
(def wallet-logger (json-run-logger "wallet"))

(def (make-url path)
  (string-append base-url path))

(defvalues (base-url)
  (values "127.0.0.1:3002/v2"))

; Derived from https://docs.cardano.org/projects/cardano-node/en/latest/stake-pool-operations/keys_and_addresses.html
;; CARDANO CLI
(defstruct payment-keys (public-key private-key) transparent: #t)
(defstruct key (type description cbor-hex) transparent: #t)

(def (json-object->key json-key)
  (make-key (hash-get json-key 'type) (hash-get json-key 'description) (hash-get json-key 'cborHex)))

(def (create-payment-keys)
  (let ((public-key-file "/tmp/payment.publickey")
        (private-key-file "/tmp/payment.privatekey"))
    (call-with-input-process
      [path: "cardano-cli"
       arguments: ["shelley" "address" "key-gen"
          "--verification-key-file" public-key-file
          "--signing-key-file" private-key-file]]
      (lambda (process)
        (read process)
        (let ((public-key (json-object->key (read-file-json public-key-file)))
              (private-key (json-object->key (read-file-json private-key-file))))
          (make-payment-keys public-key private-key))))))

(def (create-payment-address payment-keys)
  (let ((public-key-file "/tmp/payment.publickey")
        (output-file "/tmp/payment.address"))
    (call-with-input-process
      [path: "cardano-cli"
       arguments: ["shelley" "address" "build"
          "--payment-verification-key-file" public-key-file
          "--out-file" output-file
          "--mainnet"]]
      (lambda (process)
        (read process)
        (read-file-string output-file)))))


(export #t)

(import
  :std/sugar
  :clan/utils/json :clan/utils/path-config
  :clan/poo/poo :clan/poo/brace :clan/poo/io
  :clan/runtime/db
  ./types ./ethereum ./json-rpc ./transaction)

(define-type ContractConfig
  (Record
   contract_address: [Address]
   codeHash: [Digest]
   creationHash: [Digest]
   creationBlock: [Quantity]))

(def (contract-config<-file config-filename)
  (<-json ContractConfig (read-file-json (config-path config-filename))))

(def (file<-contract-config config-filename config)
  (write-file-json (config-path config-filename) (json<- ContractConfig config)))

(def (contract-config<-db db-key)
  (<-bytes ContractConfig
           (or (db-get db-key)
               (error "No contract configuration in DB" db-key))))

(def (db<-contract-config db-key config)
  (db-put! db-key (bytes<- ContractConfig config)))

;; Query the Ethereum for the configuration given the hash of the transaction creating the contract
;; ContractConfig <- Digest
(def (contract-config<-creation-hash creation-hash)
  (def receipt (eth_getTransactionReceipt creation-hash))
  (unless (successful-receipt? receipt)
    (error "No receipt for contract creation tx" creation-hash))
  (def transaction-info (eth_getTransactionByHash creation-hash))
  (def contract-address (.@ receipt contract-address))
  (def creation-block (.@ receipt block-number))
  (def code-hash (.@ transaction-info input))
  {(contract-address) (code-hash) (creation-hash) (creation-block)})

;; <- ContractConfig
(def (verify-contract-config config)
  (def chain-config (contract-config<-creation-hash (.@ config creation-hash)))
  ;; TODO: automatically implement equality for records, better than that.
  (unless (equal? (<-bytes ContractConfig config) (<-bytes ContractConfig chain-config))
    (error "Contract configuration not matched by on-chain transaction"
      config chain-config)))

;; : ContractConfig <- (ContractConfig <- 'a) (Unit <- 'a ContractConfig) 'a (Digest <-)
(def (ensure-contract getter setter arg maker)
  (try
   (def config (getter arg))
   (verify-contract-config config)
   config
   (catch (_)
     (def creation-hash (maker))
     (def config (contract-config<-creation-hash creation-hash))
     (setter arg config)
     config)))

;; : ContractConfig <- PathString (Digest <-)
(def (ensure-contract-config/file filename maker)
  (ensure-contract contract-config<-file file<-contract-config filename maker))

;; : ContractConfig <- DBKey (Digest <-)
(def (ensure-contract-config/db db-key maker)
  (ensure-contract contract-config<-db db<-contract-config db-key maker))

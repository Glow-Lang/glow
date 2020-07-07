;; Ethereum-standard JSON descriptors for the ABI
;; https://solidity.readthedocs.io/en/develop/abi-spec.html#json

(export #t)
(import
  :clan/utils/base
  ./types ./ethereum ./abi ./transaction)

;; : Type <- String
(def (Type<-simple-eth-type s)
  (hash-get simple-eth-types s))

;; TODO: parse tuples and vector types, maybe even functions?
;; : Type <- String
(def (Type<-eth-type s)
  (or (Type<-simple-eth-type s)
      (error "ethereum type not supported" s)))

(def constructor<-contract-abi
  (cut find (lambda (x) (equal? (hash-get x "type") "constructor")) <>))

(def inputs<-function-abi
  (cut hash-get <> "inputs"))

(def (type<-variable-abi abi)
  (Type<-eth-type (hash-get abi "type")))

(def constructor-input-types<-contract-abi
  (compose (cut map type<-variable-abi <>) inputs<-function-abi constructor<-contract-abi))

;; If a JSON ABI descriptor is available, the types should be from constructor-input-types<-contract-abi
(def (create-ethabi-contract creator code types parameters value: (value 0) gas: (gas #f))
  (create-contract creator (ethabi-encode types parameters code) value: value gas: gas))

(export #t)

(import
  :gerbil/expander
  :std/format :std/getopt :std/iter :std/misc/hash
  :std/sort :std/srfi/13 :std/sugar
  :clan/basic-parsers :clan/cli :clan/decimal :clan/exit
  :clan/hash :clan/json :clan/list :clan/multicall :clan/path-config
  :clan/net/json-rpc
  :clan/poo/object :clan/poo/brace :clan/poo/cli :clan/poo/debug
  :clan/persist/db
  :mukn/ethereum/network-config :mukn/ethereum/types :mukn/ethereum/hex
  :mukn/ethereum/ethereum :mukn/ethereum/known-addresses
  :mukn/ethereum/json-rpc :mukn/ethereum/transaction :mukn/ethereum/cli
  :mukn/ethereum/testing :mukn/ethereum/erc20
  ./contacts ./identities)

(define-entry-point (transfer from: (from #f) to: (to #f) value: (value #f)
                              contacts: (contacts-file #f) identities: (identities-file #f))
  (help: "Send tokens from one account to the other"
   getopt: (make-options [] [(cut hash-restrict-keys! <> '(from to value))] options/send))
  (def contacts (load-contacts contacts-file identities-file))
  (def currency (.@ (ethereum-config) nativeCurrency))
  (def token-symbol (.@ currency symbol))
  (def network (.@ (ethereum-config) network))
  (set! from (parse-address (or from (error "Missing sender. Please use option --from"))))
  (set! to (parse-address (or to (error "Missing recipient. Please use option --to"))))
  (set! value (parse-currency-value
               (or value (error "Missing value. Please use option --value")) currency))
  (printf "\nSending ~a ~a from ~a to ~a on network ~a:\n"
          (decimal-string-ether<-wei value) token-symbol (0x<-address from) (0x<-address to) network)
  (printf "\nBalance before\n for ~a: ~a ~a,\n for ~a: ~a ~a\n"
          ;; TODO: use a function to correctly print with the right number of decimals,
          ;; with the correct token-symbol, depending on the current network and/or asset
          (0x<-address from) (decimal-string-ether<-wei (eth_getBalance from)) token-symbol
          (0x<-address to) (decimal-string-ether<-wei (eth_getBalance to)) token-symbol)
  (cli-send-tx {from to value} confirmations: 0)
  (printf "\nBalance after\n for ~a: ~a ~a,\n for ~a: ~a ~a\n"
          ;; TODO: use a function to correctly print with the right number of decimals
          (0x<-address from) (decimal-string-ether<-wei (eth_getBalance from)) token-symbol
          (0x<-address to) (decimal-string-ether<-wei (eth_getBalance to)) token-symbol))

(define-entry-point (faucet from: (from #f) to: (to #f)
                     contacts: (contacts-file #f) identities: (identities-file #f))
  (help: "Fund some accounts from the network faucet"
   getopt: (make-options []
                         [(cut hash-restrict-keys! <> '(from to value))]
                         [options/identities options/to]))
  (def-slots (network faucets name nativeCurrency) (ethereum-config))
  (load-contacts contacts-file identities-file)
  (unless to (error "Missing recipient. Please use option --to"))
  (set! to (parse-address to))
  (cond
   ((member name '("Private Ethereum Testnet"))
    (let ()
      ;; We import testing *after* the above, so we have *our* t/croesus,
      ;; but the user may have their own alice.
      (register-test-keys)
      (def value-in-ether 5)
      (def value (wei<-ether value-in-ether))
      (def token-symbol (.@ nativeCurrency symbol))
      (def from (address<-nickname "t/croesus"))
      (printf "\nSending ~a ~a from faucet ~a\n to ~a on network ~a:\n\n"
              value-in-ether token-symbol (0x<-address from) (0x<-address to) network)
      (printf "\nInitial balance: ~a ~a\n\n" (decimal-string-ether<-wei (eth_getBalance to)) token-symbol)
      (cli-send-tx {from to value} confirmations: 0)
      (printf "\nFinal balance: ~a ~a\n\n" (decimal-string-ether<-wei (eth_getBalance to)) token-symbol)))
   ((member name '("Cardano EVM Devnet"))
    (let ()
      (def faucetUrl (car faucets))
      (def token-symbol (.@ nativeCurrency symbol))
      (printf "\nRequesting 1.0 ~a token from faucet ~a to address ~a:\n\n"
              token-symbol faucetUrl (0x<-address to))
      (printf "\nInitial balance: ~a ~a\n\n" (decimal-string-ether<-wei (eth_getBalance to)) token-symbol)
      (def txHash
        (json-rpc faucetUrl "faucet_sendFunds" (vector to)
                  result-decoder: bytes<-0x
                  param-encoder: (.@ (Tuple Address) .json<-)
                  timeout: 15 log: write-json-ln))
      (def tx (eth_getTransactionByHash txHash))
      (printf "Transaction:\n  ~s\nWaiting for confirmation...\n" (sexp<- TransactionInformation tx))
      (def receipt (debug-confirm-tx tx))
      (printf "\nFinal balance: ~a ~a\n\n" (decimal-string-ether<-wei (eth_getBalance to)) token-symbol)))
   ((not (null? faucets))
    (printf "\nVisit the following URL to get ethers on network ~a:\n\n\t~a\n\n"
            (car faucets) network))
   (else
    (printf "\nThere is no faucet for network ~a - Go earn tokens the hard way.\n\n" network))))

(def options/erc20
  (make-options [(option 'erc20 "-e" "--erc20" help: "Address of ERC20 contract")]
                [] options/evm-network))

(define-entry-point ($erc20-transfer from: (from #f) erc20: (erc20 #f) to: (to #f) value: (value #f)
                                     contacts: (contacts-file #f) identities: (identities-file #f))
  (help: "Send ERC20 tokens from one account to the other"
   getopt: (make-options [] [(cut hash-restrict-keys! <> '(from erc20 to value))]
                         [options/send options/erc20]))
  (load-contacts contacts-file identities-file)
  (def currency (.@ (ethereum-config) nativeCurrency))
  (def token-symbol (.@ currency symbol))
  (def network (.@ (ethereum-config) network))
  (set! from (parse-address (or from (error "Missing sender. Please use option --from"))))
  (set! to (parse-address (or to (error "Missing recipient. Please use option --to"))))
  ;;; TODO: have some special directory for ERC20 tokens, and/or have a hierarchy of subtypes of addresses,
  ;;; and make sure to only deal with addresses that are at the same time current, of the correct type,
  ;;; valid in the given context, etc.
  (set! erc20 (parse-address (or erc20 (error "Missing recipient. Please use option --erc20"))))
  (set! value (with-catch
               (lambda _ (error "Missing or invalid value. Please use option --value"))
               (cut call-with-input-string
                    value (lambda (port) (begin0 (expect-natural port) (expect-eof port))))))
  (printf "\nSending ~a tokens from ~a to ~a on ERC20 contract ~a on network ~a...\n"
          value (0x<-address from) (0x<-address to) (0x<-address to) network)
  ;; TODO: use a function to correctly print with the right number of decimals,
  ;; with the correct token-symbol, depending on the current network and/or asset
  (printf "Balance for ~a on ERC20 contract ~a before: ~a\n"
          (0x<-address from) (0x<-address erc20) (erc20-balance erc20 from))
  (printf "Balance for ~a on ERC20 contract ~a before: ~a\n"
          (0x<-address to) (0x<-address erc20) (erc20-balance erc20 to))
  (erc20-transfer erc20 from to value)
  (printf "Balance for ~a on ERC20 contract ~a after: ~a\n"
          (0x<-address from) (0x<-address erc20) (erc20-balance erc20 from))
  (printf "Balance for ~a on ERC20 contract ~a after: ~a\n"
          (0x<-address to) (0x<-address erc20) (erc20-balance erc20 to)))

(export #t)

(import
  :std/format :std/srfi/1 :std/test
  :clan/runtime/db
  ../ethereum ../known-addresses ../json-rpc ../batch-send
  ./signing-test ./transaction-integrationtest)

;; Display an account having the given balance given a way to print address, optional name and balance
;; : 'a <- ('a <- string string) Address TokenAmount
(def (display-balance display address balance)
  (display (nicknamed-string<-address address) balance))

(def (get-address-missing-amount amount address)
  (def balance (eth_getBalance address 'pending))
  (printf (if (>= balance amount)
            "~a has ~a wei already. Good.\n"
            "~a has ~a wei only. Funding.\n") (nicknamed-string<-address address) balance)
  (if (>= balance amount)
    [] [[address (- amount balance)]]))

(def (ensure-addresses-prefunded (croesus croesus) (min-balance one-ether-in-wei) (addresses test-addresses))
  (batch-send croesus (append-map (cut get-address-missing-amount min-balance <>) addresses)
              log: displayln))

(ensure-addresses-prefunded)

(def batch-send-integrationtest
  (test-suite "integration test for glow/ethereum/batch-send"
    (test-case "batch transfer works"
      (def addresses test-addresses)
      (def balances-before (map (cut eth_getBalance <> 'pending) addresses))
      (def target-amount (+ (apply max balances-before) (expt 10 15))) ;; add one thousandth of an ETH in wei
      (printf "target-amount: ~a\n" target-amount)
      (ensure-addresses-prefunded croesus target-amount addresses)
      (def balances-after (map (cut eth_getBalance <> 'pending) addresses))
      (check-equal? balances-after (make-list (length addresses) target-amount)))))

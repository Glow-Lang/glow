(export #t)

(import
  :std/format :std/srfi/1 :std/test :std/sugar :std/iter
  :clan/persist/db :clan/decimal :clan/path-config :clan/poo/poo :clan/poo/io
  :mukn/ethereum/ethereum :mukn/ethereum/known-addresses :mukn/ethereum/json-rpc
  :mukn/ethereum/batch-send :mukn/ethereum/network-config :mukn/ethereum/assets
  :mukn/ethereum/known-addresses :mukn/ethereum/signing :mukn/ethereum/hex :mukn/ethereum/transaction
  ../runtime/ethereum-interpreter
  :mukn/ethereum/t/signing-test
  :mukn/ethereum/t/transaction-integrationtest
  )

;; Display an account having the given balance given a way to print address, optional name and balance
;; : 'a <- ('a <- string string) Address TokenAmount
(def (display-balance display address balance)
  (display (nicknamed-string<-address address) balance))

(def (get-address-missing-amount amount address)
  (def balance (eth_getBalance address 'pending))
  (printf (if (>= balance amount)
            "~a has ~a ether already. Good.\n"
            "~a has ~a ether only. Funding.\n")
          (nicknamed-string<-address address) (string<-decimal (/ balance one-ether-in-wei)))
  (if (>= balance amount)
    [] [[address (- amount balance)]]))

(def (ensure-addresses-prefunded sender (min-balance one-ether-in-wei) (addresses test-addresses))
  (batch-send sender (append-map (cut get-address-missing-amount min-balance <>) addresses)
              log: displayln))

(displayln "funding test accounts ...")
(ensure-addresses-prefunded (get-first-account))

(def program
  (parse-project-output "./examples/buy_sig.project.sexp"))
(def participants
  (hash
      (Buyer #u8(197 78 134 223 251 135 185 115 110 46 53 221 133 199 117 53 143 28 49 206))
      (Seller #u8(244 116 8 20 61 50 126 75 198 168 126 244 167 10 78 10 240 155 154 28))))
(def arguments
  (hash
    (digest0 [(string->bytes "abcdefghijklmnopqrstuvwxyz012345") Digest])
    (price [10000000 Ether])))
(def interpreter (make-Interpreter
  program: program
  participants: participants
  arguments: arguments))

{initialize interpreter}
{execute interpreter 'Buyer}

(def buy-sig-integrationtest
  (test-suite "integration test for ethereum/buy-sig"
    (test-case "buy sig works"
      {initialize interpreter}
      ;{execute interpreter 'Buyer}
      )))


#!/usr/bin/env gxi

(import
  :mukn/cardano/wallet)

(def (read-string name)
  (print (string-append name ": "))
  (read-line))

(def (read-int name)
  (print (string-append name ": "))
  (let (input (read-line))
    (string->number input)))

(def (read-amount)
  (let ((quantity (read-int " Quantity"))
        (unit (read-string " Unit")))
    (make-amount quantity unit)))

(def (read-payment)
  (println "Payment")
  (let ((address (read-string " Address"))
        (amount (read-amount)))
    (make-payment address amount)))

(def (read-payments)
  (list (read-payment)))

(def (create-transaction!)
  (let*
    ((wallet-id (read-string "Wallet id"))
     (payments (read-payments))
     (passphrase (read-string "Passphrase"))
     (wallet-response (create-transaction wallet-id payments passphrase)))
    (cond
      ((hash-key? wallet-response 'message) (begin
        (println (hash-get wallet-response 'code) ": " (hash-get wallet-response 'message) "\n\n")
        (println "RETRYING\n")
        (create-transaction!)))
      ((hash-key? wallet-response 'id) (begin
        (println "Transaction id: " (hash-get wallet-response 'id))))
      (else (error "Unknown wallet response" wallet-response)))))

(begin
  (println "CREATING TRANSACTION\n")
  (create-transaction!))


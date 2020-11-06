#!/usr/bin/env gxi

(import
  :mukn/glow/cardano/wallet)

(def (read-value name)
  (print (string-append name ": "))
  (read-line))

(def (create-wallet!)
  (let*
    ((name (read-value "Name"))
     ; TODO: use :clain/diceware to generate seed phrase
     (mnemonic-sentence (read-value "Mnemonic sentence"))
     (passphrase (read-value "Passphrase"))
     (wallet-response (create-wallet name (string-split mnemonic-sentence #\space) passphrase)))
    (cond
      ((hash-key? wallet-response 'message) (begin
        (println (hash-get wallet-response 'code) ": " (hash-get wallet-response 'message) "\n\n")
        (println "RETRYING\n")
        (create-wallet!)))
      ((hash-key? wallet-response 'id) (begin
        (println "Wallet id: " (hash-get wallet-response 'id))))
      (else (error "Unknown wallet response" wallet-response)))))

(begin
  (println "CREATING WALLET\n")
  (create-wallet!))


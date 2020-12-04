#!/usr/bin/env gxi

(import
  :mukn/glow/cardano/smart-contract-backend)

(def (read-value name)
  (print (string-append name ": "))
  (read-line))

(defvalues (header body)
  (values
    '(@header
      (Buyer Seller)
      ((digest0 : Digest) (price : Nat)))
    '(@body
      (@label begin0)
      (@label cp)
      (consensus:set-participant Buyer)
      (expect-deposited price)
      (@label cp0)
      (consensus:set-participant Seller)
      (consensus:set-participant Seller)
      (def signature (expect-published 'signature))
      (def tmp (@app isValidSignature Seller digest0 signature))
      (require! tmp)
      (expect-withdrawn Seller price)
      (return (@tuple))
      (@label end0))))

(defvalues (initial-var-map seller-var-map empty-var-map)
  (values
    '((Buyer . (pub-key "3d4017c3e843895a92b70aa74d1b7ebc9c982ccf2ec4968cc0cd55f12af4660c"))
      (Seller . (pub-key "3d4017c3e843895a92b70aa74d1b7ebc9c982ccf2ec4968cc0cd55f12af4660c"))
      (digest0 . "digest")
      (price . 100))
    '((signature . (signature "b446cd2020dbd63ffdef045fbf1233f0218322815e8141c886b281902875d059218d32cda7014fc2d6b9c8fdfefa63afcdfb7dad7fb55d20830c691bec28ee0a")))
    '()))

(def (execute-contract!)
  (begin
    (def contract-uuid (read-value "Enter contract uuid"))

    (println "\n\n- Creating buy sig instance ...")
    (glow-contract:create contract-uuid header body initial-var-map)

    (read-value "\n\nApply buyer move?")
    (glow-contract:move contract-uuid empty-var-map "cp")

    (read-value "\n\nApply seller move?")
    (glow-contract:move contract-uuid seller-var-map "cp0")))

(begin
  (println "EXECUTING CONTRACT\n")
  (execute-contract!))

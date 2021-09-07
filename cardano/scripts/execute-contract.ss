#!/usr/bin/env gxi

(import
 ;; std/misc/ports ;; TODO: Use this once dbg lbls are stripped
 :mukn/glow/cardano/smart-contract-backend
 :mukn/glow/cardano/wallet)

(def (read-value name)
  (print (string-append name ": "))
  (read-line))

;; TODO: We should get the project output from our passdata,
;; once we strip dbg lbls on glow-cardano backend:
;; For compiled:
;; (import (for-syntax :std/misc/ports :clan/syntax) :clan/syntax)
;; (def contract-code
;;   (syntax-call (lambda (stx) (read-file-string (stx-source-path stx "../../t/passdata/buy_sig.project-2.sexp")))))
;;
;; For interpreted:
;; (initialize-glow-path! [(source-path "dapps") (source-path "t/passdata")])
;; (read-file-string (find-dapp-path "buy_sig.project.sexp"))
;; Or:
;; (def contract-code (read-file-string "t/passdata/buy_sig.project.sexp"))
(def contract-code
     "(@module (begin end)
              (@label begin)
              (def payForSignature
                   (@make-interaction
                    ((@list Buyer Seller))
                    (digest0 price)
                    (begin0 end0)
                    (#f
                     (@label begin0)
                     (@label cp)
                     (consensus:set-participant Buyer)
                     (expect-deposited price)
                     (@label cp0)
                     (consensus:set-participant Seller)
                     (consensus:set-participant Seller)
                     (def signature (expect-published 'signature))
                     (def tmp
                          (@app isValidSignature Seller digest0 signature))
                     (require! tmp)
                     (consensus:withdraw Seller price)
                     (return (@tuple))
                     (@label end0))
                    (Buyer (@label begin0) ;; safe point
                           (@label cp) ;; redundant safe point, seller waits but buyer doesn't
                           (participant:set-participant Buyer)
                           (add-to-deposit price) ;; modifies buffer or fails
                           (@label cp0) ;; safe point, buyers waits and seller starts
                           (participant:set-participant Seller)
                           ;; flush buffer, contract initialization (no contract receipt), check cpitable2 for live public variables at checkpoint
                           (participant:set-participant Seller)
                           (def signature (expect-published 'signature))
                           (def tmp
                                (@app isValidSignature
                                      Seller
                                      digest0
                                      signature))
                           (require! tmp)
                           (participant:withdraw Seller price)
                           (return (@tuple))
                           (@label end0))
                    (Seller (@label begin0)
                            (@label cp)
                            (participant:set-participant Buyer)
                            (expect-deposited price)
                            (@label cp0)
                            (participant:set-participant Seller)
                            (def signature (sign digest0))
                            (participant:set-participant Seller)
                            (add-to-publish 'signature signature)
                            (def tmp
                                 (@app isValidSignature
                                       Seller
                                       digest0
                                       signature))
                            (require! tmp)
                            (participant:withdraw Seller price)
                            (return (@tuple))
                            (@label end0))))
              (return (@tuple))
              (@label end))")

(defvalues (initial-var-map seller-var-map empty-var-map)
  (values
    '((Buyer . (pub-key "d75a980182b10ab7d54bfed3c964073a0ee172f3daa62325af021a68f707511a"))
      (Seller . (pub-key "3d4017c3e843895a92b70aa74d1b7ebc9c982ccf2ec4968cc0cd55f12af4660c"))
      (digest0 . "digest")
      (price . 100))
    ;; FIXME: This should be generated from the wallet...
    '((signature . (signature "b446cd2020dbd63ffdef045fbf1233f0218322815e8141c886b281902875d059218d32cda7014fc2d6b9c8fdfefa63afcdfb7dad7fb55d20830c691bec28ee0a")))
    '()))


(def (execute-contract!)
  (def buyer-contract-instance-id (init-wallet-contract-instance-1))
  (def seller-contract-instance-id (init-wallet-contract-instance-2))

  (println "\n\nSeller waits for buyer to deploy contract")
  (glow-contract:wait seller-contract-instance-id)
  (thread-sleep! 5)

  (println "\n\n- Buyer creates buy sig instance ...")
  (glow-contract:create buyer-contract-instance-id contract-code initial-var-map)
  (thread-sleep! 5)

  (println "\n\nBuyer waits for seller to move")
  (glow-contract:wait buyer-contract-instance-id)
  (thread-sleep! 5)

  ;; (read-value "\n\nSeller moves") ;; TODO read params from cli / at the start and pass them in.
  (glow-contract:move seller-contract-instance-id seller-var-map "cp0")
  (thread-sleep! 5)
  )

(begin
  (println "EXECUTING CONTRACT\n")
  (execute-contract!))

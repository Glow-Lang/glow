(@module (begin end)
         (@label begin)
         (@debug-label dlb)
         (def payForSignature
              (@make-interaction
               ((@record (participants (@list Buyer Seller)) (assets (@list DefaultToken))))
               (digest0 price)
               (begin0 end0)
               (#f
                (@label begin0)
                (@debug-label dlb0)
                (@label cp)
                (consensus:set-participant Buyer)
                (expect-deposited (@record (DefaultToken price)))
                (@debug-label dlb1)
                (@label cp0)
                (consensus:set-participant Seller)
                (consensus:set-participant Seller)
                (def signature (expect-published 'signature))
                (def tmp
                     (@app isValidSignature Seller digest0 signature))
                (require! tmp)
                (@debug-label dlb2)
                (consensus:withdraw Seller (@record (DefaultToken price)))
                (return (@tuple))
                (@label end0))
               (Buyer (@label begin0) ;; safe point
                      (@debug-label dlb0)
                      (@label cp) ;; redundant safe point, seller waits but buyer doesn't
                      (participant:set-participant Buyer)
                      (add-to-deposit (@record (DefaultToken price))) ;; modifies buffer or fails
                      (@debug-label dlb1)
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
                      (@debug-label dlb2)
                      (participant:withdraw Seller (@record (DefaultToken price)))
                      (return (@tuple))
                      (@label end0))
               (Seller (@label begin0)
                       (@debug-label dlb0)
                       (@label cp)
                       (participant:set-participant Buyer)
                       (expect-deposited (@record (DefaultToken price)))
                       (@debug-label dlb1)
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
                       (@debug-label dlb2)
                       (participant:withdraw Seller (@record (DefaultToken price)))
                       (return (@tuple))
                       (@label end0))))
         (return (@tuple))
         (@label end))

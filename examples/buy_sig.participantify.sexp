;;-*- Gerbil -*-
(def payForSignature
     (@make-interaction
      ((@list Buyer Seller))
      ((digest0 : Digest) (price : nat))
      ()
      ;; Entry point for the interaction -- from here on user must be Buyer
      (participant-checkpoint pc #f Buyer);;;NEW!
      (@ Buyer (deposit! price))

      ;; Switching to Seller
      (participant-checkpoint pc0 Buyer Seller);;;NEW!
      (@ Seller (def signature (sign digest0)))
      (@ Seller (publish! signature))
      ;; This part is consensual, but part of the Seller transaction
      (def tmp (@app isValidSignature Seller digest0 signature))
      (require! tmp)
      (withdraw! Seller price)
      ;; In a standalone app, return kills the contract.
      ;; In a function called as part of a larger application,
      ;; it will actually set the state to the invoked continuation.
      (return (@tuple))))
;; Return from the application-defining interaction.
;; Instead of returning a unit, should it be returning a first-class environment
;; exporting all the variables defined?
(return (@tuple))

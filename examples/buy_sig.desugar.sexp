(@module
(@debug-label dlb)
(def payForSignature
     ()
     (@make-interaction
      ((@list Buyer Seller))
      ((digest0 : Digest) (price : Nat))
      ()
      (@debug-label dlb0)
      (deposit! Buyer price)
      (@debug-label dlb1)
      (@ Seller (def signature () (sign digest0)))
      (publish! Seller signature)
      (require! (@app isValidSignature Seller digest0 signature))
      (@debug-label dlb2)
      (withdraw! Seller price))))

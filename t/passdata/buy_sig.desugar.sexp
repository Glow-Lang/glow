(@module
(@debug-label dlb)
(def payForSignature
     ()
     (@make-interaction
      ((@record (participants (@list Buyer Seller)) (assets (@list DefaultToken))))
      ((digest0 : Digest) (price : Nat))
      ()
      (@debug-label dlb0)
      (deposit! Buyer (@record (DefaultToken price)))
      (@debug-label dlb1)
      (@ Seller (def signature () (sign digest0)))
      (publish! Seller signature)
      (require! (@app isValidSignature Seller digest0 signature))
      (@debug-label dlb2)
      (withdraw! Seller (@record (DefaultToken price))))))

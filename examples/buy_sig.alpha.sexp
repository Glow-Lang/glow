(@module
(@debug-label dlb)
(@interaction
 ((@list Buyer Seller))
 (def payForSignature
      (λ ((digest0 : Digest) (price : Nat))
         (@debug-label dlb0)
         (deposit! Buyer price)
         (@debug-label dlb1)
         (@publicly (Seller) (def signature (sign digest0)))
         (@debug-label dlb2)
         (withdraw! Seller price)))))

(@module
(@debug-label dlb)
(@interaction
 ((@record (participants (@list Buyer Seller)) (assets (@list DefaultToken))))
 (def payForSignature
      (λ ((digest0 : Digest) (price : Nat))
         (@debug-label dlb0)
         (deposit! Buyer (@record (DefaultToken price)))
         (@debug-label dlb1)
         (@publicly! (Seller) (def signature (sign digest0)))
         (@debug-label dlb2)
         (withdraw! Seller (@record (DefaultToken price)))))))

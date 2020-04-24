(@interaction
 ((@list Buyer Seller))
 (def payForSignature
      (λ ((digest0 : Digest) (price : nat))
         (@ Buyer (deposit! price))
         (@ Seller (@publicly (def signature (sign digest0))))
         (withdraw! Seller price))))

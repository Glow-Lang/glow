(@interaction
 ((@list Buyer Seller))
 (def payForSignature
      (λ ((digest0 : Digest) (price : Assets))
         (@ Buyer (deposit! price))
         (@ Seller (def signature (sign digest0)))
         (@ Seller (publish! signature))
         (def tmp (@app valid_signature signature digest0 Seller))
         (require! tmp)
         (withdraw! Seller price))))

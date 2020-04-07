(@ (interaction (@list Buyer Seller))
   (def payForSignature
     (λ ((digest : Digest) (price : Assets))
       (@ Buyer (deposit! price))
       (@ Seller (@ publicly (def signature (sign digest))))
       (withdraw! Seller price))))

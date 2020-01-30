(@ (interaction (@list Buyer Seller))
   (def payForSignature
     (λ ((digest : Digest) (price : Assets))
       (@ Buyer (deposit! price))
       (@ Seller (@ verifiably (def signature (sign digest))))
       (@ Seller (publish! signature))
       (verify! signature)
       (withdraw! Seller price))))

(@ (interaction (@list Buyer Seller))
   (def payForSignature
     (λ ((digest : Digest) (price : nat))
       (@ Buyer (deposit! price))
       (@ Seller (@ verifiably (def signature (sign digest))))
       (@ Seller (publish! signature))
       (verify! signature)
       (withdraw! Seller price))))

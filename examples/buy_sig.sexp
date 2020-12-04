(@module
(@ (interaction (@list Buyer Seller))
   (def payForSignature
     (λ ((digest : Digest) (price : Nat))
       (deposit! Buyer price)
       (@ Seller (@ publicly (def signature (sign digest))))
       (withdraw! Seller price)))))

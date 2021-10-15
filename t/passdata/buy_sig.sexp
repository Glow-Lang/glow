(@module
(@ (interaction (@list Buyer Seller))
   (def buySig
     (λ ((digest : Digest) (price : Nat))
       (deposit! Buyer price)
       (@ (publicly! Seller) (def signature (sign digest)))
       (withdraw! Seller price)))))

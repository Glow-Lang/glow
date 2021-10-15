(@module
(@debug-label dlb)
(@ (interaction (@list Buyer Seller))
   (def buySig
     (λ ((digest : Digest) (price : Nat))
       (@debug-label dlb0)
       (deposit! Buyer price)
       (@debug-label dlb1)
       (@ (publicly! Seller) (def signature (sign digest)))
       (@debug-label dlb2)
       (withdraw! Seller price)))))

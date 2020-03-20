(@ (interaction-participant (@list Buyer Seller) Buyer)
   (def payForSignature
     (λ ((digest : Digest) (price : Assets))
       (perform-transaction
         (deposit! price))
       (expect-transaction Seller
         (expect-published (signature : Signature))
         (expect-verified (is-signature? signature digest))
         (expect-withdrawed Seller price)))))

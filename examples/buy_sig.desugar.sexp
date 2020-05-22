(def payForSignature
     ()
     (@make-interaction
      ((@list Buyer Seller))
      ((digest0 : Digest) (price : nat))
      ()
      (deposit! Buyer price)
      (@ Seller (def signature () (sign digest0)))
      (publish! Seller signature)
      (require! (@app isValidSignature Seller digest0 signature))
      (withdraw! Seller price)))

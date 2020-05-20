(def payForSignature
     ()
     (@make-interaction
      ((@list Buyer Seller))
      ((digest0 : Digest) (price : nat))
      ()
      (@ Buyer (deposit! price))
      (@ Seller (def signature () (sign digest0)))
      (publish! Seller signature Signature.marshal Signature.unmarshal)
      (require! (@app isValidSignature Seller digest0 signature))
      (withdraw! Seller price)))

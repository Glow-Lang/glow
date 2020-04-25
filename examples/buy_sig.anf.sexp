(def payForSignature
     (@make-interaction
      ((@list Buyer Seller))
      ((digest0 : Digest) (price : nat))
      ()
      (@ Buyer (deposit! price))
      (@ Seller (def signature (sign digest0)))
      (@ Seller (publish! signature))
      (def tmp (@app isValidSignature Seller digest0 signature))
      (require! tmp)
      (withdraw! Seller price)
      (return (@tuple))))
(return (@tuple))

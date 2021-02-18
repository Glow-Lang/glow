(@module
(@debug-label dlb)
(def payForSignature
     (@make-interaction
      ((@list Buyer Seller))
      (digest0 price)
      (@debug-label dlb0)
      (deposit! Buyer price)
      (@debug-label dlb1)
      (@ Seller (def signature (sign digest0)))
      (publish! Seller signature)
      (def tmp (@app isValidSignature Seller digest0 signature))
      (require! tmp)
      (@debug-label dlb2)
      (withdraw! Seller price)
      (return (@tuple))))
(return (@tuple)))

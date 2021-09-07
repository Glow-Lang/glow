(defvalues (header body)
  (values
    '(@header
      (Buyer Seller)
      ((digest0 : Digest) (price : Nat)))
    '(@body
      (@label begin0)
      (@label cp)
      (consensus:set-participant Buyer)
      (expect-deposited price)
      (@label cp0)
      (consensus:set-participant Seller)
      (consensus:set-participant Seller)
      (def signature (expect-published 'signature))
      (def tmp (@app isValidSignature Seller digest0 signature))
      (require! tmp)
      (expect-withdrawn Seller price)
      (return (@tuple))
      (@label end0))))

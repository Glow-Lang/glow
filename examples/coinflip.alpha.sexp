(@module
(@interaction
 ((@list A B))
 (def coinFlip
  (λ (wagerAmount escrowAmount)
    (@ A (def randA (@app randomUInt256)))
    (@ A (@verifiably (def commitment (digest randA))))
    (publish! A commitment)
    (deposit! A (@app + wagerAmount escrowAmount))
    (@ B (def randB (@app randomUInt256)))
    (publish! B randB)
    (deposit! B wagerAmount)
    (publish! A randA)
    (verify! commitment)
    (if (== (@app bitwise-and (@app bitwise-xor randA randB) 1) 0)
        (withdraw! A (@app + (@app * 2 wagerAmount) escrowAmount))
        (block
          (withdraw! B (@app * 2 wagerAmount))
          (withdraw! A escrowAmount)))))))

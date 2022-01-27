(@module
  (@ (interaction (@list From To))
    (def transfer
      (λ ((amount : Nat))
        (deposit! From amount)
        (withdraw! To amount)
        amount))))

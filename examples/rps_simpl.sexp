(@module (@ (interaction (@list A B))
            (def rockPaperScissors
                 (λ (wagerAmount)
                    (@ A
                       (def handA
                            (input Nat
                                   "First player, pick your hand: 0 (Rock), 1 (Paper), 2 (Scissors)")))
                    (@ A (require! (< handA 3)))
                    (@ A (def salt (@app randomUInt256)))
                    (@ A
                       (@ verifiably
                          (def commitment (digest (@tuple salt handA)))))
                    (publish! A commitment)
                    (deposit! A wagerAmount)
                    (@ B
                       (def handB
                            (input Nat
                                   "Second player, pick your hand: 0 (Rock), 1 (Paper), 2 (Scissors)")))
                    (publish! B handB)
                    (deposit! B wagerAmount)
                    (require! (< handB 3))
                    (publish! A salt handA)
                    (require! (< handA 3))
                    (verify! commitment)
                    (def outcome (mod (+ handA (- 4 handB)) 3))
                    (switch outcome
                            (2 (withdraw! A (* 2 wagerAmount)))
                            (0 (withdraw! B (* 2 wagerAmount)))
                            (1
                             (withdraw! A wagerAmount)
                             (withdraw! B wagerAmount)))
                    outcome))))

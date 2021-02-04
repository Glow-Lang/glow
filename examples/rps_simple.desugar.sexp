(@module (def winner () (λ ((handA : Nat) (handB : Nat)) (: Nat) (@app mod (@app + handA (@app - 4 handB)) 3)))
         (def rockPaperScissors
              ()
              (@make-interaction
               ((@list A B))
               (wagerAmount)
               ()
               (@ A
                  (def handA0
                       ()
                       (input Nat "First player, pick your hand: 0 (Rock), 1 (Paper), 2 (Scissors)")))
               (@ A (require! (@app < handA0 3)))
               (@ A (def salt () (@app randomUInt256)))
               (@ A (def commitment () (digest salt handA0)))
               (publish! A commitment)
               (deposit! A wagerAmount)
               (@ B
                  (def handB0
                       ()
                       (input Nat "Second player, pick your hand: 0 (Rock), 1 (Paper), 2 (Scissors)")))
               (publish! B handB0)
               (deposit! B wagerAmount)
               (require! (@app < handB0 3))
               (publish! A salt)
               (publish! A handA0)
               (require! (@app < handA0 3))
               (require! (== commitment (digest salt handA0)))
               (def outcome () (@app winner handA0 handB0))
               (switch outcome
                       (2 (withdraw! A (@app * 2 wagerAmount)))
                       (0 (withdraw! B (@app * 2 wagerAmount)))
                       (1 (withdraw! A wagerAmount) (withdraw! B wagerAmount)))
               outcome)))

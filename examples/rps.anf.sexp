(defdata Hand
         (Rock Paper Scissors)
         (with:
          (def tmp
               (λ (tag)
                  (: Hand)
                  (def x (input Hand tag))
                  (ann x Hand)
                  (return x)))
          (def tmp0
               (λ ((x0 : Hand))
                  (: nat)
                  (switch x0
                          (Rock (return 0))
                          (Paper (return 1))
                          (Scissors (return 2)))))
          (def tmp2
               (λ ((x1 : nat))
                  (: Hand)
                  (switch x1
                          (0 (return Rock))
                          (1 (return Paper))
                          (2 (return Scissors)))))
          (def tmp4 (@record (input tmp) (toNat tmp1) (ofNat tmp3)))
          (return tmp4)))
(defdata Outcome
         (B_Wins Draw A_Wins)
         (with:
          (def tmp5
               (λ (tag0)
                  (: Outcome)
                  (def x2 (input Outcome tag0))
                  (ann x2 Outcome)
                  (return x2)))
          (def tmp6
               (λ ((x3 : Outcome))
                  (: nat)
                  (switch x3
                          (B_Wins (return 0))
                          (Draw (return 1))
                          (A_Wins (return 2)))))
          (def tmp8
               (λ ((x4 : nat))
                  (: Outcome)
                  (switch x4
                          (0 (return B_Wins))
                          (1 (return Draw))
                          (2 (return A_Wins)))))
          (def tmp10 (@record (input tmp5) (toNat tmp7) (ofNat tmp9)))
          (return tmp10)))
(def winner
     (λ ((handA : Hand) (handB : Hand))
        (: Outcome)
        (def tmp11 (@dot Outcome ofNat))
        (def tmp12 (@dot Hand toNat))
        (def tmp13 (@app tmp12 handA))
        (def tmp14 (@dot Hand toNat))
        (def tmp15 (@app tmp14 handB))
        (def tmp16 (@app - 4 tmp15))
        (def tmp17 (@app + tmp13 tmp16))
        (def tmp18 (@app mod tmp17 3))
        (return (@app tmp11 tmp18))))
(def rockPaperScissors
     (@make-interaction
      ((@list A B))
      (wagerAmount)
      ()
      (@ A (def tmp19 (@dot Hand input)))
      (@ A (def handA0 (@app tmp19 "First player, pick your hand")))
      (@ A (def salt (@app randomUInt256)))
      (@ A (def tmp20 (@tuple salt handA0)))
      (@ A (def commitment (digest tmp20)))
      (publish! A commitment)
      (deposit! A wagerAmount)
      (@ B (def tmp21 (@dot Hand input)))
      (@ B (def handB0 (@app tmp21 "Second player, pick your hand")))
      (publish! B handB0)
      (deposit! B wagerAmount)
      (publish! A salt)
      (publish! A handA0)
      (def tmp22 (@tuple salt handA0))
      (def tmp23 (digest tmp22))
      (def tmp24 (== commitment tmp23))
      (require! tmp24)
      (def outcome (@app winner handA0 handB0))
      (switch outcome
              (A_Wins (def tmp25 (@app * 2 wagerAmount)) (withdraw! A tmp25))
              (B_Wins (def tmp26 (@app * 2 wagerAmount)) (withdraw! B tmp26))
              (Draw (withdraw! A wagerAmount) (withdraw! B wagerAmount)))
      (return outcome)))
(return (@tuple))


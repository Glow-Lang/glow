(defdata Hand
         Rock
         Paper
         Scissors
         with:
         (block (def tmp (λ (tag) (def x : name (input name tag)) x))
                (def tmp0
                     (λ ((x0 : name))
                        (switch x0 (Rock 0) (Paper 1) (Scissors 2))))
                (def tmp1
                     (λ ((x1 : name))
                        (switch x1 (0 Rock) (1 Paper) (2 Scissors))))
                (@record (input tmp) (toNat tmp0) (ofNat tmp1))))
(defdata Outcome
         B_Wins
         Draw
         A_Wins
         with:
         (block (def tmp2 (λ (tag0) (def x2 : name (input name tag0)) x2))
                (def tmp3
                     (λ ((x3 : name))
                        (switch x3 (B_Wins 0) (Draw 1) (A_Wins 2))))
                (def tmp4
                     (λ ((x4 : name))
                        (switch x4 (0 B_Wins) (1 Draw) (2 A_Wins))))
                (@record (input tmp2) (toNat tmp3) (ofNat tmp4))))
(def winner
     (λ ((handA : Hand) (handB : Hand))
        :
        Outcome
        (def tmp5 (@dot Outcome ofNat))
        (def tmp6 (@dot Hand toNat))
        (def tmp7 (@app tmp6 handA))
        (def tmp8 (@dot Hand toNat))
        (def tmp9 (@app tmp8 handB))
        (def tmp10 (@app - 4 tmp9))
        (def tmp11 (@app mod tmp10 3))
        (def tmp12 (@app + tmp7 tmp11))
        (@app tmp5 tmp12)))
(@interaction
 ((@list A B))
 (def rockPaperScissors
      (λ (wagerAmount)
         (@ A (def tmp13 (@dot Hand input)))
         (@ A (def handA0 (@app tmp13 "First player, pick your hand")))
         (@ A (def salt (@app randomUInt256)))
         (@ A (def tmp14 (@tuple salt handA0)))
         (@ A (def commitment (digest tmp14)))
         (@ A (publish! commitment))
         (@ A (deposit! wagerAmount))
         (@ B (def tmp15 (@dot Hand input)))
         (@ B (def handB0 (@app tmp15 "Second player, pick your hand")))
         (@ B (publish! handB0))
         (@ B (deposit! wagerAmount))
         (@ A (publish! salt handA0))
         (def tmp16 (@tuple salt handA0))
         (def tmp17 (digest tmp16))
         (def tmp18 (@app = commitment tmp17))
         (require! tmp18)
         (def outcome (@app winner handA0 handB0))
         (switch outcome
                 (A_Wins (def tmp19 (@app * 2 wagerAmount))
                         (withdraw! A tmp19))
                 (B_Wins (def tmp20 (@app * 2 wagerAmount))
                         (withdraw! B tmp20))
                 (Draw (withdraw! A wagerAmount) (withdraw! B wagerAmount)))
         outcome)))

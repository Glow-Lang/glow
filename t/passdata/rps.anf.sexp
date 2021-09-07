(@module
(@debug-label dlb)
(defdata Hand
         Rock Paper Scissors)

(def tmp
               (λ (tag)
                  (def x (input Hand tag))
                  (return x)))
(def tmp0
               (λ (x0)
                  (switch x0
                          ((@app-ctor Rock) (return 0))
                          ((@app-ctor Paper) (return 1))
                          ((@app-ctor Scissors) (return 2)))))
(def tmp1
               (λ (x1)
                  (switch x1
                          (0 (return Rock))
                          (1 (return Paper))
                          (2 (return Scissors)))))
(def Hand1 (@record (input tmp) (toNat tmp0) (ofNat tmp1)))

(@debug-label dlb0)
(defdata Outcome
         B_Wins Draw A_Wins)

(def tmp2
               (λ (tag0)
                  (def x2 (input Outcome tag0))
                  (return x2)))
(def tmp3
               (λ (x3)
                  (switch x3
                          ((@app-ctor B_Wins) (return 0))
                          ((@app-ctor Draw) (return 1))
                          ((@app-ctor A_Wins) (return 2)))))
(def tmp4
               (λ (x4)
                  (switch x4
                          (0 (return B_Wins))
                          (1 (return Draw))
                          (2 (return A_Wins)))))
(def Outcome1 (@record (input tmp2) (toNat tmp3) (ofNat tmp4)))

(@debug-label dlb1)
(def winner
     (λ (handA handB)
        (@debug-label dlb2)
        (def tmp5 (@dot Outcome1 ofNat))
        (def tmp6 (@dot Hand1 toNat))
        (def tmp7 (@app tmp6 handA))
        (def tmp8 (@dot Hand1 toNat))
        (def tmp9 (@app tmp8 handB))
        (def tmp10 (@app - 4 tmp9))
        (def tmp11 (@app + tmp7 tmp10))
        (def tmp12 (@app mod tmp11 3))
        (return (@app tmp5 tmp12))))

(@debug-label dlb3)
(def rockPaperScissors
     (@make-interaction
      ((@record (participants (@list A B)) (assets (@list DefaultToken))))
      (wagerAmount)
      (@debug-label dlb4)
      (@ A (def tmp13 (@dot Hand1 input)))
      (@ A (def handA0 (@app tmp13 "First player, pick your hand")))
      (@debug-label dlb5)
      (@ A (def salt (@app randomUInt256)))
      (@debug-label dlb6)
      (@ A (def tmp14 (@tuple salt handA0)))
      (@ A (def commitment (digest tmp14)))
      (@debug-label dlb7)
      (publish! A commitment)
      (@debug-label dlb8)
      (deposit! A (@record (DefaultToken wagerAmount)))
      (@debug-label dlb9)
      (@ B (def tmp15 (@dot Hand1 input)))
      (@ B (def handB0 (@app tmp15 "Second player, pick your hand")))
      (@debug-label dlb10)
      (publish! B handB0)
      (@debug-label dlb11)
      (deposit! B (@record (DefaultToken wagerAmount)))
      (@debug-label dlb12)
      (publish! A salt)
      (publish! A handA0)
      (@debug-label dlb13)
      (def tmp16 (@tuple salt handA0))
      (def tmp17 (digest tmp16))
      (def tmp18 (== commitment tmp17))
      (require! tmp18)
      (@debug-label dlb14)
      (def outcome (@app winner handA0 handB0))
      (@debug-label dlb15)
      (switch outcome
              ((@app-ctor A_Wins) (@debug-label dlb16)
                                  (def tmp19 (@app * 2 wagerAmount))
                                  (withdraw! A (@record (DefaultToken tmp19))))
              ((@app-ctor B_Wins) (@debug-label dlb17)
                                  (def tmp20 (@app * 2 wagerAmount))
                                  (withdraw! B (@record (DefaultToken tmp20))))
              ((@app-ctor Draw) (@debug-label dlb18)
                                (withdraw! A (@record (DefaultToken wagerAmount)))
                                (@debug-label dlb19)
                                (withdraw! B (@record (DefaultToken wagerAmount)))))
      (@debug-label dlb20)
      (return outcome)))
(return (@tuple)))

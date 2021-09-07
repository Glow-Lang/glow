(hash (begin
        (ci 'begin
            (@list)
            (@list)
            (@list (ti 'begin
                       'end
                       #f
                       (@list)
                       (@list '(rockPaperScissors . #f)
                              '(winner . #f)
                              '(Outcome1 . #f)
                              '(tmp4 . #f)
                              '(tmp3 . #f)
                              '(tmp2 . #f)
                              '(Hand1 . #f)
                              '(tmp1 . #f)
                              '(tmp0 . #f)
                              '(tmp . #f))
                       (@list '(tmp2 . #f) '(tmp3 . #f) '(tmp4 . #f) '(tmp . #f) '(tmp0 . #f) '(tmp1 . #f))
                       (@list)))))
      (begin-switch
       (ci 'begin-switch
           (@list 'x0)
           (@list (ti 'begin1 'begin-switch #f (@list) (@list) (@list) (@list)))
           (@list (ti 'begin-switch 'end1 #f (@list) (@list) (@list '(x0 . #f)) (@list '(x0 . #f)))
                  (ti 'begin-switch 'end1 #f (@list) (@list) (@list '(x0 . #f)) (@list '(x0 . #f)))
                  (ti 'begin-switch 'end1 #f (@list) (@list) (@list '(x0 . #f)) (@list '(x0 . #f))))))
      (begin-switch0
       (ci 'begin-switch0
           (@list 'Rock 'x1 'Paper 'Scissors)
           (@list (ti 'begin2 'begin-switch0 #f (@list) (@list) (@list) (@list)))
           (@list (ti 'begin-switch0 'end2 #f (@list) (@list) (@list '(Scissors . #f) '(x1 . #f)) (@list '(Rock . #f) '(x1 . #f) '(Paper . #f) '(Scissors . #f)))
                  (ti 'begin-switch0 'end2 #f (@list) (@list) (@list '(Paper . #f) '(x1 . #f)) (@list '(Rock . #f) '(x1 . #f) '(Paper . #f) '(Scissors . #f)))
                  (ti 'begin-switch0 'end2 #f (@list) (@list) (@list '(Rock . #f) '(x1 . #f)) (@list '(Rock . #f) '(x1 . #f) '(Paper . #f) '(Scissors . #f))))))
      (begin-switch1
       (ci 'begin-switch1
           (@list 'x3)
           (@list (ti 'begin4 'begin-switch1 #f (@list) (@list) (@list) (@list)))
           (@list (ti 'begin-switch1 'end4 #f (@list) (@list) (@list '(x3 . #f)) (@list '(x3 . #f)))
                  (ti 'begin-switch1 'end4 #f (@list) (@list) (@list '(x3 . #f)) (@list '(x3 . #f)))
                  (ti 'begin-switch1 'end4 #f (@list) (@list) (@list '(x3 . #f)) (@list '(x3 . #f))))))
      (begin-switch2
       (ci 'begin-switch2
           (@list 'B_Wins 'x4 'Draw 'A_Wins)
           (@list (ti 'begin5 'begin-switch2 #f (@list) (@list) (@list) (@list)))
           (@list (ti 'begin-switch2 'end5 #f (@list) (@list) (@list '(A_Wins . #f) '(x4 . #f)) (@list '(B_Wins . #f) '(x4 . #f) '(Draw . #f) '(A_Wins . #f)))
                  (ti 'begin-switch2 'end5 #f (@list) (@list) (@list '(Draw . #f) '(x4 . #f)) (@list '(B_Wins . #f) '(x4 . #f) '(Draw . #f) '(A_Wins . #f)))
                  (ti 'begin-switch2 'end5 #f (@list) (@list) (@list '(B_Wins . #f) '(x4 . #f)) (@list '(B_Wins . #f) '(x4 . #f) '(Draw . #f) '(A_Wins . #f))))))
      (begin-switch3
       (ci 'begin-switch3
           (@list 'A '* 'wagerAmount 'outcome 'B)
           (@list (ti 'cp1
                      'begin-switch3
                      'A
                      (@list (syntax (publish! A handA0)) (syntax (publish! A salt)))
                      (@list '(outcome . #f) '(tmp18 . #f) '(tmp17 . #f) '(tmp16 . #f) '(handA0 . #f) '(salt . #f))
                      (@list '(winner . #f)
                             '(handA0 . #f)
                             '(handB0 . #f)
                             '(tmp18 . #f)
                             '(commitment . #f)
                             '(tmp17 . #f)
                             '(tmp16 . #f)
                             '(salt . #f)
                             '(handA0 . #f)
                             '(handA0 . A)
                             '(A . #f)
                             '(salt . A)
                             '(A . #f))
                      (@list '(winner . #f) '(handB0 . #f) '(commitment . #f))))
           (@list (ti 'begin-switch3
                      'end-switch3
                      #f
                      (@list (syntax (withdraw! B (@record (DefaultToken wagerAmount))))
                             (syntax (withdraw! A (@record (DefaultToken wagerAmount)))))
                      (@list)
                      (@list '(B . #f) '(wagerAmount . #f) '(A . #f) '(wagerAmount . #f) '(outcome . #f))
                      (@list '(A . #f) '(* . #f) '(wagerAmount . #f) '(B . #f)))
                  (ti 'begin-switch3
                      'end-switch3
                      #f
                      (@list (syntax (withdraw! B (@record (DefaultToken tmp20)))))
                      (@list '(tmp20 . #f))
                      (@list '(B . #f) '(tmp20 . #f) '(* . #f) '(wagerAmount . #f) '(outcome . #f))
                      (@list '(A . #f) '(* . #f) '(wagerAmount . #f) '(B . #f)))
                  (ti 'begin-switch3
                      'end-switch3
                      #f
                      (@list (syntax (withdraw! A (@record (DefaultToken tmp19)))))
                      (@list '(tmp19 . #f))
                      (@list '(A . #f) '(tmp19 . #f) '(* . #f) '(wagerAmount . #f) '(outcome . #f))
                      (@list '(A . #f) '(* . #f) '(wagerAmount . #f) '(B . #f))))))
      (begin0 (ci 'begin0 (@list 'tag) (@list)
                  (@list (ti 'begin0 'end0 #f (@list) (@list '(x . #f)) (@list '(x . #f) '(tag . #f)) (@list '(tag . #f))))))
      (begin1 (ci 'begin1 (@list 'x0) (@list)
                  (@list (ti 'begin1 'begin-switch #f (@list) (@list) (@list) (@list)))))
      (begin2 (ci 'begin2 (@list 'Rock 'x1 'Paper 'Scissors) (@list)
                  (@list (ti 'begin2 'begin-switch0 #f (@list) (@list) (@list) (@list)))))
      (begin3 (ci 'begin3 (@list 'tag0) (@list)
                  (@list (ti 'begin3 'end3 #f (@list) (@list '(x2 . #f)) (@list '(x2 . #f) '(tag0 . #f)) (@list '(tag0 . #f))))))
      (begin4 (ci 'begin4 (@list 'x3) (@list)
                  (@list (ti 'begin4 'begin-switch1 #f (@list) (@list) (@list) (@list)))))
      (begin5 (ci 'begin5 (@list 'B_Wins 'x4 'Draw 'A_Wins) (@list)
                  (@list (ti 'begin5 'begin-switch2 #f (@list) (@list) (@list) (@list)))))
      (begin6 (ci 'begin6
                  (@list 'mod '+ '- 'handB 'Hand1 'handA 'Outcome1)
                  (@list)
                  (@list (ti 'begin6
                             'end6
                             #f
                             (@list)
                             (@list '(tmp12 . #f) '(tmp11 . #f) '(tmp10 . #f) '(tmp9 . #f) '(tmp8 . #f) '(tmp7 . #f) '(tmp6 . #f) '(tmp5 . #f))
                             (@list '(tmp5 . #f)
                                    '(tmp12 . #f)
                                    '(mod . #f)
                                    '(tmp11 . #f)
                                    '(+ . #f)
                                    '(tmp7 . #f)
                                    '(tmp10 . #f)
                                    '(- . #f)
                                    '(tmp9 . #f)
                                    '(tmp8 . #f)
                                    '(handB . #f)
                                    '(Hand1 . #f)
                                    '(tmp6 . #f)
                                    '(handA . #f)
                                    '(Hand1 . #f)
                                    '(Outcome1 . #f))
                             (@list '(mod . #f) '(+ . #f) '(- . #f) '(handB . #f) '(Hand1 . #f) '(handA . #f) '(Outcome1 . #f))))))
      (begin7 (ci 'begin7 (@list 'winner 'A '* 'wagerAmount 'B) (@list) (@list (ti 'begin7 'cp #f (@list) (@list) (@list) (@list)))))
      (cp (ci 'cp
              (@list 'winner 'A '* 'wagerAmount 'B)
              (@list (ti 'begin7 'cp #f (@list) (@list) (@list) (@list)))
              (@list (ti 'cp
                         'cp0
                         'A
                         (@list (syntax (deposit! A (@record (DefaultToken wagerAmount))))
                                (syntax (publish! A commitment)))
                         (@list '(commitment . #f))
                         (@list '(A . #f) '(wagerAmount . #f) '(commitment . A) '(A . #f))
                         (@list)))))
      (cp0 (ci 'cp0
               (@list 'winner 'commitment 'A '* 'wagerAmount 'B)
               (@list (ti 'cp
                          'cp0
                          'A
                          (@list (syntax (deposit! A (@record (DefaultToken wagerAmount))))
                                 (syntax (publish! A commitment)))
                          (@list '(commitment . #f))
                          (@list '(A . #f) '(wagerAmount . #f) '(commitment . A) '(A . #f))
                          (@list)))
               (@list (ti 'cp0
                          'cp1
                          'B
                          (@list (syntax (deposit! B (@record (DefaultToken wagerAmount))))
                                 (syntax (publish! B handB0)))
                          (@list '(handB0 . #f))
                          (@list '(B . #f) '(wagerAmount . #f) '(handB0 . B) '(B . #f))
                          (@list)))))
      (cp1 (ci 'cp1
               (@list 'winner 'handB0 'commitment 'A '* 'wagerAmount 'B)
               (@list (ti 'cp0
                          'cp1
                          'B
                          (@list (syntax (deposit! B (@record (DefaultToken wagerAmount))))
                                 (syntax (publish! B handB0)))
                          (@list '(handB0 . #f))
                          (@list '(B . #f) '(wagerAmount . #f) '(handB0 . B) '(B . #f))
                          (@list)))
               (@list (ti 'cp1
                          'begin-switch3
                          'A
                          (@list (syntax (publish! A handA0)) (syntax (publish! A salt)))
                          (@list '(outcome . #f) '(tmp18 . #f) '(tmp17 . #f) '(tmp16 . #f) '(handA0 . #f) '(salt . #f))
                          (@list '(winner . #f)
                                 '(handA0 . #f)
                                 '(handB0 . #f)
                                 '(tmp18 . #f)
                                 '(commitment . #f)
                                 '(tmp17 . #f)
                                 '(tmp16 . #f)
                                 '(salt . #f)
                                 '(handA0 . #f)
                                 '(handA0 . A)
                                 '(A . #f)
                                 '(salt . A)
                                 '(A . #f))
                          (@list '(winner . #f) '(handB0 . #f) '(commitment . #f))))))
      (end (ci 'end
               (@list)
               (@list (ti 'begin
                          'end
                          #f
                          (@list)
                          (@list '(rockPaperScissors . #f)
                                 '(winner . #f)
                                 '(Outcome1 . #f)
                                 '(tmp4 . #f)
                                 '(tmp3 . #f)
                                 '(tmp2 . #f)
                                 '(Hand1 . #f)
                                 '(tmp1 . #f)
                                 '(tmp0 . #f)
                                 '(tmp . #f))
                          (@list '(tmp2 . #f) '(tmp3 . #f) '(tmp4 . #f) '(tmp . #f) '(tmp0 . #f) '(tmp1 . #f))
                          (@list)))
               (@list)))
      (end-switch (ci 'end-switch (@list) (@list) (@list)))
      (end-switch0 (ci 'end-switch0 (@list) (@list) (@list)))
      (end-switch1 (ci 'end-switch1 (@list) (@list) (@list)))
      (end-switch2 (ci 'end-switch2 (@list) (@list) (@list)))
      (end-switch3
       (ci 'end-switch3
           (@list 'outcome)
           (@list (ti 'begin-switch3
                      'end-switch3
                      #f
                      (@list (syntax (withdraw! B (@record (DefaultToken wagerAmount))))
                             (syntax (withdraw! A (@record (DefaultToken wagerAmount)))))
                      (@list)
                      (@list '(B . #f) '(wagerAmount . #f) '(A . #f) '(wagerAmount . #f) '(outcome . #f))
                      (@list '(A . #f) '(* . #f) '(wagerAmount . #f) '(B . #f)))
                  (ti 'begin-switch3
                      'end-switch3
                      #f
                      (@list (syntax (withdraw! B (@record (DefaultToken tmp20)))))
                      (@list '(tmp20 . #f))
                      (@list '(B . #f) '(tmp20 . #f) '(* . #f) '(wagerAmount . #f) '(outcome . #f))
                      (@list '(A . #f) '(* . #f) '(wagerAmount . #f) '(B . #f)))
                  (ti 'begin-switch3
                      'end-switch3
                      #f
                      (@list (syntax (withdraw! A (@record (DefaultToken tmp19)))))
                      (@list '(tmp19 . #f))
                      (@list '(A . #f) '(tmp19 . #f) '(* . #f) '(wagerAmount . #f) '(outcome . #f))
                      (@list '(A . #f) '(* . #f) '(wagerAmount . #f) '(B . #f))))
           (@list (ti 'end-switch3 'end7 #f (@list) (@list) (@list '(outcome . #f)) (@list '(outcome . #f))))))
      (end0 (ci 'end0 (@list) (@list (ti 'begin0 'end0 #f (@list) (@list '(x . #f)) (@list '(x . #f) '(tag . #f)) (@list '(tag . #f)))) (@list)))
      (end1 (ci 'end1
                (@list)
                (@list (ti 'begin-switch 'end1 #f (@list) (@list) (@list '(x0 . #f)) (@list '(x0 . #f)))
                       (ti 'begin-switch 'end1 #f (@list) (@list) (@list '(x0 . #f)) (@list '(x0 . #f)))
                       (ti 'begin-switch 'end1 #f (@list) (@list) (@list '(x0 . #f)) (@list '(x0 . #f))))
                (@list)))
      (end2 (ci 'end2
                (@list)
                (@list (ti 'begin-switch0
                           'end2
                           #f
                           (@list)
                           (@list)
                           (@list '(Scissors . #f) '(x1 . #f))
                           (@list '(Rock . #f) '(x1 . #f) '(Paper . #f) '(Scissors . #f)))
                       (ti 'begin-switch0
                           'end2
                           #f
                           (@list)
                           (@list)
                           (@list '(Paper . #f) '(x1 . #f))
                           (@list '(Rock . #f) '(x1 . #f) '(Paper . #f) '(Scissors . #f)))
                       (ti 'begin-switch0
                           'end2
                           #f
                           (@list)
                           (@list)
                           (@list '(Rock . #f) '(x1 . #f))
                           (@list '(Rock . #f) '(x1 . #f) '(Paper . #f) '(Scissors . #f))))
                (@list)))
      (end3 (ci 'end3 (@list) (@list (ti 'begin3 'end3 #f (@list) (@list '(x2 . #f)) (@list '(x2 . #f) '(tag0 . #f)) (@list '(tag0 . #f)))) (@list)))
      (end4 (ci 'end4
                (@list)
                (@list (ti 'begin-switch1 'end4 #f (@list) (@list) (@list '(x3 . #f)) (@list '(x3 . #f)))
                       (ti 'begin-switch1 'end4 #f (@list) (@list) (@list '(x3 . #f)) (@list '(x3 . #f)))
                       (ti 'begin-switch1 'end4 #f (@list) (@list) (@list '(x3 . #f)) (@list '(x3 . #f))))
                (@list)))
      (end5 (ci 'end5
                (@list)
                (@list (ti 'begin-switch2
                           'end5
                           #f
                           (@list)
                           (@list)
                           (@list '(A_Wins . #f) '(x4 . #f))
                           (@list '(B_Wins . #f) '(x4 . #f) '(Draw . #f) '(A_Wins . #f)))
                       (ti 'begin-switch2 'end5 #f (@list) (@list) (@list '(Draw . #f) '(x4 . #f)) (@list '(B_Wins . #f) '(x4 . #f) '(Draw . #f) '(A_Wins . #f)))
                       (ti 'begin-switch2
                           'end5
                           #f
                           (@list)
                           (@list)
                           (@list '(B_Wins . #f) '(x4 . #f))
                           (@list '(B_Wins . #f) '(x4 . #f) '(Draw . #f) '(A_Wins . #f))))
                (@list)))
      (end6 (ci 'end6
                (@list)
                (@list (ti 'begin6
                           'end6
                           #f
                           (@list)
                           (@list '(tmp12 . #f) '(tmp11 . #f) '(tmp10 . #f) '(tmp9 . #f) '(tmp8 . #f) '(tmp7 . #f) '(tmp6 . #f) '(tmp5 . #f))
                           (@list '(tmp5 . #f)
                                  '(tmp12 . #f)
                                  '(mod . #f)
                                  '(tmp11 . #f)
                                  '(+ . #f)
                                  '(tmp7 . #f)
                                  '(tmp10 . #f)
                                  '(- . #f)
                                  '(tmp9 . #f)
                                  '(tmp8 . #f)
                                  '(handB . #f)
                                  '(Hand1 . #f)
                                  '(tmp6 . #f)
                                  '(handA . #f)
                                  '(Hand1 . #f)
                                  '(Outcome1 . #f))
                           (@list '(mod . #f) '(+ . #f) '(- . #f) '(handB . #f) '(Hand1 . #f) '(handA . #f) '(Outcome1 . #f))))
                (@list)))
      (end7 (ci 'end7 (@list) (@list (ti 'end-switch3 'end7 #f (@list) (@list) (@list '(outcome . #f)) (@list '(outcome . #f)))) (@list))))

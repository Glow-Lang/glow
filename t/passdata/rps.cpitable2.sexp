(hash (begin
        (ci 'begin
            []
            []
            [(ti 'begin
                 'end
                 #f
                 []
                 ['(rockPaperScissors . #f)
                  '(winner . #f)
                  '(Outcome1 . #f)
                  '(tmp4 . #f)
                  '(tmp3 . #f)
                  '(tmp2 . #f)
                  '(Hand1 . #f)
                  '(tmp1 . #f)
                  '(tmp0 . #f)
                  '(tmp . #f)]
                 ['(tmp2 . #f)
                  '(tmp3 . #f)
                  '(tmp4 . #f)
                  '(tmp . #f)
                  '(tmp0 . #f)
                  '(tmp1 . #f)]
                 [])]))
      (begin-switch
       (ci 'begin-switch
           ['x1]
           [(ti 'begin1 'begin-switch #f [] [] [] [])]
           [(ti 'begin-switch 'end1 #f [] [] ['(x1 . #f)] ['(x1 . #f)])
            (ti 'begin-switch 'end1 #f [] [] ['(x1 . #f)] ['(x1 . #f)])
            (ti 'begin-switch 'end1 #f [] [] ['(x1 . #f)] ['(x1 . #f)])]))
      (begin-switch0
       (ci 'begin-switch0
           ['Rock 'x0 'Paper 'Scissors]
           [(ti 'begin2 'begin-switch0 #f [] [] [] [])]
           [(ti 'begin-switch0
                'end2
                #f
                []
                []
                ['(Scissors . #f) '(x0 . #f)]
                ['(Rock . #f) '(x0 . #f) '(Paper . #f) '(Scissors . #f)])
            (ti 'begin-switch0
                'end2
                #f
                []
                []
                ['(Paper . #f) '(x0 . #f)]
                ['(Rock . #f) '(x0 . #f) '(Paper . #f) '(Scissors . #f)])
            (ti 'begin-switch0
                'end2
                #f
                []
                []
                ['(Rock . #f) '(x0 . #f)]
                ['(Rock . #f) '(x0 . #f) '(Paper . #f) '(Scissors . #f)])]))
      (begin-switch1
       (ci 'begin-switch1
           ['x4]
           [(ti 'begin4 'begin-switch1 #f [] [] [] [])]
           [(ti 'begin-switch1 'end4 #f [] [] ['(x4 . #f)] ['(x4 . #f)])
            (ti 'begin-switch1 'end4 #f [] [] ['(x4 . #f)] ['(x4 . #f)])
            (ti 'begin-switch1 'end4 #f [] [] ['(x4 . #f)] ['(x4 . #f)])]))
      (begin-switch2
       (ci 'begin-switch2
           ['B_Wins 'x3 'Draw 'A_Wins]
           [(ti 'begin5 'begin-switch2 #f [] [] [] [])]
           [(ti 'begin-switch2
                'end5
                #f
                []
                []
                ['(A_Wins . #f) '(x3 . #f)]
                ['(B_Wins . #f) '(x3 . #f) '(Draw . #f) '(A_Wins . #f)])
            (ti 'begin-switch2
                'end5
                #f
                []
                []
                ['(Draw . #f) '(x3 . #f)]
                ['(B_Wins . #f) '(x3 . #f) '(Draw . #f) '(A_Wins . #f)])
            (ti 'begin-switch2
                'end5
                #f
                []
                []
                ['(B_Wins . #f) '(x3 . #f)]
                ['(B_Wins . #f) '(x3 . #f) '(Draw . #f) '(A_Wins . #f)])]))
      (begin-switch3
       (ci 'begin-switch3
           ['A '* 'wagerAmount 'outcome 'B]
           [(ti 'cp1
                'begin-switch3
                'A
                [#'(publish! A handA0) #'(publish! A salt)]
                ['(outcome . #f)
                 '(tmp18 . #f)
                 '(tmp17 . #f)
                 '(tmp16 . #f)
                 '(handA0 . #f)
                 '(salt . #f)]
                ['(winner . #f)
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
                 '(A . #f)]
                ['(winner . #f) '(handB0 . #f) '(commitment . #f)])]
           [(ti 'begin-switch3
                'end-switch3
                #f
                [#'(withdraw! B (@record (DefaultToken wagerAmount)))
                 #'(withdraw! A (@record (DefaultToken wagerAmount)))]
                []
                ['(B . #f)
                 '(wagerAmount . #f)
                 '(A . #f)
                 '(wagerAmount . #f)
                 '(outcome . #f)]
                ['(A . #f) '(* . #f) '(wagerAmount . #f) '(B . #f)])
            (ti 'begin-switch3
                'end-switch3
                #f
                [#'(withdraw! B (@record (DefaultToken tmp20)))]
                ['(tmp20 . #f)]
                ['(B . #f)
                 '(tmp20 . #f)
                 '(* . #f)
                 '(wagerAmount . #f)
                 '(outcome . #f)]
                ['(A . #f) '(* . #f) '(wagerAmount . #f) '(B . #f)])
            (ti 'begin-switch3
                'end-switch3
                #f
                [#'(withdraw! A (@record (DefaultToken tmp19)))]
                ['(tmp19 . #f)]
                ['(A . #f)
                 '(tmp19 . #f)
                 '(* . #f)
                 '(wagerAmount . #f)
                 '(outcome . #f)]
                ['(A . #f) '(* . #f) '(wagerAmount . #f) '(B . #f)])]))
      (begin0 (ci 'begin0
                  ['tag]
                  []
                  [(ti 'begin0
                       'end0
                       #f
                       []
                       ['(x . #f)]
                       ['(x . #f) '(tag . #f)]
                       ['(tag . #f)])]))
      (begin1 (ci 'begin1
                  ['x1]
                  []
                  [(ti 'begin1 'begin-switch #f [] [] [] [])]))
      (begin2 (ci 'begin2
                  ['Rock 'x0 'Paper 'Scissors]
                  []
                  [(ti 'begin2 'begin-switch0 #f [] [] [] [])]))
      (begin3 (ci 'begin3
                  ['tag0]
                  []
                  [(ti 'begin3
                       'end3
                       #f
                       []
                       ['(x2 . #f)]
                       ['(x2 . #f) '(tag0 . #f)]
                       ['(tag0 . #f)])]))
      (begin4 (ci 'begin4
                  ['x4]
                  []
                  [(ti 'begin4 'begin-switch1 #f [] [] [] [])]))
      (begin5 (ci 'begin5
                  ['B_Wins 'x3 'Draw 'A_Wins]
                  []
                  [(ti 'begin5 'begin-switch2 #f [] [] [] [])]))
      (begin6 (ci 'begin6
                  ['mod '+ '- 'handB 'Hand1 'handA 'Outcome1]
                  []
                  [(ti 'begin6
                       'end6
                       #f
                       []
                       ['(tmp12 . #f)
                        '(tmp11 . #f)
                        '(tmp10 . #f)
                        '(tmp9 . #f)
                        '(tmp8 . #f)
                        '(tmp7 . #f)
                        '(tmp6 . #f)
                        '(tmp5 . #f)]
                       ['(tmp5 . #f)
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
                        '(Outcome1 . #f)]
                       ['(mod . #f)
                        '(+ . #f)
                        '(- . #f)
                        '(handB . #f)
                        '(Hand1 . #f)
                        '(handA . #f)
                        '(Outcome1 . #f)])]))
      (begin7 (ci 'begin7
                  ['winner 'A '* 'wagerAmount 'B]
                  []
                  [(ti 'begin7 'cp #f [] [] [] [])]))
      (cp (ci 'cp
              ['winner 'A '* 'wagerAmount 'B]
              [(ti 'begin7 'cp #f [] [] [] [])]
              [(ti 'cp
                   'cp0
                   'A
                   [#'(deposit! A (@record (DefaultToken wagerAmount)))
                    #'(publish! A commitment)]
                   ['(commitment . #f)]
                   ['(A . #f) '(wagerAmount . #f) '(commitment . A) '(A . #f)]
                   [])]))
      (cp0 (ci 'cp0
               ['winner 'commitment 'A '* 'wagerAmount 'B]
               [(ti 'cp
                    'cp0
                    'A
                    [#'(deposit! A (@record (DefaultToken wagerAmount)))
                     #'(publish! A commitment)]
                    ['(commitment . #f)]
                    ['(A . #f) '(wagerAmount . #f) '(commitment . A) '(A . #f)]
                    [])]
               [(ti 'cp0
                    'cp1
                    'B
                    [#'(deposit! B (@record (DefaultToken wagerAmount)))
                     #'(publish! B handB0)]
                    ['(handB0 . #f)]
                    ['(B . #f) '(wagerAmount . #f) '(handB0 . B) '(B . #f)]
                    [])]))
      (cp1 (ci 'cp1
               ['winner 'handB0 'commitment 'A '* 'wagerAmount 'B]
               [(ti 'cp0
                    'cp1
                    'B
                    [#'(deposit! B (@record (DefaultToken wagerAmount)))
                     #'(publish! B handB0)]
                    ['(handB0 . #f)]
                    ['(B . #f) '(wagerAmount . #f) '(handB0 . B) '(B . #f)]
                    [])]
               [(ti 'cp1
                    'begin-switch3
                    'A
                    [#'(publish! A handA0) #'(publish! A salt)]
                    ['(outcome . #f)
                     '(tmp18 . #f)
                     '(tmp17 . #f)
                     '(tmp16 . #f)
                     '(handA0 . #f)
                     '(salt . #f)]
                    ['(winner . #f)
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
                     '(A . #f)]
                    ['(winner . #f) '(handB0 . #f) '(commitment . #f)])]))
      (end (ci 'end
               []
               [(ti 'begin
                    'end
                    #f
                    []
                    ['(rockPaperScissors . #f)
                     '(winner . #f)
                     '(Outcome1 . #f)
                     '(tmp4 . #f)
                     '(tmp3 . #f)
                     '(tmp2 . #f)
                     '(Hand1 . #f)
                     '(tmp1 . #f)
                     '(tmp0 . #f)
                     '(tmp . #f)]
                    ['(tmp2 . #f)
                     '(tmp3 . #f)
                     '(tmp4 . #f)
                     '(tmp . #f)
                     '(tmp0 . #f)
                     '(tmp1 . #f)]
                    [])]
               []))
      (end-switch (ci 'end-switch [] [] []))
      (end-switch0 (ci 'end-switch0 [] [] []))
      (end-switch1 (ci 'end-switch1 [] [] []))
      (end-switch2 (ci 'end-switch2 [] [] []))
      (end-switch3
       (ci 'end-switch3
           ['outcome]
           [(ti 'begin-switch3
                'end-switch3
                #f
                [#'(withdraw! B (@record (DefaultToken wagerAmount)))
                 #'(withdraw! A (@record (DefaultToken wagerAmount)))]
                []
                ['(B . #f)
                 '(wagerAmount . #f)
                 '(A . #f)
                 '(wagerAmount . #f)
                 '(outcome . #f)]
                ['(A . #f) '(* . #f) '(wagerAmount . #f) '(B . #f)])
            (ti 'begin-switch3
                'end-switch3
                #f
                [#'(withdraw! B (@record (DefaultToken tmp20)))]
                ['(tmp20 . #f)]
                ['(B . #f)
                 '(tmp20 . #f)
                 '(* . #f)
                 '(wagerAmount . #f)
                 '(outcome . #f)]
                ['(A . #f) '(* . #f) '(wagerAmount . #f) '(B . #f)])
            (ti 'begin-switch3
                'end-switch3
                #f
                [#'(withdraw! A (@record (DefaultToken tmp19)))]
                ['(tmp19 . #f)]
                ['(A . #f)
                 '(tmp19 . #f)
                 '(* . #f)
                 '(wagerAmount . #f)
                 '(outcome . #f)]
                ['(A . #f) '(* . #f) '(wagerAmount . #f) '(B . #f)])]
           [(ti 'end-switch3
                'end7
                #f
                []
                []
                ['(outcome . #f)]
                ['(outcome . #f)])]))
      (end0 (ci 'end0
                []
                [(ti 'begin0
                     'end0
                     #f
                     []
                     ['(x . #f)]
                     ['(x . #f) '(tag . #f)]
                     ['(tag . #f)])]
                []))
      (end1 (ci 'end1
                []
                [(ti 'begin-switch 'end1 #f [] [] ['(x1 . #f)] ['(x1 . #f)])
                 (ti 'begin-switch 'end1 #f [] [] ['(x1 . #f)] ['(x1 . #f)])
                 (ti 'begin-switch 'end1 #f [] [] ['(x1 . #f)] ['(x1 . #f)])]
                []))
      (end2 (ci 'end2
                []
                [(ti 'begin-switch0
                     'end2
                     #f
                     []
                     []
                     ['(Scissors . #f) '(x0 . #f)]
                     ['(Rock . #f) '(x0 . #f) '(Paper . #f) '(Scissors . #f)])
                 (ti 'begin-switch0
                     'end2
                     #f
                     []
                     []
                     ['(Paper . #f) '(x0 . #f)]
                     ['(Rock . #f) '(x0 . #f) '(Paper . #f) '(Scissors . #f)])
                 (ti 'begin-switch0
                     'end2
                     #f
                     []
                     []
                     ['(Rock . #f) '(x0 . #f)]
                     ['(Rock . #f) '(x0 . #f) '(Paper . #f) '(Scissors . #f)])]
                []))
      (end3 (ci 'end3
                []
                [(ti 'begin3
                     'end3
                     #f
                     []
                     ['(x2 . #f)]
                     ['(x2 . #f) '(tag0 . #f)]
                     ['(tag0 . #f)])]
                []))
      (end4 (ci 'end4
                []
                [(ti 'begin-switch1 'end4 #f [] [] ['(x4 . #f)] ['(x4 . #f)])
                 (ti 'begin-switch1 'end4 #f [] [] ['(x4 . #f)] ['(x4 . #f)])
                 (ti 'begin-switch1 'end4 #f [] [] ['(x4 . #f)] ['(x4 . #f)])]
                []))
      (end5 (ci 'end5
                []
                [(ti 'begin-switch2
                     'end5
                     #f
                     []
                     []
                     ['(A_Wins . #f) '(x3 . #f)]
                     ['(B_Wins . #f) '(x3 . #f) '(Draw . #f) '(A_Wins . #f)])
                 (ti 'begin-switch2
                     'end5
                     #f
                     []
                     []
                     ['(Draw . #f) '(x3 . #f)]
                     ['(B_Wins . #f) '(x3 . #f) '(Draw . #f) '(A_Wins . #f)])
                 (ti 'begin-switch2
                     'end5
                     #f
                     []
                     []
                     ['(B_Wins . #f) '(x3 . #f)]
                     ['(B_Wins . #f) '(x3 . #f) '(Draw . #f) '(A_Wins . #f)])]
                []))
      (end6 (ci 'end6
                []
                [(ti 'begin6
                     'end6
                     #f
                     []
                     ['(tmp12 . #f)
                      '(tmp11 . #f)
                      '(tmp10 . #f)
                      '(tmp9 . #f)
                      '(tmp8 . #f)
                      '(tmp7 . #f)
                      '(tmp6 . #f)
                      '(tmp5 . #f)]
                     ['(tmp5 . #f)
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
                      '(Outcome1 . #f)]
                     ['(mod . #f)
                      '(+ . #f)
                      '(- . #f)
                      '(handB . #f)
                      '(Hand1 . #f)
                      '(handA . #f)
                      '(Outcome1 . #f)])]
                []))
      (end7 (ci 'end7
                []
                [(ti 'end-switch3
                     'end7
                     #f
                     []
                     []
                     ['(outcome . #f)]
                     ['(outcome . #f)])]
                [])))

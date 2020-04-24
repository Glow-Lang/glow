(symdict ('Outcome (entry:type #f (@list) (type:name 'Outcome0)))
         ('Hand (entry:type #f (@list) (type:name 'Hand0)))
         ('B_Wins (entry:ctor #f (typing-scheme (symdict) (type:name 'Outcome0))))
         ('Draw (entry:ctor #f (typing-scheme (symdict) (type:name 'Outcome0))))
         ('Scissors (entry:ctor #f (typing-scheme (symdict) (type:name 'Hand0))))
         ('Rock (entry:ctor #f (typing-scheme (symdict) (type:name 'Hand0))))
         ('rockPaperScissors
          (entry:known
           #f
           (typing-scheme
            (symdict)
            (type:arrow
             (@list (type:name 'Participant)
                    (type:name 'Participant)
                    (ntype:intersection
                     (@list (type:var 'wagerAmount0)
                            (type:var 'wagerAmount1)
                            (type:var 'wagerAmount2)
                            (type:var 'wagerAmount3)
                            (type:var 'wagerAmount4)
                            (type:var 'wagerAmount5)
                            (type:name 'int))))
             (type:name 'Outcome0)))))
         ('Paper (entry:ctor #f (typing-scheme (symdict) (type:name 'Hand0))))
         ('winner
          (entry:known
           #f
           (typing-scheme
            (symdict)
            (type:arrow (@list (type:name 'Hand0) (type:name 'Hand0)) (type:name 'Outcome0)))))
         ('A_Wins (entry:ctor #f (typing-scheme (symdict) (type:name 'Outcome0)))))

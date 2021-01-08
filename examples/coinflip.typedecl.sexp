(symdict ('coinFlip
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
                            (type:name 'Int)))
                    (ntype:intersection
                     (@list (type:var 'escrowAmount0)
                            (type:var 'escrowAmount1)
                            (type:var 'escrowAmount2)
                            (type:name 'Int))))
             (type:tuple (@list)))))))

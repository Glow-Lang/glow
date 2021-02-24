(symdict ('g
          (entry:known
           #f
           (typing-scheme
            (symdict)
            (type:arrow
             (@list (ntype:intersection (@list (type:var 'a3) (type:name 'Int))))
             (type:name 'Int)))))
         ('f
          (entry:known
           #f
           (typing-scheme
            (symdict)
            (type:arrow
             (@list (ntype:intersection (@list (type:var 'a0) (type:name 'Int))))
             (type:name 'Int)))))
         ('inter
          (entry:known
           #f
           (typing-scheme
            (symdict)
            (type:arrow
             (@list (type:name 'Participant)
                    (type:name 'Participant)
                    (ntype:intersection (@list (type:var 'b0) (type:name 'Bool)))
                    (ntype:intersection
                     (@list (type:var 'x0)
                            (type:var 'x1)
                            (type:var 'a0)
                            (type:var 'x2)
                            (type:var 'a3)
                            (type:name 'Int))))
             (type:tuple (@list)))))))

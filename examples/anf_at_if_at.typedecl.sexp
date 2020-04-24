(symdict ('g
          (entry:known
           #f
           (typing-scheme
            (symdict)
            (type:arrow
             (@list (ntype:intersection (@list (type:var 'a3) (type:name 'int))))
             (type:name 'int)))))
         ('f
          (entry:known
           #f
           (typing-scheme
            (symdict)
            (type:arrow
             (@list (ntype:intersection (@list (type:var 'a0) (type:name 'int))))
             (type:name 'int)))))
         ('inter
          (entry:known
           #f
           (typing-scheme
            (symdict)
            (type:arrow
             (@list (type:name 'Participant)
                    (type:name 'Participant)
                    (ntype:intersection (@list (type:var 'b0) (type:name 'bool)))
                    (ntype:intersection
                     (@list (type:var 'x1)
                            (type:var 'a0)
                            (type:var 'x0)
                            (type:var 'a3)
                            (type:name 'int))))
             (type:tuple (@list)))))))

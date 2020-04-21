(symdict ('g
          (entry:known
           #f
           (typing-scheme
            (symdict)
            (type:arrow
             (@list (ntype:intersection (@list (type:var 'a3) (type:name 'int (@list)))))
             (type:name 'int (@list))))))
         ('f
          (entry:known
           #f
           (typing-scheme
            (symdict)
            (type:arrow
             (@list (ntype:intersection (@list (type:var 'a0) (type:name 'int (@list)))))
             (type:name 'int (@list))))))
         ('inter
          (entry:known
           #f
           (typing-scheme
            (symdict)
            (type:arrow
             (@list (type:name 'Participant (@list))
                    (type:name 'Participant (@list))
                    (ntype:intersection (@list (type:var 'b0) (type:name 'bool (@list))))
                    (ntype:intersection
                     (@list (type:var 'x1)
                            (type:var 'a0)
                            (type:var 'x0)
                            (type:var 'a3)
                            (type:name 'int (@list)))))
             (type:tuple (@list)))))))

(symdict ('Wrap
          (entry:ctor
           #f
           (typing-scheme (symdict) (type:arrow (@list (type:var 'aₚ)) (type:app (type:name 'wrap0) (@list (@list (type:var 'aₙ) (type:var 'aₚ))))))))
         ('unwrap0
          (entry:known
           #f
           (typing-scheme
            (symdict)
            (type:arrow
             (@list (ntype:intersection (@list (type:var 'w3) (type:app (type:name 'wrap0) (@list (@list (type:var 'aₙ) (type:var 'aₚ)))))))
             (type:tuple (@list))))))
         ('unwrap1
          (entry:known
           #f
           (typing-scheme
            (symdict)
            (type:arrow
             (@list (ntype:intersection
                     (@list (type:var 'w4)
                            (type:app
                             (type:name 'wrap0)
                             (@list (@list (type:var 'aₙ) (ntype:intersection (@list (type:var 'aₚ) (type:var 'v0) (type:var 'v3))))))
                            (type:app
                             (type:name 'wrap0)
                             (@list (@list (type:var 'aₙ) (ntype:intersection (@list (type:var 'aₚ) (type:var 'v0)))))))))
             (type:var 'v3)))))
         ('unwrap2
          (entry:known
           #f
           (typing-scheme
            (symdict)
            (type:arrow
             (@list (ntype:intersection
                     (@list (type:var 'w5)
                            (type:app
                             (type:name 'wrap0)
                             (@list (@list (type:var 'aₙ) (ntype:intersection (@list (type:var 'aₚ) (type:var 'v4) (type:var 'v5) (type:name 'Int))))))
                            (type:app (type:name 'wrap0) (@list (@list (type:var 'aₙ) (ntype:intersection (@list (type:var 'aₚ) (type:var 'v4)))))))))
             (type:name 'Int)))))
         ('wrap (entry:type #f (@list (@list 'aₙ 'aₚ)) (type:app (type:name 'wrap0) (@list (@list (type:var 'aₙ) (type:var 'aₚ)))))))

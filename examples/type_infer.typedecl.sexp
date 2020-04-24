(symdict ('self
          (entry:known
           #f
           (typing-scheme
            (symdict)
            (type:arrow
             (@list (ntype:intersection
                     (@list (type:var 'x3)
                            (type:arrow (@list (type:var 'x4)) (type:var 'x5))
                            (type:var 'x6)
                            (type:var 'x4))))
             (type:var 'x5)))))
         ('id_on_int
          (entry:known
           #f
           (typing-scheme
            (symdict)
            (type:arrow
             (@list (ntype:intersection
                     (@list (type:var 'x7) (type:name 'int) (type:var 'x8))))
             (type:var 'x8)))))
         ('select
          (entry:known
           #f
           (typing-scheme
            (symdict)
            (type:arrow
             (@list (ntype:intersection
                     (@list (type:var 'p0)
                            (type:arrow
                             (@list (type:var 'v1))
                             (ntype:intersection
                              (@list (type:var 'p1) (type:name 'bool))))))
                    (ntype:intersection (@list (type:var 'v2) (type:var 'v1) (type:var 'v0)))
                    (type:var 'd0))
             (ptype:union (@list (type:var 'v0) (type:var 'd0)))))))
         ('pick_one
          (entry:known
           #f
           (typing-scheme
            (symdict)
            (type:arrow
             (@list (ntype:intersection (@list (type:var 'b0) (type:name 'bool)))
                    (type:var 'x2)
                    (type:var 'y0))
             (ptype:union (@list (type:var 'x2) (type:var 'y0))))))))

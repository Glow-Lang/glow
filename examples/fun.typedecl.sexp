(symdict ('identity
          (entry:known
           #f
           (typing-scheme (symdict) (type:arrow (@list (type:var 'x2)) (type:var 'x2)))))
         ('konstant
          (entry:known
           #f
           (typing-scheme
            (symdict)
            (type:arrow (@list (type:var 'x3)) (type:arrow (@list ntype:top) (type:var 'x3))))))
         ('smelting
          (entry:known
           #f
           (typing-scheme
            (symdict)
            (type:arrow
             (@list (ntype:intersection
                     (@list (type:arrow (@list (type:var 'z1)) (type:var 'x5))
                            (type:var 'x4)
                            (type:arrow
                             (@list (type:var 'z1))
                             (ntype:intersection
                              (@list (type:var 'x5)
                                     (type:arrow (@list (type:var 'xz1)) (type:var 'xz0))))))))
             (type:arrow
              (@list (ntype:intersection
                      (@list (type:var 'y1)
                             (type:arrow
                              (@list (type:var 'z3))
                              (ntype:intersection (@list (type:var 'y2) (type:var 'xz1)))))))
              (type:arrow
               (@list (ntype:intersection
                       (@list (type:var 'z0) (type:var 'z1) (type:var 'z2) (type:var 'z3))))
               (type:var 'xz0))))))))

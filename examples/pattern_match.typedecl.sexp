(symdict ('No (entry:ctor #f (typing-scheme (symdict) (type:name 'ymn0))))
         ('App
          (entry:ctor
           #f
           (typing-scheme
            (symdict)
            (type:arrow
             (@list (type:name 'lcexpr0) (type:name 'lcexpr0))
             (type:name 'lcexpr0)))))
         ('ans (entry:known #f (typing-scheme (symdict) (type:name 'ymn0))))
         ('lcexpr (entry:type #f (@list) (type:name 'lcexpr0)))
         ('b (entry:known #f (typing-scheme (symdict) (type:name 'bool))))
         ('l
          (entry:known
           #f
           (typing-scheme
            (symdict)
            (type:app
             (type:name 'listof)
             (@list (type:name-subtype 'nat (type:name 'int)))))))
         ('omega (entry:known #f (typing-scheme (symdict) (type:name 'lcexpr0))))
         ('ymn (entry:type #f (@list) (type:name 'ymn0)))
         ('i
          (entry:known
           #f
           (typing-scheme (symdict) (type:name-subtype 'nat (type:name 'int)))))
         ('Var
          (entry:ctor
           #f
           (typing-scheme
            (symdict)
            (type:arrow (@list (type:name 'int)) (type:name 'lcexpr0)))))
         ('bs (entry:known #f (typing-scheme (symdict) (type:name 'bytes))))
         ('v
          (entry:known
           #f
           (typing-scheme
            (symdict)
            (type:record
             (symdict ('y (type:name-subtype 'nat (type:name 'int)))
                      ('x (type:name-subtype 'nat (type:name 'int))))))))
         ('Maybe (entry:ctor #f (typing-scheme (symdict) (type:name 'ymn0))))
         ('Yes (entry:ctor #f (typing-scheme (symdict) (type:name 'ymn0))))
         ('possible
          (entry:known
           #f
           (typing-scheme
            (symdict)
            (type:arrow (@list (type:name 'ymn0)) (type:name 'bool)))))
         ('p
          (entry:known
           #f
           (typing-scheme
            (symdict)
            (type:tuple
             (@list (type:name-subtype 'nat (type:name 'int))
                    (type:name-subtype 'nat (type:name 'int)))))))
         ('definite
          (entry:known
           #f
           (typing-scheme
            (symdict)
            (type:arrow (@list (type:name 'ymn0)) (type:name 'bool)))))
         ('Lam
          (entry:ctor
           #f
           (typing-scheme
            (symdict)
            (type:arrow (@list (type:name 'lcexpr0)) (type:name 'lcexpr0)))))
         ('freevars
          (entry:known
           #f
           (typing-scheme
            (symdict)
            (type:arrow
             (@list (type:name 'lcexpr0))
             (ptype:union
              (@list (type:app
                      (type:name 'listof)
                      (@list (type:name 'int)))
                     (type:app
                      (type:name 'listof)
                      (@list (type:name-subtype 'nat (type:name 'int)))))))))))

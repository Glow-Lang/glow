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
         ('b (entry:known #f (typing-scheme (symdict) (type:name 'Bool))))
         ('l
          (entry:known
           #f
           (typing-scheme
            (symdict)
            (type:app
             (type:name 'listof)
             (@list (@list #f (type:name-subtype 'Nat (type:name 'Int))))))))
         ('omega (entry:known #f (typing-scheme (symdict) (type:name 'lcexpr0))))
         ('ymn (entry:type #f (@list) (type:name 'ymn0)))
         ('i
          (entry:known
           #f
           (typing-scheme (symdict) (type:name-subtype 'Nat (type:name 'Int)))))
         ('Var
          (entry:ctor
           #f
           (typing-scheme
            (symdict)
            (type:arrow (@list (type:name 'Int)) (type:name 'lcexpr0)))))
         ('bs (entry:known #f (typing-scheme (symdict) (type:name 'Bytes))))
         ('v
          (entry:known
           #f
           (typing-scheme
            (symdict)
            (type:record
             (symdict ('y (type:name-subtype 'Nat (type:name 'Int)))
                      ('x (type:name-subtype 'Nat (type:name 'Int))))))))
         ('Maybe (entry:ctor #f (typing-scheme (symdict) (type:name 'ymn0))))
         ('Yes (entry:ctor #f (typing-scheme (symdict) (type:name 'ymn0))))
         ('possible
          (entry:known
           #f
           (typing-scheme
            (symdict)
            (type:arrow (@list (type:name 'ymn0)) (type:name 'Bool)))))
         ('p
          (entry:known
           #f
           (typing-scheme
            (symdict)
            (type:tuple
             (@list (type:name-subtype 'Nat (type:name 'Int))
                    (type:name-subtype 'Nat (type:name 'Int)))))))
         ('definite
          (entry:known
           #f
           (typing-scheme
            (symdict)
            (type:arrow (@list (type:name 'ymn0)) (type:name 'Bool)))))
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
                      (@list (@list #f (ptype:union (@list (type:var 'x8) (type:name 'Int))))))
                     (type:app
                      (type:name 'listof)
                      (@list (@list #f (type:name-subtype 'Nat (type:name 'Int))))))))))))

(symdict ('No (entry:ctor #f (typing-scheme (symdict) (type:name 'ymn0 (@list)))))
         ('App
          (entry:ctor
           #f
           (typing-scheme
            (symdict)
            (type:arrow
             (@list (type:name 'lcexpr0 (@list)) (type:name 'lcexpr0 (@list)))
             (type:name 'lcexpr0 (@list))))))
         ('ans (entry:known #f (typing-scheme (symdict) (type:name 'ymn0 (@list)))))
         ('lcexpr (entry:type #f (@list) (type:name 'lcexpr0 (@list))))
         ('b (entry:known #f (typing-scheme (symdict) (type:name 'bool (@list)))))
         ('l
          (entry:known
           #f
           (typing-scheme
            (symdict)
            (type:app
             (type:name 'listof (@list covariant))
             (@list (type:name-subtype 'nat (type:name 'int (@list))))))))
         ('omega (entry:known #f (typing-scheme (symdict) (type:name 'lcexpr0 (@list)))))
         ('ymn (entry:type #f (@list) (type:name 'ymn0 (@list))))
         ('i
          (entry:known
           #f
           (typing-scheme (symdict) (type:name-subtype 'nat (type:name 'int (@list))))))
         ('Var
          (entry:ctor
           #f
           (typing-scheme
            (symdict)
            (type:arrow (@list (type:name 'int (@list))) (type:name 'lcexpr0 (@list))))))
         ('bs (entry:known #f (typing-scheme (symdict) (type:name 'bytes (@list)))))
         ('v
          (entry:known
           #f
           (typing-scheme
            (symdict)
            (type:record
             (symdict ('y (type:name-subtype 'nat (type:name 'int (@list))))
                      ('x (type:name-subtype 'nat (type:name 'int (@list)))))))))
         ('Maybe (entry:ctor #f (typing-scheme (symdict) (type:name 'ymn0 (@list)))))
         ('Yes (entry:ctor #f (typing-scheme (symdict) (type:name 'ymn0 (@list)))))
         ('possible
          (entry:known
           #f
           (typing-scheme
            (symdict)
            (type:arrow (@list (type:name 'ymn0 (@list))) (type:name 'bool (@list))))))
         ('p
          (entry:known
           #f
           (typing-scheme
            (symdict)
            (type:tuple
             (@list (type:name-subtype 'nat (type:name 'int (@list)))
                    (type:name-subtype 'nat (type:name 'int (@list))))))))
         ('definite
          (entry:known
           #f
           (typing-scheme
            (symdict)
            (type:arrow (@list (type:name 'ymn0 (@list))) (type:name 'bool (@list))))))
         ('Lam
          (entry:ctor
           #f
           (typing-scheme
            (symdict)
            (type:arrow (@list (type:name 'lcexpr0 (@list))) (type:name 'lcexpr0 (@list))))))
         ('freevars
          (entry:known
           #f
           (typing-scheme
            (symdict)
            (type:arrow
             (@list (type:name 'lcexpr0 (@list)))
             (ptype:union
              (@list (type:app
                      (type:name 'listof (@list covariant))
                      (@list (type:name 'int (@list))))
                     (type:app
                      (type:name 'listof (@list covariant))
                      (@list (type:name-subtype 'nat (type:name 'int (@list))))))))))))

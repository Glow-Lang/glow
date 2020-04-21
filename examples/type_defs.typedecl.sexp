(symdict ('yn (entry:type #f (@list) (type:name 'yn0 (@list))))
         ('No (entry:ctor #f (typing-scheme (symdict) (type:name 'yn0 (@list)))))
         ('App
          (entry:ctor
           #f
           (typing-scheme
            (symdict)
            (type:arrow
             (@list (type:app (type:name 'lcexpr0 (@list invariant)) (@list (type:var 'lit)))
                    (type:app (type:name 'lcexpr0 (@list invariant)) (@list (type:var 'lit))))
             (type:app (type:name 'lcexpr0 (@list invariant)) (@list (type:var 'lit)))))))
         ('Lit
          (entry:ctor
           #f
           (typing-scheme
            (symdict)
            (type:arrow
             (@list (type:var 'lit))
             (type:app (type:name 'lcexpr0 (@list invariant)) (@list (type:var 'lit)))))))
         ('Pair
          (entry:ctor
           #f
           (typing-scheme
            (symdict)
            (type:arrow
             (@list (type:var 'a) (type:var 'b))
             (type:app
              (type:name 'pair0 (@list invariant invariant))
              (@list (type:var 'a) (type:var 'b)))))))
         ('EQ (entry:ctor #f (typing-scheme (symdict) (type:name 'ordering0 (@list)))))
         ('colorRGB
          (entry:type
           #f
           (@list)
           (type:record
            (symdict ('g (type:name 'int (@list)))
                     ('b (type:name 'int (@list)))
                     ('r (type:name 'int (@list)))))))
         ('lcexpr
          (entry:type
           #f
           (@list 'lit)
           (type:app (type:name 'lcexpr0 (@list invariant)) (@list (type:var 'lit)))))
         ('tuple_pair
          (entry:known
           #f
           (typing-scheme
            (symdict)
            (type:arrow
             (@list (type:tuple (@list (type:var 'a) (type:var 'b))))
             (type:app
              (type:name 'pair0 (@list invariant invariant))
              (@list (type:var 'a) (type:var 'b)))))))
         ('Ok
          (entry:ctor
           #f
           (typing-scheme
            (symdict)
            (type:arrow
             (@list (type:var 'a))
             (type:app
              (type:name 'result0 (@list invariant invariant))
              (@list (type:var 'a) (type:var 'b)))))))
         ('pair_tuple
          (entry:known
           #f
           (typing-scheme
            (symdict)
            (type:arrow
             (@list (type:app
                     (type:name 'pair0 (@list invariant invariant))
                     (@list (type:var 'a) (type:var 'b))))
             (type:tuple (@list (type:var 'a) (type:var 'b)))))))
         ('Cons
          (entry:ctor
           #f
           (typing-scheme
            (symdict)
            (type:arrow
             (@list (type:var 'a)
                    (type:app (type:name 'conslist0 (@list invariant)) (@list (type:var 'a))))
             (type:app (type:name 'conslist0 (@list invariant)) (@list (type:var 'a)))))))
         ('option
          (entry:type
           #f
           (@list 'a)
           (type:app (type:name 'option0 (@list invariant)) (@list (type:var 'a)))))
         ('assocpairlist
          (entry:type
           #f
           (@list 'a 'b)
           (type:app
            (type:name 'conslist0 (@list invariant))
            (@list (type:app
                    (type:name 'pair0 (@list invariant invariant))
                    (@list (type:var 'a) (type:var 'b)))))))
         ('result
          (entry:type
           #f
           (@list 'a 'b)
           (type:app
            (type:name 'result0 (@list invariant invariant))
            (@list (type:var 'a) (type:var 'b)))))
         ('GT (entry:ctor #f (typing-scheme (symdict) (type:name 'ordering0 (@list)))))
         ('Zero (entry:ctor #f (typing-scheme (symdict) (type:name 'natural0 (@list)))))
         ('Var
          (entry:ctor
           #f
           (typing-scheme
            (symdict)
            (type:arrow
             (@list (type:name 'natural0 (@list)))
             (type:app (type:name 'lcexpr0 (@list invariant)) (@list (type:var 'lit)))))))
         ('purelcexpr
          (entry:type
           #f
           (@list)
           (type:app
            (type:name 'lcexpr0 (@list invariant))
            (@list (type:name 'nothing0 (@list))))))
         ('None
          (entry:ctor
           #f
           (typing-scheme
            (symdict)
            (type:app (type:name 'option0 (@list invariant)) (@list (type:var 'a))))))
         ('ordering (entry:type #f (@list) (type:name 'ordering0 (@list))))
         ('Empty
          (entry:ctor
           #f
           (typing-scheme
            (symdict)
            (type:app (type:name 'conslist0 (@list invariant)) (@list (type:var 'a))))))
         ('lcintexpr
          (entry:type
           #f
           (@list)
           (type:app (type:name 'lcexpr0 (@list invariant)) (@list (type:name 'int (@list))))))
         ('nothing (entry:type #f (@list) (type:name 'nothing0 (@list))))
         ('option_result
          (entry:known
           #f
           (typing-scheme
            (symdict)
            (type:arrow
             (@list (type:app (type:name 'option0 (@list invariant)) (@list (type:var 'a))))
             (type:app
              (type:name 'result0 (@list invariant invariant))
              (@list (type:var 'a) (type:tuple (@list))))))))
         ('Some
          (entry:ctor
           #f
           (typing-scheme
            (symdict)
            (type:arrow
             (@list (type:var 'a))
             (type:app (type:name 'option0 (@list invariant)) (@list (type:var 'a)))))))
         ('Error
          (entry:ctor
           #f
           (typing-scheme
            (symdict)
            (type:arrow
             (@list (type:var 'b))
             (type:app
              (type:name 'result0 (@list invariant invariant))
              (@list (type:var 'a) (type:var 'b)))))))
         ('Yes (entry:ctor #f (typing-scheme (symdict) (type:name 'yn0 (@list)))))
         ('pos2d (entry:type #f (@list) (type:name 'pos2d0 (@list))))
         ('conslist
          (entry:type
           #f
           (@list 'a)
           (type:app (type:name 'conslist0 (@list invariant)) (@list (type:var 'a)))))
         ('Posn
          (entry:ctor
           #f
           (typing-scheme
            (symdict)
            (type:arrow
             (@list (type:name 'int (@list)) (type:name 'int (@list)))
             (type:name 'pos2d0 (@list))))))
         ('Lam
          (entry:ctor
           #f
           (typing-scheme
            (symdict)
            (type:arrow
             (@list (type:app (type:name 'lcexpr0 (@list invariant)) (@list (type:var 'lit))))
             (type:app (type:name 'lcexpr0 (@list invariant)) (@list (type:var 'lit)))))))
         ('pair
          (entry:type
           #f
           (@list 'a 'b)
           (type:app
            (type:name 'pair0 (@list invariant invariant))
            (@list (type:var 'a) (type:var 'b)))))
         ('LT (entry:ctor #f (typing-scheme (symdict) (type:name 'ordering0 (@list)))))
         ('Succ
          (entry:ctor
           #f
           (typing-scheme
            (symdict)
            (type:arrow (@list (type:name 'natural0 (@list))) (type:name 'natural0 (@list))))))
         ('natural (entry:type #f (@list) (type:name 'natural0 (@list)))))

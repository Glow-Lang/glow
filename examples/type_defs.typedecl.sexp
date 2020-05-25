(symdict ('yn (entry:type #f (@list) (type:name 'yn0)))
         ('No (entry:ctor #f (typing-scheme (symdict) (type:name 'yn0))))
         ('App
          (entry:ctor
           #f
           (typing-scheme
            (symdict)
            (type:arrow
             (@list (type:app (type:name 'lcexpr0) (@list (@list (type:var 'litₙ) (type:var 'litₚ))))
                    (type:app (type:name 'lcexpr0) (@list (@list (type:var 'litₙ) (type:var 'litₚ)))))
             (type:app (type:name 'lcexpr0) (@list (@list (type:var 'litₙ) (type:var 'litₚ))))))))
         ('Lit
          (entry:ctor
           #f
           (typing-scheme
            (symdict)
            (type:arrow
             (@list (type:var 'litₚ))
             (type:app (type:name 'lcexpr0) (@list (@list (type:var 'litₙ) (type:var 'litₚ))))))))
         ('Pair
          (entry:ctor
           #f
           (typing-scheme
            (symdict)
            (type:arrow
             (@list (type:var 'aₚ) (type:var 'bₚ))
             (type:app
              (type:name 'pair0)
              (@list (@list (type:var 'aₙ) (type:var 'aₚ)) (@list (type:var 'bₙ) (type:var 'bₚ))))))))
         ('EQ (entry:ctor #f (typing-scheme (symdict) (type:name 'ordering0))))
         ('colorRGB
          (entry:type
           #f
           (@list)
           (type:record
            (symdict ('g (type:name 'int))
                     ('b (type:name 'int))
                     ('r (type:name 'int))))))
         ('lcexpr
          (entry:type
           #f
           (@list (@list 'litₙ 'litₚ))
           (type:app (type:name 'lcexpr0) (@list (@list (type:var 'litₙ) (type:var 'litₚ))))))
         ('tuple_pair
          (entry:known
           #f
           (typing-scheme
            (symdict)
            (type:arrow
             (@list (type:tuple (@list (type:var 'a) (type:var 'b))))
             (type:app
              (type:name 'pair0)
              (@list (@list (ntype:intersection (@list (type:var 'a) (type:var 'aₙ))) (type:var 'a)) (@list (ntype:intersection (@list (type:var 'b) (type:var 'bₙ))) (type:var 'b))))))))
         ('Ok
          (entry:ctor
           #f
           (typing-scheme
            (symdict)
            (type:arrow
             (@list (type:var 'aₚ1))
             (type:app
              (type:name 'result0)
              (@list (@list (type:var 'aₙ1) (type:var 'aₚ1)) (@list (type:var 'bₙ0) (type:var 'bₚ0))))))))
         ('pair_tuple
          (entry:known
           #f
           (typing-scheme
            (symdict)
            (type:arrow
             (@list (type:app
                     (type:name 'pair0)
                     (@list (@list (type:var 'a) (type:var 'a)) (@list (type:var 'b) (type:var 'b)))))
             (type:tuple (@list (type:var 'a) (type:var 'b)))))))
         ('Cons
          (entry:ctor
           #f
           (typing-scheme
            (symdict)
            (type:arrow
             (@list (type:var 'aₚ2)
                    (type:app (type:name 'conslist0) (@list (@list (type:var 'aₙ2) (type:var 'aₚ2)))))
             (type:app (type:name 'conslist0) (@list (@list (type:var 'aₙ2) (type:var 'aₚ2))))))))
         ('option
          (entry:type
           #f
           (@list (@list 'aₙ0 'aₚ0))
           (type:app (type:name 'option0) (@list (@list (type:var 'aₙ0) (type:var 'aₚ0))))))
         ('assocpairlist
          (entry:type
           #f
           (@list (@list 'aₙ3 'aₚ3) (@list 'bₙ1 'bₚ1))
           (type:app
            (type:name 'conslist0)
            (@list (@list (type:app
                           (type:name 'pair0)
                           (@list (@list (type:var 'aₚ3) (type:var 'aₙ3)) (@list (type:var 'bₚ1) (type:var 'bₙ1))))
                          (type:app
                           (type:name 'pair0)
                           (@list (@list (type:var 'aₙ3) (type:var 'aₚ3)) (@list (type:var 'bₙ1) (type:var 'bₚ1)))))))))
         ('result
          (entry:type
           #f
           (@list (@list 'aₙ1 'aₚ1) (@list 'bₙ0 'bₚ0))
           (type:app
            (type:name 'result0)
            (@list (@list (type:var 'aₙ1) (type:var 'aₚ1)) (@list (type:var 'bₙ0) (type:var 'bₚ0))))))
         ('GT (entry:ctor #f (typing-scheme (symdict) (type:name 'ordering0))))
         ('Zero (entry:ctor #f (typing-scheme (symdict) (type:name 'natural0))))
         ('Var
          (entry:ctor
           #f
           (typing-scheme
            (symdict)
            (type:arrow
             (@list (type:name 'natural0))
             (type:app (type:name 'lcexpr0) (@list (@list (type:var 'litₙ) (type:var 'litₚ))))))))
         ('purelcexpr
          (entry:type
           #f
           (@list)
           (type:app
            (type:name 'lcexpr0)
            (@list (@list (type:name 'nothing0) (type:name 'nothing0))))))
         ('None
          (entry:ctor
           #f
           (typing-scheme
            (symdict)
            (type:app (type:name 'option0) (@list (@list (type:var 'aₙ0) (type:var 'aₚ0)))))))
         ('ordering (entry:type #f (@list) (type:name 'ordering0)))
         ('Empty
          (entry:ctor
           #f
           (typing-scheme
            (symdict)
            (type:app (type:name 'conslist0) (@list (@list (type:var 'aₙ2) (type:var 'aₚ2)))))))
         ('lcintexpr
          (entry:type
           #f
           (@list)
           (type:app (type:name 'lcexpr0) (@list (@list (type:name 'int) (type:name 'int))))))
         ('nothing (entry:type #f (@list) (type:name 'nothing0)))
         ('option_result
          (entry:known
           #f
           (typing-scheme
            (symdict)
            (type:arrow
             (@list (type:app (type:name 'option0) (@list (@list (type:var 'a) (type:var 'a)))))
             (type:app
              (type:name 'result0)
              (@list (@list (ntype:intersection (@list (type:var 'a) (type:var 'aₙ1))) (type:var 'a))
                     (@list (type:tuple (@list)) (type:tuple (@list)))))))))
         ('Some
          (entry:ctor
           #f
           (typing-scheme
            (symdict)
            (type:arrow
             (@list (type:var 'aₚ0))
             (type:app (type:name 'option0) (@list (@list (type:var 'aₙ0) (type:var 'aₚ0))))))))
         ('Error
          (entry:ctor
           #f
           (typing-scheme
            (symdict)
            (type:arrow
             (@list (type:var 'bₚ0))
             (type:app
              (type:name 'result0)
              (@list (@list (type:var 'aₙ1) (type:var 'aₚ1)) (@list (type:var 'bₙ0) (type:var 'bₚ0))))))))
         ('Yes (entry:ctor #f (typing-scheme (symdict) (type:name 'yn0))))
         ('pos2d (entry:type #f (@list) (type:name 'pos2d0)))
         ('conslist
          (entry:type
           #f
           (@list (@list 'aₙ2 'aₚ2))
           (type:app (type:name 'conslist0) (@list (@list (type:var 'aₙ2) (type:var 'aₚ2))))))
         ('Posn
          (entry:ctor
           #f
           (typing-scheme
            (symdict)
            (type:arrow
             (@list (type:name 'int) (type:name 'int))
             (type:name 'pos2d0)))))
         ('Lam
          (entry:ctor
           #f
           (typing-scheme
            (symdict)
            (type:arrow
             (@list (type:app (type:name 'lcexpr0) (@list (@list (type:var 'litₙ) (type:var 'litₚ)))))
             (type:app (type:name 'lcexpr0) (@list (@list (type:var 'litₙ) (type:var 'litₚ))))))))
         ('pair
          (entry:type
           #f
           (@list (@list 'aₙ 'aₚ) (@list 'bₙ 'bₚ))
           (type:app
            (type:name 'pair0)
            (@list (@list (type:var 'aₙ) (type:var 'aₚ)) (@list (type:var 'bₙ) (type:var 'bₚ))))))
         ('LT (entry:ctor #f (typing-scheme (symdict) (type:name 'ordering0))))
         ('Succ
          (entry:ctor
           #f
           (typing-scheme
            (symdict)
            (type:arrow (@list (type:name 'natural0)) (type:name 'natural0)))))
         ('natural (entry:type #f (@list) (type:name 'natural0))))

(export #t)

(import :std/pregexp
        :std/misc/list
        :mukn/glow/compiler/common)

;; symbol-split : Symbol -> (values Symbol (U #f Nat))
(def (symbol-split sym)
  (def str (symbol->string sym))
  (match (pregexp-match-positions "\\d*$" str)
    ([] (values sym #f))
    ([(cons a b)]
     (cond ((= a b) (values sym #f))
           (else    (values (string->symbol (substring str 0 a))
                            (string->number (substring str a b))))))))

;; symbolnat : Symbol (U #f Nat) -> Symbol
(def (symbolnat s n)
  (cond (n (format-symbol "~a~a" s n))
        (else s)))

;; An UnusedList is a [Listof Nat]
;; If empty, the nat is unused.
;; Otherwise the elements are numbers that are unused and
;; the last one is the number such that everything above
;; it is unused.

;; unusedlist-unused? : UnusedList Nat -> Bool
(def (unusedlist-unused? ul n)
  (match ul
    ([] #t)
    ([x] (and n (<= x n)))
    ([x . rst]
     (cond ((or (not n) (< n x)) #f)
           ((= n x) #t)
           (else (unusedlist-unused? rst n))))))

;; unusedlist-first : UnusedList -> (U #f Nat)
(def (unusedlist-first ul)
  (match ul
    ([] #f)
    ([n . _] n)))

;; unusedlist-rest : UnusedList -> (U #f Nat)
(def (unusedlist-rest ul)
  (match ul
    ([] [0])
    ([n] [(1+ n)])
    ([_ . rst] rst)))

;; range : Int Int -> [Listof Int]
(def (range a b)
  (def ∆ (- b a))
  (if (<= 0 ∆) (iota ∆ a) (iota (- ∆) a -1)))

;; unusedlist-remove : UnusedList (U #f Nat) -> UnusedList
(def (unusedlist-remove ul n)
  (match ul
    ([] (cond (n (append1 (iota n) (1+ n)))
              (else [0])))
    ([x] (cond ((or (not n) (< n x)) ul)
               ((= n x) [(1+ x)])
               (else (append1 (range x n) (1+ n)))))
    ([x . rst] (cond ((or (not n) (< n x)) ul)
                     ((= n x) rst)
                     (else (cons x (unusedlist-remove rst n)))))))

(export #t)

(import :std/format
        :std/misc/list
        :clan/pure/dict/symdict
        "variance.ss")

;; A Type is one of:
;;  - (type:bottom)
;;  - (type:name Symbol [Listof Variance])  ;; TODO: add srcloc
;;  - (type:var Symbol)
;;  - (type:app Type [Listof Type])
;;  - (type:tuple [Listof Type])
;;  - (type:record [Symdictof Type])
;;  - (type:arrow [Listof Type] Type)
(defstruct type:bottom () transparent: #t)
(defstruct type:name (sym vances) transparent: #t)  ;; TODO: add srcloc
(defstruct type:var (sym) transparent: #t)
(defstruct type:app (fun args) transparent: #t)
(defstruct type:tuple (args) transparent: #t)
(defstruct type:record (field-args))
(defstruct type:arrow (in-tys out-ty) transparent: #t)
(def (type? v) (or (type:bottom? v) (type:name? v) (type:var? v) (type:app? v) (type:tuple? v) (type:record? v) (type:arrow? v)))

;; PTypes are types used for outputs, while NTypes are types used for inputs.
;; PTypes are extended to allow unions, while NTypes allow intersections.
;; PTypes represent lower bounds for values, while NTypes represent upper bounds for contexts.
;; subtype is PType <: NType
;; join is PType ⊔ PType
;; meet is NType ⊓ NType

;; A covariant position within a PType has a PType.
;; A covariant position within an NType has an NType.
;; A contravariant position within a PType has an NType.
;; A contravariant position within an NType has a PType.
;; An invariant position within either has a Type, with no unions or intersections.
(defstruct ptype:union (types) transparent: #t)
(defstruct ntype:intersection (types) transparent: #t)

(def type:int (type:name 'int []))
(def type:bool (type:name 'bool []))
(def type:bytes (type:name 'bytes []))

;; lists are covariant, so (listof a) <: (listof b) when a <: b
(def typector:listof (type:name 'listof [covariant]))
(def (type:listof t) (type:app typector:listof [t]))
(def (type:listof? t)
  (match t
    ((type:app c [_]) (eq? c typector:listof))
    (_ #f)))
(def (type:listof-elem t)
  (match t
    ((type:app c [e])
     (unless (eq? c typector:listof) (error 'listof-elem "expected a listof type"))
     e)))

;; --------------------------------------------------------

;; type-vars : Type -> [Listof Symbol]
(def (type-vars t)
  (match t
    ((type:bottom) [])
    ((type:name _ _) [])
    ((type:var s) [s])
    ((type:app f as)
     (append (type-vars f) (flatten1 (map type-vars as))))
    ((type:tuple as)
     (flatten1 (map type-vars as)))
    ((type:record flds)
     (flatten1 (map (lambda (k) (type-vars (symdict-ref flds k)))
                    (symdict-keys flds))))
    ((type:arrow as b)
     (append (flatten1 (map type-vars as)) (type-vars b)))
    ((ptype:union ts)
     (flatten1 (map type-vars ts)))
    ((ntype:intersection ts)
     (flatten1 (map type-vars ts)))))

;; type-has-var? : Type Symbol -> Bool
(def (type-has-var? t x)
  (def (hv? t) (type-has-var? t x))
  (match t
    ((type:bottom) #f)
    ((type:name s _) #f)
    ((type:var s) (eq? x s))
    ((type:app f as)
     (or (hv? f) (ormap hv? as)))
    ((type:tuple as)
     (ormap hv? as))
    ((type:record flds)
     (ormap (lambda (k) (hv? (symdict-ref flds k)))
            (symdict-keys flds)))
    ((type:arrow as b)
     (or (ormap hv? as) (hv? b)))
    ((ptype:union ts)
     (ormap hv? ts))
    ((ntype:intersection ts)
     (ormap hv? ts))))

;; type-subst : [Symdict Type] Type -> Type
(def (type-subst tyvars t)
  ;; sub : Type -> Type
  (def (sub t) (type-subst tyvars t))
  (match t
    ((type:bottom) t)
    ((type:name _ _) t)
    ((type:var s)
     (cond ((symdict-has-key? tyvars s) (symdict-ref tyvars s))
           (else t)))
    ((type:app f as)
     (type:app (sub f) (map sub as)))
    ((type:tuple as)
     (type:tuple (map sub as)))
    ((type:record fldtys)
     (type:record
      (list->symdict
       (map (lambda (p) (cons (car p) (sub (cdr p))))
            fldtys))))
    ((type:arrow as b)
     (type:arrow (map sub as) (sub b)))
    ((ptype:union ts)
     (ptype:union (map sub ts)))
    ((ntype:intersection ts)
     (ntype:intersection (map sub ts)))))

;; --------------------------------------------------------

;; type->sexpr : Type -> Sexpr
(def (type->sexpr t)
  (match t
    ((type:bottom) '⊥)
    ((type:name s _) s)
    ((type:var s) `',s)
    ((type:app f as) (cons (type->sexpr f) (map type->sexpr as)))
    ((type:tuple as) (cons '@tuple (map type->sexpr as)))
    ((type:record flds) (cons '@record
                              (map (lambda (k) [k (type->sexpr (symdict-ref flds k))])
                                   (symdict-keys flds))))
    ((type:arrow as b) (cons '-> (append1 (map type->sexpr as) (type->sexpr b))))
    ((ptype:union ts) (cons '∪ (map type->sexpr ts)))
    ((ntype:intersection ts) (cons '∩ (map type-vars ts)))))

;; type->string : Type -> String
(def (type->string t) (format "~y" (type->sexpr t)))

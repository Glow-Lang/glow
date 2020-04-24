(export #t)

(import :std/iter
        :std/format
        :std/misc/list
        :std/misc/repr
        :clan/pure/dict/symdict
        :clan/utils/debug
        ./variance)

;; A Type is one of:
;;  - (type:bottom)
;;  - (type:name Symbol)  ;; TODO: add srcloc
;;  - (type:name-subtype Symbol Type)
;;  - (type:var Symbol)
;;  - (type:app Type [Listof Type])
;;  - (type:tuple [Listof Type])
;;  - (type:record [Symdictof Type])
;;  - (type:arrow [Listof Type] Type)
(defstruct type:bottom () transparent: #t)
(defstruct type:name (sym) transparent: #t)  ;; TODO: add srcloc
(defstruct type:name-subtype (sym super) transparent: #t)
(defstruct type:var (sym) transparent: #t)
(defstruct type:app (fun args) transparent: #t)
(defstruct type:tuple (args) transparent: #t)
(defstruct type:record (field-args))
(defstruct type:arrow (in-tys out-ty) transparent: #t)
(def (type? v)
  (or (type:bottom? v)
      (type:name? v)
      (type:name-subtype? v)
      (type:var? v)
      (type:app? v)
      (type:tuple? v)
      (type:record? v)
      (type:arrow? v)))

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

(def (ptype? v) (or (type? v) (ptype:union? v)))
(def (ntype? v) (or (type? v) (ntype:intersection? v)))

;; A MonoEnv is a [Symdictof NType]
;; monotype environments ∆ bind λ-bound variables only,
;; no known-type/let-bound variables, don't have ∀-quantifiers

;; A TypingScheme is a (typing-scheme MonoEnv PType)
;; Representing both the type of an expression and the types of its
;; free λ-bound variables. A polar typing scheme [D⁻]t⁺ is a typing
;; scheme where the types D⁻(x) of λ-bound variables are given by
;; negative type terms, and the type of the result t⁺ is given by a
;; positive type term.
(defstruct typing-scheme (menv type) transparent: #t)

;; --------------------------------------------------------

;; An TypeInfoTable is a [Hashof Symbol TypeInfo]
;; A TypeInfo is a (type-info [Listof Variance] TypingScheme)
(defstruct type-info (variances methods) transparent: #t)

;; make-type-info-table : -> TypeInfoTable
(def (make-type-info-table) (make-hash-table-eq))

;; current-type-info-table : [Parameterof TypeInfoTable]
(def current-type-info-table (make-parameter (make-type-info-table)))

;; copy-current-type-info-table : -> TypeInfoTable
(def (copy-current-type-info-table) (hash-copy (current-type-info-table)))

;; type-name-variances : Symbol -> [Listof Variance]
(def (type-name-variances name)
  (type-info-variances (hash-ref (current-type-info-table) name)))

;; type-name-methods : Symbol -> TypingScheme
(def (type-name-methods name)
  (def ms (type-info-methods (hash-ref (current-type-info-table) name)))
  (unless (typing-scheme? ms) (error 'type-name-methods "expected typing-scheme output" ms))
  ms)

;; add-type-info! : Symbol TypeInfo -> Void
;; Adds the type info, checking that it's not overriding existing info
(def (add-type-info! name info)
  (unless (symbol? name) (error 'add-type-info! "expected symbol"))
  (unless (type-info? info) (error 'add-type-info! "expected type-info"))
  (unless (typing-scheme? (type-info-methods info)) (error 'add-type-info "expected typing-scheme within type-info"))
  (def tbl (current-type-info-table))
  (when (hash-key? tbl name) (error 'add-type-info! "entry already exists for" name))
  (hash-put! tbl name info))

;; --------------------------------------------------------

(def type:unit (type:tuple []))
(def type:empty-record (type:record empty-symdict))
(def empty-methods (typing-scheme empty-symdict type:empty-record))
(def empty-type-info (type-info [] empty-methods))
(def (add-empty-type-info! name) (add-type-info! name empty-type-info))

(def type:int (type:name 'int))
(def type:nat (type:name-subtype 'nat type:int))
(def type:bool (type:name 'bool))
(def type:bytes (type:name 'bytes))

(def type:Participant (type:name 'Participant))
(def type:Digest (type:name 'Digest))
(def type:Assets (type:name 'Assets))
(def type:Signature (type:name 'Signature))

(for-each add-empty-type-info! '(int nat bool bytes Participant Digest Assets Signature))

;; lists are covariant, so (listof a) <: (listof b) when a <: b
(def typector:listof (type:name 'listof))
(add-type-info! 'listof (type-info [covariant] empty-methods))

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

;; type-join : PType PType -> PType
;; the type that is a supertype of both types
(def (type-join a b)
  (match* (a b)
    (((ptype:union as) (ptype:union bs))
     (ptype:union
      (for/fold (bs bs) ((a (reverse as)))
        (cond ((member a bs) bs)
              (else (cons a bs))))))
    (((ptype:union as) b)
     (ptype:union (cond ((member b as) as)
                        (else (append1 as b)))))
    ((a (ptype:union bs))
     (ptype:union (cond ((member a bs) bs)
                        (else (cons a bs)))))
    ((a b)
     (cond ((equal? a b) a)
           (else (ptype:union (list a b)))))))

;; types-join : [Listof PType] -> PType
;; the type that is a supertype of all types in the list
(def (types-join ts)
  (match ts
    ([] (type:bottom))
    ([t] t)
    ([a b] (type-join a b))
    (_ (type-join (car ts) (types-join (cdr ts))))))

;; type-meet : NType NType -> NType
;; the type that is a subtype of both types
;; NOTE: the meet of non-overlapping types is still different from bottom
(def (type-meet a b)
  (match* (a b)
    (((ntype:intersection as) (ntype:intersection bs))
     (ntype:intersection
      (for/fold (bs bs) ((a (reverse as)))
        (cond ((member a bs) bs)
              (else (cons a bs))))))
    (((ntype:intersection as) b)
     (ntype:intersection (cond ((member b as) as)
                               (else (append1 as b)))))
    ((a (ntype:intersection bs))
     (ntype:intersection (cond ((member a bs) bs)
                               (else (cons a bs)))))
    ((a b)
     (cond ((equal? a b) a)
           (else (ntype:intersection (list a b)))))))

;; types-meet : [Listof NType] -> NType
;; the type that is a subtype of all types in the list
(def (types-meet ts)
  (match ts
    ([] (ntype:intersection []))
    ([t] t)
    ([a b] (type-meet a b))
    (_ (type-meet (car ts) (types-meet (cdr ts))))))

;; menv-meet : MonoEnv MonoEnv -> MonoEnv
;; union of keys, type-meet of values
(def (menv-meet as bs)
  (for/fold (acc as) ((k (symdict-keys bs)))
    (def b (symdict-ref bs k))
    (cond
      ((symdict-has-key? acc k) (symdict-update acc k (lambda (a) (type-meet a b)) b))
      (else                     (symdict-put acc k b)))))

;; menvs-meet : [Listof MonoEnv] -> MonoEnv
;; union of keys, type-meet of values
(def (menvs-meet menvs)
  (for/fold (acc empty-symdict) ((me menvs)) (menv-meet acc me)))

;; --------------------------------------------------------

;; type-vars : Type -> [Listof Symbol]
(def (type-vars t)
  (match t
    ((type:bottom) [])
    ((type:name _) [])
    ((type:name-subtype _ sup) (type-vars sup))
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
    ((type:name s) #f)
    ((type:name-subtype _ sup) (hv? sup))
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
    ((type:name _) t)
    ((type:name-subtype nm sup)
     (type:name-subtype nm (sub sup)))
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
     (types-join (map sub ts)))
    ((ntype:intersection ts)
     (types-meet (map sub ts)))))

;; --------------------------------------------------------

(def (print-typing-scheme ts)
  (with (((typing-scheme menv t) ts))
    (display-separated
     (symdict-keys menv)
     prefix: "["
     suffix: "]"
     separator: ", "
     display-element: (lambda (k out)
                        (fprintf out "~a: ~s" k (type->sexpr (symdict-ref menv k)))))
    (write (type->sexpr t))))

;; typing-schemes-join : [Listof TypingScheme] -> TypingScheme
;; meet on menvs, join on types
(def (typing-schemes-join ts)
  (typing-scheme (menvs-meet (map typing-scheme-menv ts))
                 (types-join (map typing-scheme-type ts))))

;; typing-scheme/menv : MonoEnv TypingScheme -> TypingScheme
(def (typing-scheme/menv menv1 ts)
  (with (((typing-scheme menv2 t) ts))
    (typing-scheme (menv-meet menv1 menv2) t)))

;; --------------------------------------------------------

;; type->sexpr : Type -> Sexpr
;; A human-readable form for displaying types to the user
(def (type->sexpr t)
  (match t
    ((type:bottom) '⊥)
    ((type:name s) s)
    ((type:name-subtype s _) s)
    ((type:var s) `',s)
    ((type:app f as) (cons (type->sexpr f) (map type->sexpr as)))
    ((type:tuple as) (cons '@tuple (map type->sexpr as)))
    ((type:record flds) (cons '@record
                              (map (lambda (k) [k (type->sexpr (symdict-ref flds k))])
                                   (symdict-keys flds))))
    ((type:arrow as b) (cons '-> (append1 (map type->sexpr as) (type->sexpr b))))
    ((ptype:union ts) (cons '∪ (map type->sexpr ts)))
    ((ntype:intersection ts) (cons '∩ (map type->sexpr ts)))))

;; type->string : Type -> String
(def (type->string t) (format "~y" (type->sexpr t)))

;; symbol->repr-sexpr : Symbol -> Sexpr
(def (symbol->repr-sexpr s) `',s)

;; repr-sexpr->symbol : Sexpr -> Symbol
(def (repr-sexpr->symbol s) (match s (['quote x] x)))

;; list->repr-sexpr : [Listof V] [V -> Sexpr] -> Sexpr
(def (list->repr-sexpr l v->s)
  `(@list ,@(map v->s l)))

;; repr-sexpr->list : Sexpr [Sexpr -> V] -> [Listof V]
(def (repr-sexpr->list s s->v)
  (match s
    ((cons '@list l) (map s->v l))
    (_ (error 'repr-sexpr->list "expected `@list`"))))

;; symdict->repr-sexpr : [Symdictof V] [V -> Sexpr] -> Sexpr
(def (symdict->repr-sexpr d v->s)
  `(symdict
    ,@(for/collect (k (symdict-keys d))
        `(',k ,(v->s (symdict-ref d k))))))

;; repr-sexpr->symdict : Sexpr [Sexpr -> V] -> [Symdictof V]
(def (repr-sexpr->symdict s s->v)
  (match s
    ((cons 'symdict l)
     (list->symdict
      (for/collect (e l)
        (match e
          ([['quote k] sv] (cons k (s->v sv)))
          (_ (error 'repr-sexpr->symdict "bad entry shape"))))))
    (_ (error 'repr-sexpr->symdict "expected `symdict`"))))

;; type->repr-sexpr : Type -> Sexpr
;; A lossless s-expression representation in "repr" style
(def (type->repr-sexpr t)
  (match t
    ((type:bottom) '(type:bottom))
    ((type:name s) `(type:name ',s))
    ((type:name-subtype s t) `(type:name-subtype ',s ,(type->repr-sexpr t)))
    ((type:var s) `(type:var ',s))
    ((type:app f as) `(type:app ,(type->repr-sexpr f) ,(list->repr-sexpr as type->repr-sexpr)))
    ((type:tuple as) `(type:tuple ,(list->repr-sexpr as type->repr-sexpr)))
    ((type:record flds) `(type:record ,(symdict->repr-sexpr flds type->repr-sexpr)))
    ((type:arrow as b) `(type:arrow ,(list->repr-sexpr as type->repr-sexpr) ,(type->repr-sexpr b)))
    ((ptype:union ts) `(ptype:union ,(list->repr-sexpr ts type->repr-sexpr)))
    ((ntype:intersection ts) `(ntype:intersection ,(list->repr-sexpr ts type->repr-sexpr)))))

;; repr-sexpr->type : Sexpr -> Type
;; The left-inverse for type->repr-sexpr
(def (repr-sexpr->type s)
  (match s
    ('(type:bottom) (type:bottom))
    (['type:name ['quote s]] (type:name s))
    (['type:name-subtype ['quote s] t] (type:name-subtype s (repr-sexpr->type t)))
    (['type:var ['quote s]] (type:var s))
    (['type:app f as] (type:app (repr-sexpr->type f) (repr-sexpr->list as repr-sexpr->type)))
    (['type:tuple as] (type:tuple (repr-sexpr->list as repr-sexpr->type)))
    (['type:record flds] (type:record (repr-sexpr->symdict flds repr-sexpr->type)))
    (['type:arrow as b] (type:arrow (repr-sexpr->list as repr-sexpr->type) (repr-sexpr->type b)))
    (['ptype:union ts] (ptype:union (repr-sexpr->list ts repr-sexpr->type)))
    (['ntype:intersection ts] (ntype:intersection (repr-sexpr->list ts repr-sexpr->type)))))

;; typing-scheme->repr-sexpr : TypingScheme -> Sexpr
(def (typing-scheme->repr-sexpr ts)
  (match ts
    ((typing-scheme menv t)
     `(typing-scheme ,(symdict->repr-sexpr menv type->repr-sexpr)
                     ,(type->repr-sexpr t)))))

;; repr-sexpr->typing-scheme : Sexpr -> TypingScheme
;; The left-inverse for typing-scheme->repr-sexpr
(def (repr-sexpr->typing-scheme s)
  (match s
    (['typing-scheme menv t]
     (typing-scheme (repr-sexpr->symdict menv repr-sexpr->type)
                    (repr-sexpr->type t)))))

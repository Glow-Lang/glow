(export #t)

(import :std/iter
        :std/misc/list
        :gerbil/gambit/exact
        :gerbil/gambit/bytes
        <expander-runtime>
        (for-template :gerbil/core)
        :clan/pure/dict/assq
        :clan/pure/dict/symdict)

;; Typechecking glow-sexpr

;; Type system and scoping rules are closest
;; to ReasonML, but Record types are structural.

;; A Variance is a (variance Bool Bool)
(defstruct variance (covariant? contravariant?) transparent: #t)
(def irrelevant    (variance #t #t)) ; bottom, ignored
(def covariant     (variance #t #f))
(def contravariant (variance #f #t))
(def invariant     (variance #f #f)) ; top, constrained
;; variance-irrelevant? : Variance -> Bool
(def (variance-irrelevant? v)
  (and (variance-covariant? v) (variance-contravariant? v)))
;; variance-invariant? : Variance -> Bool
(def (variance-invariant? v)
  (and (not (variance-covariant? v)) (not (variance-contravariant? v))))
;; variance-join : Variance Variance -> Variance
;; irrelevant ⊔ a = a
;; invariant ⊔ a = invariant
(def (variance-join a b)
  (with (((variance ap an) a) ((variance bp bn) b))
    (variance (and ap bp) (and an bn))))
;; variance-compose : Variance Variance -> Variance
;; irrelevant ∘ a = irrelevant
;; covariant ∘ a = a
;; contravariant ∘ (variance p n) = (variance n p)
;; invariant ∘ (variance p n) = (variance (and p n) (and p n))
(def (variance-compose a b)
  (with (((variance ap an) a) ((variance bp bn) b))
    (variance (and (or ap bn) (or an bp))
              (and (or ap bp) (or an bn)))))

;; A Type is one of:
;;  - (type:bottom)
;;  - (type:name Symbol [Listof Variance])  ;; TODO: add srcloc
;;  - (type:var Symbol)
;;  - (type:app Type [Listof Type])
;;  - (type:tuple [Listof Type])
;;  - (type:record [Symdictof Type])
(defstruct type:bottom () transparent: #t)
(defstruct type:name (sym vances) transparent: #t)  ;; TODO: add srcloc
(defstruct type:var (sym) transparent: #t)
(defstruct type:app (fun args) transparent: #t)
(defstruct type:tuple (args) transparent: #t)
(defstruct type:record (field-args))
(def (type? v) (or (type:bottom? v) (type:name? v) (type:var? v) (type:app? v) (type:tuple? v) (type:record? v)))

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

;; A Pattys is an [Assqof Symbol Type]
;; for the types of the pattern variables within a pattern

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

;; type=? : Type Type -> Bool
(def (type=? a b)
  (and (subtype? a b) (subtype? b a)))

;; variance-type~? : Variance Type Type -> Bool
;; (~? irrelavent a b) = #t
;; (~? covariant a b) = (subtype? a b)
;; (~? contravariant a b) = (subtype? b a)
;; (~? invariant a b) = (type=? a b)
(def (variance-type~? v a b)
  (and (or (variance-covariant? v) (subtype? b a))
       (or (variance-contravariant? v) (subtype? a b))))

;; subtype? : PType NType -> Bool
(def (subtype? a b)
  (match* (a b)
    (((type:bottom) _) #t)
    (((type:name x vxs) (type:name y vys))
     (and (eq? x y) (equal? vxs vys)))
    (((type:var x) (type:var y)) (eq? x y))
    (((type:tuple as) (type:tuple bs))
     (and (= (length as) (length bs))
          (andmap subtype? as bs)))
    (((type:record as) (type:record bs))
     (symdict=? as bs subtype?))
    (((type:app (type:name f1 v1s) a1s) (type:app (type:name f2 v2s) a2s))
     (and (eq? f1 f2)
          (equal? v1s v2s)
          (= (length v1s) (length a1s) (length a2s))
          (andmap variance-type~? v1s a1s a2s)))
    (((ptype:union as) b)
     (andmap (lambda (a) (subtype? a b)) as))
    ((a (ntype:intersection bs))
     (andmap (lambda (b) (subtype? a b)) bs))
    ((_ _) #f)))

;; type-join : PType PType -> PType
;; finds the type that is a supertype of both types, otherwise error
(def (type-join a b)
  (match* (a b)
    (((ptype:union as) (ptype:union bs)) (ptype:union (append as bs)))
    (((ptype:union as) b) (ptype:union (append1 as b)))
    ((a (ptype:union bs)) (ptype:union (cons a bs)))
    ((a b)
     (cond ((subtype? a b) b)
           ((subtype? b a) a)
           (else (ptype:union (list a b)))))))

;; types-join : [Listof PType] -> PType
;; finds the type that is a supertype of all types in the list
(def (types-join ts)
  (match ts
    ([] (type:bottom))
    ([t] t)
    ([a b] (type-join a b))
    (_ (type-join (car ts) (types-join (cdr ts))))))

;; type-meet : NType NType -> NType
;; finds the type that is a subtype of both types, otherwise error
;; NOTE: the meet of non-overlapping types is still different from bottom
(def (type-meet a b)
  (match* (a b)
    (((ntype:intersection as) (ntype:intersection bs)) (ntype:intersection (append as bs)))
    (((ntype:intersection as) b) (ntype:intersection (append1 as b)))
    ((a (ntype:intersection bs)) (ntype:intersection (cons a bs)))
    ((a b)
     (cond ((subtype? a b) a)
           ((subtype? b a) b)
           (else (ntype:intersection (list a b)))))))

;; pattys-join : [Listof Pattys] -> Pattys
;; intersection of keys, join of types for same key
;; used for or-patterns
(def (pattys-join ps)
  (cond ((null? ps) [])
        ((null? (cdr ps)) (car ps))
        (else
         (for/collect ((k (assq-keys (car ps)))
                       when
                       (andmap (lambda (o) (assq-has-key? o k)) (cdr ps)))
           (cons k (types-join (map (lambda (p) (assq-ref p k)) ps)))))))

;; pattys-append : [Listof Pattys] -> Pattys
;; append, but check no duplicate keys
;; used for combinations of patterns in the same scope other than "or"
(def (pattys-append ps)
  (def p (apply append ps))
  (check-duplicate-symbols (map car p))
 p)

;; check-duplicate-symbols : [Listof Symbol] -> TrueOrError
;; true if there are no duplicates, error otherwise
(def (check-duplicate-symbols syms)
  (match syms
    ([] #t)
    ([a . bs]
     (if (memq a bs)
         (error a "duplicate identifier")
         (check-duplicate-symbols bs)))))

;; An EnvEntry is one of:
;;  - (entry:val Type)
;;  - (entry:fun [Listof Symbol] [Listof Type] Type)
;;  - (entry:ctor [Listof Symbol] [Listof Type] Type)
;;  - (entry:type [Listof Symbol] Type)
(defstruct entry:val (type) transparent: #t)
(defstruct entry:fun (typarams input-types output-type) transparent: #t)
(defstruct entry:ctor (typarams input-types output-type) transparent: #t)
(defstruct entry:type (params type) transparent: #t)
(def (env-entry? v) (or (entry:val? v) (entry:fun? v) (entry:ctor? v) (entry:type? v)))

;; An Env is a [Symdictof EnvEntry]
;; A TyvarEnv is a [Symdictof Type]

;; A TyvarBisubst is a [Symdictof TypeInterval]
;; A TypeInterval is a (type-interval NType PType)
;; maps negative occurrences of type variables to the NType,
;; and positive occurrences of type variables to the PType
;; should be idempotent: applying a bisubst a 2nd time shouldn't matter
;; should be stable: the NType should be a subtype of the PType
(defstruct type-interval (ntype ptype) transparent: #t)
;; for example applying the bisubst
;; (symdict ('a (type-interval B C)))
;; to the identity function with type ('a -> 'a)
;; results in (B -> C), where a value of type B flows into C

;; not-bound-as-ctor? : Env Symbol -> Bool
(def (not-bound-as-ctor? env s)
  (or (not (symdict-has-key? env s))
      (not (entry:ctor? (symdict-ref env s)))))

;; bound-as-ctor? : Env Symbol -> Bool
(def (bound-as-ctor? env s)
  (and (symdict-has-key? env s)
       (entry:ctor? (symdict-ref env s))))

;; type-subst : TyvarEnv Type -> Type
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
    ((ptype:union ts)
     (ptype:union (map sub ts)))
    ((ntype:intersection ts)
     (ntype:intersection (map sub ts)))))

;; tyvar-bisubst : TypeInterval Variance Symbol -> PNType
(def (tyvar-bisubst tvl v s)
  (cond ((variance-covariant? v) (type-interval-ptype tvl))
        ((variance-contravariant? v) (type-interval-ntype tvl))
        ((eq? (type-interval-ptype tvl) (type-interval-ntype tvl))
         (type-interval-ptype tvl))
        (else (error s "could not infer type"))))

;; ptype-bisubst : TyvarBisubst Variance PNType -> PNType
;; P vs N depends on the variance
(def (type-bisubst tybi v t)
  ;; sub : PNType -> PNType
  (def (sub pt) (type-bisubst tybi v pt))
  ;; vsub : Variance PNType -> PNType
  ;; variance is within v, so must compose with v
  (def (vsub v2 t)
    (type-bisubst tybi (variance-compose v v2) t))
  (match t
    ((type:bottom) t)
    ((type:name _ _) t)
    ((type:var s)
     (cond ((symdict-has-key? tybi s)
            (tyvar-bisubst (symdict-ref tybi s) v s))
           (else t)))
    ((type:app (type:name f vs) as)
     (unless (= (length vs) (length as))
       (error f "wrong number of arguments to type constructor"))
     (type:app (type:name f vs) (map vsub vs as)))
    ((type:tuple as)
     (type:tuple (map sub as)))
    ((type:record fldtys)
     (type:record
      (list->symdict
       (map (lambda (p) (cons (car p) (sub (cdr p))))
            fldtys))))
    ((ptype:union ts)
     (ptype:union (map sub ts)))
    ((ntype:intersection ts)
     (ntype:intersection (map sub ts)))))

;; A Constraints is a [Listof Constraint]
;; A Constraint is one of:
;;  - (constraint:subtype PType NType)
;;  - (constraint:type-equal Type Type)
(defstruct constraint:subtype (a b) transparent: #t)
(defstruct constraint:type-equal (a b) transparent: #t)

;; constraints-from-variance : Variance PNType PNType -> Constraints
(def (constraints-from-variance v a b)
  (cond ((variance-irrelevant? v) [])
        ((variance-covariant? v) [(constraint:subtype a b)])
        ((variance-contravariant? v) [(constraint:subtype b a)])
        (else [(constraint:type-equal a b)])))

;; constraints-bisubst : TyvarBisubst Constraints -> Constraints
(def (constraints-bisubst tybi cs)
  (map (lambda (c) (constraint-bisubst tybi c)) cs))

;; constraint-bisubst : TyvarBisubst Constraint -> Constraint
(def (constraint-bisubst tybi c)
  (match c
    ((constraint:subtype a b)
     (constraint:subtype (type-bisubst tybi contravariant a)
                         (type-bisubst tybi covariant b)))
    ((constraint:type-equal a b)
     (constraint:type-equal (type-bisubst tybi invariant a)
                            (type-bisubst tybi invariant b)))))

;; symdict-key-set=? : [Symdict Any] [Symdict Any] -> Bool
(def (symdict-key-set=? a b) (symdict=? a b true))

;; biunify : Constraints TyvarBisubst -> TyvarBisubst
(def (biunify cs tybi)
  (match cs
    ([] tybi)
    ([fst . rst]
     (match fst
       ((constraint:type-equal a b)
        (biunify (cons* (constraint:subtype a b) (constraint:subtype b a) rst) tybi))
       ((constraint:subtype (type:bottom) _)
        (biunify rst tybi))
       ((constraint:subtype (type:name x vxs) (type:name y vys))
        (unless (eq? x y) (error 'subtype "type mismatch"))
        (unless (equal? vxs vys) (error 'subtype "inconsistant variances"))
        (biunify rst tybi))
       ((constraint:subtype (type:var a) (type:var b))
        (cond
          ((eq? a b) (biunify rst tybi))
          ; a does not occur in b
          ((symdict-has-key? tybi a)
           (with (((type-interval tn tp) (symdict-ref tybi a)))
             (error "TODO")))
          (else
           (let ((tybi2 (symdict-put tybi a (type-interval (type:var a) (type:var b)))))
             (biunify (constraints-bisubst tybi2 rst) tybi2)))))
       ((constraint:subtype (type:tuple as) (type:tuple bs))
        (unless (length=? as bs)
          (error 'subtype "tuples lengths don't match"))
        (biunify (append (map make-constraint:subtype as bs) rst) tybi))
       ((constraint:subtype (type:record as) (type:record bs))
        (unless (symdict-key-set=? as bs)
          (error 'subtype "record fields don't match"))
        (biunify (append (map (lambda (k)
                                (constraint:subtype (symdict-ref as k) (symdict-ref bs k)))
                              (symdict-keys as))
                         rst)
                 tybi))
       ((constraint:subtype (type:app (type:name f1 v1s) a1s) (type:app (type:name f2 v2s) a2s))
        (unless (eq? f1 f2) (error 'subtype "type mismatch"))
        (unless (equal? v1s v2s) (error 'subtype "inconsistant variances"))
        (unless (= (length v1s) (length a1s) (length a2s))
          (error 'subtype "wrong number of arguments to type constructor"))
        (biunify (foldr (lambda (v a1 a2 rst)
                          (append (constraints-from-variance v a1 a2) rst))
                        rst
                        v1s
                        a1s
                        a2s)
                 tybi))
       ((constraint:subtype (ptype:union as) b)
        (biunify (append (map (lambda (a) (constraint:subtype a b)) as) rst) tybi))
       ((constraint:subtype a (ntype:intersection bs))
        (biunify (append (map (lambda (b) (constraint:subtype a b)) bs) rst) tybi))
       (_ (error 'biunify "TODO"))))))

; literals:
;   common:
;     \@
;   stmt:
;     : quote def λ deftype defdata publish! verify!
;   expr/pat:
;     : ann \@tuple \@record \@list \@or-pat if block switch _ require! assert! deposit! withdraw!
;   type:
;     quote \@tuple \@record

(def (stx-atomic-literal? v)
  (def e (stx-e v))
  (or (integer? e) (string? e) (bytes? e) (boolean? e)))

;; parse-type : Env TyvarEnv TypeStx -> Type
(def (parse-type env tyvars stx)
  ;; party : TypeStx -> Type
  (def (party s) (parse-type env tyvars s))
  (syntax-case stx (@ quote @tuple @record)
    ((@ _ _) (error 'parse-type "TODO: deal with @"))
    ((@tuple t ...) (type:tuple (stx-map party #'(t ...))))
    ((@record (fld t) ...)
     (type:record
      (list->symdict
       (stx-map (lambda (fld t) (cons (syntax-e fld) (party t)))
                #'(fld ...)
                #'(t ...)))))
    ('x (identifier? #'x)
     (let ((s (syntax-e #'x)))
       (cond ((symdict-has-key? tyvars s) (symdict-ref tyvars s))
             (else (type:var s)))))
    (x (identifier? #'x)
     (let ((s (syntax-e #'x)))
       (unless (symdict-has-key? env s)
         (error s "unknown type"))
       (match (symdict-ref env s)
         ((entry:type [] t) t))))
    ((f a ...) (identifier? #'f)
     (let ((s (syntax-e #'f)))
       (match (symdict-ref env s)
         ((entry:type xs b)
          (def as (stx-map party #'(a ...)))
          (unless (= (length xs) (length as))
            (error 'parse-type "wrong number of type arguments"))
          (def tyvars2 (symdict-put/list tyvars (map cons xs as)))
          (type-subst tyvars2 b)))))))

;; parse-param-name : ParamStx -> Symbol
(def (parse-param-name p)
  (syntax-e (if (identifier? p) p (stx-car p))))

;; parse-param-type : Env ParamStx -> (U #f Type)
(def (parse-param-type env p)
  (syntax-case p (:)
    (x (identifier? #'x) #f)
    ((x : type) (parse-type env empty-symdict #'type))))

;; tc-body : Env BodyStx -> Type
(def (tc-body env stx)
  (cond ((stx-null? stx) (type:tuple []))
        ((stx-null? (stx-cdr stx)) (tc-expr env (stx-car stx)))
        (else
         (tc-body (tc-stmt env (stx-car stx)) (stx-cdr stx)))))

;; tc-stmt : Env StmtStx -> Env
;; TODO: reorganize into one case for each keyword, possibly delegating to a function with
;; the multiple cases within a single keyword
(def (tc-stmt env stx)
  (syntax-case stx (@ : quote def λ deftype defdata publish! verify!)
    ((@ _ _) (error 'tc-stmt "TODO: deal with @"))
    ((deftype . _) (tc-stmt-deftype env stx))
    ((defdata . _) (tc-stmt-defdata env stx))
    ((def . _) (tc-stmt-def env stx))
    ((publish! x ...) (stx-andmap identifier? #'(x ...))
     (error 'tc-stmt "TODO: deal with publish!"))
    ((verify! x ...) (stx-andmap identifier? #'(x ...))
     (error 'tc-stmt "TODO: deal with verify!"))
    (expr
     (let ()
       (tc-expr env #'expr)
       env))))

;; tc-stmt-deftype : Env StmtStx -> Env
(def (tc-stmt-deftype env stx)
  (syntax-case stx (@ : quote deftype defdata)
    ((deftype x t) (identifier? #'x)
     (symdict-put env
                  (syntax-e #'x)
                  (entry:type [] (parse-type env empty-symdict #'t))))
    ((deftype (f 'x ...) b) (identifier? #'f)
     (let ((s (syntax-e #'f))
           (xs (stx-map syntax-e #'(x ...))))
       (def tyvars (list->symdict (map cons xs (map make-type:var xs))))
       (symdict-put env
                    s
                    (entry:type xs (parse-type env tyvars #'b)))))))

;; tc-stmt-defdata : Env StmtStx -> Env
(def (tc-stmt-defdata env stx)
  (syntax-case stx (@ : quote deftype defdata)
    ((defdata x variant ...) (identifier? #'x)
     (let ((s (syntax-e #'x)))
       (def b (type:name (gensym s) []))
       (tc-defdata-variants (symdict-put env s (entry:type [] b))
                            []
                            b
                            #'(variant ...))))
    ((defdata (f 'x ...) variant ...) (identifier? #'f)
     (let ((s (syntax-e #'f))
           (xs (stx-map syntax-e #'(x ...))))
       ;; TODO: allow variances to be either annotated or inferred
       (def vances (map (lambda (x) invariant) xs))
       (def b (type:app (type:name (gensym s) vances) (map make-type:var xs)))
       (tc-defdata-variants (symdict-put env s (entry:type xs b))
                            xs
                            b
                            #'(variant ...))))))


;; tc-defdata-variant : Env [Listof Symbol] Type VariantStx -> [Cons Symbol EnvEntry]
(def (tc-defdata-variant env xs b stx)
  ;; tyvars : TyvarEnv
  (def tyvars (list->symdict (map cons xs (map make-type:var xs))))
  ;; party : TypeStx -> Type
  (def (party s) (parse-type env tyvars s))
  (syntax-case stx ()
    (x (identifier? #'x)
     (cons (syntax-e #'x) (entry:ctor xs [] b)))
    ((f a ...) (identifier? #'f)
     (cons (syntax-e #'f) (entry:ctor xs (stx-map party #'(a ...)) b)))))

;; tc-defdata-variants : Env [Listof Symbol] Type [StxListof VariantStx] -> Env
(def (tc-defdata-variants env xs b stx)
  ;; tcvariant : VariantStx -> [Cons Symbol EnvEntry]
  (def (tcvariant v) (tc-defdata-variant env xs b v))
  (symdict-put/list env (stx-map tcvariant stx)))

;; tc-stmt-def : Env StmtStx -> Env
(def (tc-stmt-def env stx)
  (syntax-case stx (@ : quote def λ)
    ((def f (λ params : out-type body ...)) (identifier? #'f)
     (let ((s (syntax-e #'f))
           (xs (stx-map parse-param-name #'params))
           (in-ts (stx-map (lambda (p) (parse-param-type env p)) #'params))
           (out-t (parse-type env empty-symdict #'out-type)))
       (symdict-put env s (tc-function env xs in-ts out-t #'(body ...)))))
    ((def f (λ params body ...)) (identifier? #'f)
     (let ((s (syntax-e #'f))
           (xs (stx-map parse-param-name #'params))
           (in-ts (stx-map (lambda (p) (parse-param-type env p)) #'params)))
       (symdict-put env s (tc-function env xs in-ts #f #'(body ...)))))
    ((def x : type expr) (identifier? #'x)
     (let ((s (syntax-e #'x))
           (t (parse-type env empty-symdict #'type)))
       (tc-expr/check env #'expr t)
       (symdict-put env s (entry:val t))))
    ((def x expr) (identifier? #'x)
     (let ((s (syntax-e #'x)))
       (symdict-put env s (entry:val (tc-expr env #'expr)))))))

;; tc-function : Env [Listof Symbol] [Listof (U #f Type)] (U #f Type) BodyStx -> EnvEntry
(def (tc-function env xs in-tys exp-out-ty body)
  ;; TODO: type inference on missing in-tys
  ;; until then, error when any are missing
  (for ((x xs) (in-ty in-tys))
    (unless in-ty (error x "missing type annotation on function parameter")))
  ;; in-tys : [Listof Type]
  ;; body-env : Env
  (def body-env (symdict-put/list env (map cons xs (map make-entry:val in-tys))))
  (def actual-ty (tc-body body-env body))
  (unless (or (not exp-out-ty) (subtype? actual-ty exp-out-ty))
    (error 'tc-function "function output type mismatch"))
  (def out-ty (or exp-out-ty actual-ty))
  (entry:fun [] in-tys out-ty))

;; tc-expr : Env ExprStx -> Type
(def (tc-expr env stx)
  ;; tce : ExprStx -> Type
  (def (tce e) (tc-expr env e))
  (syntax-case stx (@ : ann @tuple @record @list if block switch require! assert! deposit! withdraw!)
    ((@ _ _) (error 'tc-expr "TODO: deal with @"))
    ((ann expr type)
     (tc-expr/check env #'expr (parse-type env empty-symdict #'type)))
    (x (identifier? #'x) (tc-expr-val-id env stx))
    (lit (stx-atomic-literal? #'lit) (tc-literal #'lit))
    ((@tuple e ...)
     (type:tuple (stx-map tce #'(e ...))))
    ((@record (x e) ...)
     (and (stx-andmap identifier? #'(x ...))
          (check-duplicate-identifiers #'(x ...)))
     (let ((s (stx-map syntax-e #'(x ...))))
       (type:record
        (list->symdict (map cons s (stx-map tce #'(e ...)))))))
    ((@list e ...)
     (let ((ts (stx-map tce #'(e ...))))
       (type:listof (types-join ts))))
    ((block b ...)
     (tc-body #'(b ...)))
    ((if c t e)
     (let ()
       (tc-expr/check env #'c type:bool)
       (type-join (tc-expr env #'t) (tc-expr env #'e))))
    ((switch e swcase ...)
     (let ((t (tc-expr env #'e)))
       ;; TODO: implement exhaustiveness checking
       (types-join
        (stx-map (lambda (c) (tc-switch-case env t c)) #'(swcase ...)))))
    ((require! e)
     (error 'tc-expr "TODO: deal with require!"))
    ((assert! e)
     (error 'tc-expr "TODO: deal with assert!"))
    ((deposit! e)
     (error 'tc-expr "TODO: deal with deposit!"))
    ((withdraw! x e) (identifier? #'x)
     (error 'tc-expr "TODO: deal with withdraw!"))
    ((f a ...) (identifier? #'f)
     (let ((s (syntax-e #'f)))
       (match (tc-expr-fun-id env #'f)
         ;; monomorphic case
         ;; TODO: handle polymorphic case with unification
         ((entry:fun [] in-tys out-ty)
          (unless (= (stx-length #'(a ...)) (length in-tys))
            (error s "wrong number of arguments"))
          (stx-for-each (lambda (a t) (tc-expr/check env a t))
                        #'(a ...)
                        in-tys)
          out-ty))))))

;; tc-expr-val-id : Env Identifier -> Type
(def (tc-expr-val-id env x)
  (def s (syntax-e x))
  (unless (symdict-has-key? env s)
    (error s "unbound identifier"))
  (match (symdict-ref env s)
    ((entry:val t) t)
    ((entry:ctor [] [] t) t)))

;; tc-expr-fun-id : Env Identifier -> EnvEntry
;; Treat entry:fun and entry:ctor the same as functions
(def (tc-expr-fun-id env f)
  (def s (syntax-e f))
  (unless (symdict-has-key? env s)
    (error s "unbound identifier"))
  (match (symdict-ref env s)
    ((or (entry:fun vs in-tys out-ty)
         (entry:ctor vs in-tys out-ty))
     (entry:fun vs in-tys out-ty))))

;; tc-expr/check : Env ExprStx (U #f Type) -> TypeOrError
;; returns expected-ty on success, actual-ty if no expected, otherwise error
(def (tc-expr/check env stx expected-ty)
  (unless (or (not expected-ty) (type? expected-ty))
    (error 'tc-expr/check "expected (U #f Type) for 3rd argument"))
  (def actual-ty (tc-expr env stx))
  (unless (or (not expected-ty) (subtype? actual-ty expected-ty))
    (error 'tc-expr/check "type mismatch"))
  (or expected-ty actual-ty))

;; tc-literal : LiteralStx -> Type
(def (tc-literal stx)
  (def e (stx-e stx))
  (cond ((exact-integer? e) type:int)
        ((boolean? e) type:bool)
        ((string? e) type:bytes) ; represent as bytess using UTF-8
        ((bytes? e) type:bytes)
        (else (error 'tc-literal "unrecognized literal"))))

;; tc-switch-case : Env Type SwitchCaseStx -> Type
(def (tc-switch-case env valty stx)
  (syntax-case stx ()
    ((pat body ...)
     (let ((pattys (tc-pat env valty #'pat)))
       (def env/pattys
         (symdict-put/list
          env
          (map (lambda (p) (cons (car p) (entry:val (cdr p))))
               pattys)))
       (tc-body env/pattys #'(body ...))))))


;; tc-pat : Env Type PatStx -> Pattys
(def (tc-pat env valty stx)
  (syntax-case stx (@ : ann @tuple @record @list @or-pat)
    ((@ _ _) (error 'tc-pat "TODO: deal with @"))
    ((ann pat type)
     (let ((t (parse-type env empty-symdict #'type)))
       ;; valty <: t
       ;; because passing valty into a context expecting t
       ;; (switch (ann 5 nat) ((ann x int) x))
       (unless (subtype? valty t)
         (error 'tc-pat "type mismatch"))
       (tc-pat env t #'pat)))
    (blank (and (identifier? #'blank) (free-identifier=? #'blank #'_))
     [])
    (x (and (identifier? #'x) (not-bound-as-ctor? env (syntax-e #'x)))
     (let ((s (syntax-e #'x)))
       [(cons s valty)]))
    (x (and (identifier? #'x) (bound-as-ctor? env (syntax-e #'x)))
     (let ((s (syntax-e #'x)))
       (match (symdict-ref env s)
         ((entry:ctor [] [] t)
          ;; t doesn't necessarily have to be a supertype,
          ;; but does need to be join-compatible so that
          ;; valty <: (type-join valty t)
          (type-join valty t)
          []))))
    (lit (stx-atomic-literal? #'lit)
     (let ((t (tc-literal #'lit)))
       ;; t doesn't necessarily have to be a supertype,
       ;; but does need to be join-compatible so that
       ;; valty <: (type-join valty t)
       (type-join valty t)
       []))
    ((@or-pat p ...)
     (pattys-join (stx-map (lambda (p) (tc-pat env valty p)) #'(p ...))))
    ((@list p ...)
     (cond
       ((type:listof? valty)
        (let ((elem-ty (type:listof-elem valty)))
          (def ptys (stx-map (lambda (p) (tc-pat env elem-ty p)) #'(p ...)))
          (pattys-append ptys)))
       (else
        (error 'tc-pat "TODO: handle list patterns better, unification?"))))
    ((@tuple p ...)
     (match valty
       ((type:tuple ts)
        (unless (= (stx-length #'(p ...)) (length ts))
          (error 'tuple "wrong number of elements in tuple pattern"))
        (def ptys (stx-map (lambda (t p) (tc-pat env t p)) ts #'(p ...)))
        (pattys-append ptys))
       (_ (error 'tc-pat "TODO: handle tuple patterns better, unification?"))))
    ((@record (x p) ...) (stx-andmap identifier? #'(x ...))
     (let ((s (stx-map syntax-e #'(x ...))))
       (match valty
         ((type:record fldtys)
          (def flds (symdict-keys fldtys))
          ;; s ⊆ flds
          (for (x s)
            (unless (symdict-has-key? fldtys x)
              (error x "unexpected key in record pattern")))
          ;; flds ⊆ s
          (for (f flds)
            (unless (member f s)
              (error f "missing key in record pattern")))
          (def ptys
            (stx-map (lambda (x p)
                       (tc-pat env (symdict-ref fldtys x) p))
                     s
                     #'(p ...)))
          (pattys-append ptys))
         (_ (error 'tc-pat "TODO: handle tuple patterns better, unification?")))))
    ((f a ...) (and (identifier? #'f) (bound-as-ctor? env (syntax-e #'f)))
     (let ((s (syntax-e #'f)))
       (unless (symdict-has-key? env s)
         (error s "unbound pattern constructor"))
       (match (symdict-ref env s)
         ((entry:ctor [] in-tys out-ty)
          ;; valty <: out-ty
          ;; because passing valty into a context expecting out-ty
          (unless (= (stx-length #'(a ...)) (length in-tys))
            (error s "wrong number of arguments to data constructor"))
          (def ptys
            (stx-map (lambda (t p) (tc-pat env t p))
                     in-tys
                     #'(a ...)))
          (pattys-append ptys))
         (_ (error 'tc-pat "TODO: handle data constructor patterns better")))))))

#|

(def (on-body stx)
  (syntax-case stx ()
    [() (on-expr #'(@tuple))]
    [(expr) (on-expr #'expr)]
    [(stmt body ...) (??? (on-stmt #'stmt) (on-body #'(body ...)))]))

(def (on-stmt stx)
  (syntax-case stx (@ : quote def λ deftype defdata publish! assert! deposit! withdraw!)
    [(@ attr stmt) (??? (on-stmt #'stmt))]
    [(deftype head type) (??? (on-type #'type))]
    [(defdata head variant ...) (??? (stx-map on-variant #'(variant ...)))]
    [(def id (λ params : out-type body ...)) (identifier? #'id) (??? (on-type #'out-type) (on-body #'(body ...)))]
    [(def id : type expr) (identifier? #'id) (??? (on-type #'type) (on-expr #'expr))]
    [(publish! id ...) (stx-andmap identifier? #'(id ...)) ???]
    [(verify! id ...) (stx-andmap identifier? #'(id ...)) ???]
    [_ (on-expr stx)]))

(def (on-expr stx)
  (syntax-case stx (@ : ann \@tuple \@record \@list \@or-pat if block switch _)
    [(@ attr expr) (??? (on-expr #'expr))]
    [(ann expr type) (??? (on-expr #'expr) (on-type #'type))]
    [id (identifier? #'id) ???]
    [lit (stx-atomic-literal? #'lit) ???]
    [(@list expr ...) (??? (stx-map on-expr #'(expr ...)))]
    [(@tuple expr ...) (??? (stx-map on-expr #'(expr ...)))]
    [(@record (id expr) ...) (??? (stx-map on-expr #'(expr ...)))]
    [(block body ...) (on-body #'(body ...))]
    [(if c-expr t-expr e-expr) (??? (on-expr #'c-expr) (on-expr #'t-expr) (on-expr #'e-expr))]
    [(switch expr case ...) (??? (on-expr #'expr) (on-cases #'(case ...)))]
    [(require! expr) (??? (on-expr #'expr))]
    [(assert! expr) (??? (on-expr #'expr))]
    [(deposit! expr) (??? (on-expr #'expr))]
    [(withdraw! id expr) (identifier? #'id) (??? (on-expr #'expr))]
    [(id expr ...) (identifier? #'id) (??? (stx-map on-expr #'(expr ...)))]))

(def (on-cases stx)
  (??? (stx-map on-case stx)))

(def (on-case stx)
  (syntax-case stx ()
    [(pat body ...) (??? (on-pat #'pat) (on-body #'(body ...)))]))

(def (on-pat stx)
  (syntax-case stx (@ : ann \@tuple \@record \@list \@or-pat if block switch _)
    [(@ attr pat) (??? (on-pat #'pat))]
    [(ann pat type) (??? (on-pat #'pat) (on-type #'type))]
    [id (identifier? #'id) ???]
    [_ ???]
    [lit (stx-atomic-literal? #'lit) ???]
    [(@list pat ...) (??? (stx-map on-pat #'(pat ...)))]
    [(@tuple pat ...) (??? (stx-map on-pat #'(pat ...)))]
    [(@record (id pat) ...) (??? (stx-map on-pat #'(pat ...)))]
    [(@or-pat pat ...) (??? (stx-map on-pat #'(pat ...)))]
    [(id pat ...) (identifier? #'id) (??? (stx-map on-pat #'(pat ...)))]))


(def (on-type stx)
  (syntax-case stx (@ quote \@tuple \@record)
    [(@ attr type) (??? (on-type #'type))]
    [id (identifier? #'id) ???]
    [(quote tyvar) (identifier? #'tyvar) ???]
    [(@tuple type ...) (??? (stx-map on-type #'(type ...)))]
    [(@record (id type) ...) (??? (stx-map on-type #'(type ...)))]
    [(id type ...) (identifier? #'id) (??? (stx-map on-type #'(type ...)))]))
|#

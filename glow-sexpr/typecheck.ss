(export #t)

(import :std/iter
        :gerbil/gambit/exact
        :gerbil/gambit/bytes
        <expander-runtime>
        (for-template :gerbil/core)
        :clan/pure/dict/assq
        :clan/pure/dict/symdict)

;; Typechecking glow-sexpr

;; Type system and scoping rules are closest
;; to ReasonML, but Record types are structural.

;; A Type is one of:
;;  - (type:bottom)
;;  - (type:name Symbol [Boxof (U Bool Type)])
;;  - (type:var Symbol)
;;  - (type:app Type [Listof Type])
;;  - (type:tuple [Listof Type])
;;  - (type:record [Symdictof Type])
(defstruct type:bottom () transparent: #t)
(defstruct type:name (sym box) transparent: #t)
(defstruct type:var (sym) transparent: #t)
(defstruct type:app (fun args) transparent: #t)
(defstruct type:tuple (args) transparent: #t)
(defstruct type:record (field-args))
(def (type? v) (or (type:bottom? v) (type:name? v) (type:var? v) (type:app? v) (type:tuple? v) (type:record? v)))

;; A Pattys is an [Assqof Symbol Type]

(def type:int (type:name 'int (box #t)))
(def type:bool (type:name 'bool (box #t)))
(def type:bytestr (type:name 'bytestr (box #t)))

;; TODO: specify lists are covariant
(def typector:listof (type:name 'listof (box #t)))
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

;; type-actual : Type -> Type
(def (type-actual t)
  (match t
    ((type:name _ (box #t)) t)
    ((type:name x (box #f)) (error x "unknown type"))
    ((type:name _ (box ty)) ty)
    (_ t)))

;; type=? : Type Type -> Bool
(def (type=? a b)
  (let ((a (type-actual a)) (b (type-actual b)))
    (and (subtype? a b) (subtype? b a))))

;; subtype? : Type Type -> Bool
(def (subtype? a b)
  (match* ((type-actual a) (type-actual b))
    (((type:bottom) _) #t)
    (((type:name x (box #t)) (type:name y (box #t))) (eq? x y))
    (((type:var x) (type:var y)) (eq? x y))
    (((type:tuple as) (type:tuple bs))
     (and (= (length as) (length bs))
          (andmap subtype? as bs)))
    (((type:record as) (type:record bs))
     (symdict=? as bs subtype?))
    (((type:app f1 a1s) (type:app f2 a2s))
     ;; TODO: allow type constructors to specify covariant, contravariant, or invariant
     ;; for now conservatively assume everything is invariant
     (and (= (length a1s) (length a2s))
          (type=? f1 f2)
          (andmap type=? a1s a2s)))
    ((_ _) #f)))

;; type-join : Type Type -> Type
;; finds the type that is a supertype of both types, otherwise error
(def (type-join a b)
  (cond ((subtype? a b) b)
        ((subtype? b a) a)
        (else (error 'type-join "incompatible types"))))

;; types-join : [Listof Type] -> Type
;; finds the type that is a supertype of all types in the list
(def (types-join ts)
  (match ts
    ([] (type:bottom))
    ([t] t)
    ([a b] (type-join a b))
    (_ (type-join (car ts) (types-join (cdr ts))))))

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
            fldtys))))))

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
(def (tc-stmt env stx)
  (syntax-case stx (@ : quote def λ deftype defdata publish! verify!)
    ((@ _ _) (error 'tc-stmt "TODO: deal with @"))
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
                    (entry:type xs (parse-type env tyvars #'b)))))
    ((defdata x variant ...) (identifier? #'x)
     (let ((s (syntax-e #'x)))
       (def b (type:name (gensym s) (box #t)))
       (tc-defdata-variants (symdict-put env s (entry:type [] b))
                            []
                            b
                            #'(variant ...))))
    ((defdata (f 'x ...) variant ...) (identifier? #'f)
     (let ((s (syntax-e #'f))
           (xs (stx-map syntax-e #'(x ...))))
       (def b (type:app (type:name (gensym s) (box #t)) (map make-type:var xs)))
       (tc-defdata-variants (symdict-put env s (entry:type xs b))
                            xs
                            b
                            #'(variant ...))))
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
       (symdict-put env s (entry:val (tc-expr env #'expr)))))
    ((publish! x ...) (stx-andmap identifier? #'(x ...))
     (error 'tc-stmt "TODO: deal with publish!"))
    ((verify! x ...) (stx-andmap identifier? #'(x ...))
     (error 'tc-stmt "TODO: deal with verify!"))
    (expr
     (let ()
       (tc-expr env #'expr)
       env))))

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
    (x (identifier? #'x)
     (let ((s (syntax-e #'x)))
       (unless (symdict-has-key? env s)
         (error s "unbound identifier"))
       (match (symdict-ref env s)
         ((entry:val t) t)
         ((entry:ctor [] [] t) t))))
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
       (unless (symdict-has-key? env s)
         (error s "unbound identifier"))
       (match (symdict-ref env s)
         ;; monomorphic case
         ;; TODO: handle polymorphic case with unification
         ((or (entry:fun [] in-tys out-ty)
              (entry:ctor [] in-tys out-ty))
          (unless (= (stx-length #'(a ...)) (length in-tys))
            (error s "wrong number of arguments"))
          (stx-for-each (lambda (a t) (tc-expr/check env a t))
                        #'(a ...)
                        in-tys)
          out-ty))))))


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
        ((string? e) type:bytestr) ; represent as bytestrs using UTF-8
        ((bytes? e) type:bytestr)
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

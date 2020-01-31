(import <expander-runtime>
        (for-template :gerbil/core)
        :clan/pure/dict/symdict)

;; Typechecking glow-sexpr

;; Type system and scoping rules are closest
;; to ReasonML, but Record types are structural.

;; A Type is one of:
;;  - (type:name Symbol [Boxof (U Bool Type)])
;;  - (type:var Symbol)
;;  - (type:app Type [Listof Type])
;;  - (type:tuple [Listof Type])
;;  - (type:record [Symdictof Type])
(defstruct type:name (sym box))
(defstruct type:var (sym))
(defstruct type:app (fun args))
(defstruct type:tuple (args))
(defstruct type:record (field-args))
(def (type? v) (or (type:name? v) (type:var? v) (type:app? v) (type:tuple? v) (type:record? v)))

;; An EnvEntry is one of:
;;  - (entry:val Type)
;;  - (entry:fun [Listof Symbol] [Listof Type] Type)
;;  - (entry:ctor [Listof Symbol] [Listof Type] Type)
;;  - (entry:type [Listof Symbol] Type)
(defstruct entry:val (type))
(defstruct entry:fun (typarams input-types output-type))
(defstruct entry:ctor (typarams input-types output-type))
(defstruct entry:type (params type))
(def (env-entry? v) (or (entry:val? v) (entry:fun? v) (entry:ctor? v) (entry:type? v)))

;; An Env is a [Symdictof EnvEntry]
;; A TyvarEnv is a [Symdictof Type]

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
  (or (integer? e) (string? e) (boolean? e)))

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
          (parse-type env tyvars2 b)))))))

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
                            #'(variant ...))))))

;; tc-defdata-variant : Env [Listof Symbol] Type VariantStx -> [Cons Symbol EnvEntry]
(def (tc-defdata-variant env xs b stx)
  ;; party : TypeStx -> Type
  (def (party s) (parse-type env xs s))
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

(export #t)

(import :std/format :std/iter :std/stxutil
        :std/misc/list :std/sort :std/srfi/1
        (only-in :std/misc/rbtree symbol-cmp)
        <expander-runtime>
        :clan/list
        :clan/pure/dict/symdict
        (for-template :mukn/glow/compiler/syntax-context)
        :mukn/glow/compiler/syntax-context
        :mukn/glow/compiler/common
        :mukn/glow/compiler/alpha-convert/fresh
        :mukn/glow/compiler/typecheck/stx-prop
        :mukn/glow/compiler/typecheck/type
        :mukn/glow/compiler/method-resolve/method-resolve
        :mukn/glow/compiler/checkpointify/checkpointify
        :clan/base)

(def rest cdr)

(def id-ctors (make-hash-table-eq))

;; translate-pure-stmts : [StxListof StmtStx] -> [Listof SchemeStx]
(def (translate-pure-stmts stxs)
  (flatten1 (stx-map translate-pure-stmt stxs)))

;; translate-pure-stmt : StmtStx -> [Listof SchemeStx]
(def (translate-pure-stmt stx)
  (syntax-case stx (@ @label deftype defdata def ann return ignore! switch)
    ;; TODO: ignore @label whether pure or not
    ((@label _) [])
    ((def v e) [(restx1 stx ['def #'v (translate-pure-expr #'e)])])
    ((ignore! e) [(restx1 stx ['void (translate-pure-expr #'e)])])
    ((return e)
     ;; TODO: make sure this isn't meant to be an effect like a continuation-call, just to show tail-position
     [(translate-pure-expr #'e)])
    ((defdata . _) (translate-defdata stx))
    ((deftype . _) [])
    ((ann v _) (translate-pure-expr #'v))
    ((switch c cases ...)
     [(restx1 stx (cons* 'match (translate-pure-expr #'c) (stx-map translate-pure-switch-case #'(cases ...))))])))

;; translate-defdata : StmtStx -> [Listof SchemeStx]
(def (translate-defdata stx)
  (syntax-case stx ()
    ((_ spec variant ...)
     (with-syntax* ((tname (head-id #'spec))
                    (((vname fldty ...) ...) (stx-map normalize-defdata-variant #'(variant ...)))
                    ((vkw ...) (stx-map (compose symbol->keyword stx-e) #'(vname ...)))
                    ((tup ...) #'((Tuple fldty ...) ...))
                    (((vkws/tups ...) ...) #'((vkw tup) ...))
                    ((tvname ...) (stx-map (cut format-id #'tname "~a-~a" #'tname <>) #'(vname ...))))
       (for ((vnm (syntax->list #'(vname ...)))
             (tvnm (syntax->list #'(tvname ...))))
         (hash-put! id-ctors (stx-e vnm) tvnm))
       (cons*
         #'(define-type tname (Sum vkws/tups ... ...))
         #'(define-sum-constructors tname vname ...)
         (flatten1 (stx-map translate-defdata-variant #'(tvname ...) #'(variant ...))))))))

;; normalize-defdata-variant : DefdataVariantStx -> DefdataVariantStx
(def (normalize-defdata-variant stx)
  (if (identifier? stx) [stx] stx))

;; translate-defdata-variant : Identifier DefdataVariantStx -> [Listof SchemeStx]
(def (translate-defdata-variant tvname stx)
  (syntax-case stx ()
    (name (identifier? #'name)
     (with-syntax ((tv tvname))
       [#'(def name (tv (Tuple)))]))
    ((name t ...)
     (with-syntax ((tv tvname)
                   ((field ...) (stx-map (compose identifier-fresh head-id) #'(t ...))))
       [#'(def (name field ...) (tv (Tuple field ...)))]))))

;; translate-pure-switch-case : SwitchCaseStx -> SchemeStx
(def (translate-pure-switch-case stx)
  (syntax-case stx ()
    ((pat body ...)
     (restx stx (cons (translate-switch-pat #'pat) (translate-pure-stmts #'(body ...)))))))

;; translate-switch-pat : SwitchPatStx -> SchemeStx
(def (translate-switch-pat stx)
  (syntax-case stx (ann @var-pat @app-ctor @list @tuple @record @or-pat)
    ((ann a _) (translate-switch-pat #'a))
    ((@var-pat x) #'x)
    ((@app-ctor f a ...)
     (with-syntax ((ctor (hash-ref id-ctors (stx-e #'f) #'f))
                   ((a2 ...) (stx-map translate-switch-pat #'(a ...))))
       #'(ctor (vector a2 ...))))
    (blank (and (identifier? #'blank) (free-identifier=? #'blank #'_)) #'_)
    ((@list a ...)
     (with-syntax (((a2 ...) (stx-map translate-switch-pat #'(a ...))))
       #'(@list a2 ...)))
    ((@tuple a ...)
     (with-syntax (((a2 ...) (stx-map translate-switch-pat #'(a ...))))
       #'(vector a2 ...)))
    ((@record (fld a) ...)
     (with-syntax (((a2 ...) (stx-map translate-switch-pat #'(a ...))))
       #'(.o (fld a2) ...)))
    ((@or-pat a ...)
     (with-syntax (((a2 ...) (stx-map translate-switch-pat #'(a ...))))
       #'(or a2 ...)))
    (lit (stx-atomic-literal? #'lit) #'lit)))

;; translate-pure-expr : ExprStx -> SchemeStx
(def (translate-pure-expr stx)
  (syntax-case stx (ann @app @app-ctor @dot @list @tuple @record λ @make-interaction digest input)
    ((ann a _) (translate-pure-expr #'a))
    ((@app f a ...)
     (with-syntax (((f2 a2 ...) (stx-map translate-pure-expr #'(f a ...))))
       #'(%%app f2 a2 ...)))
    ((@app-ctor x)
     (with-syntax ((ctor (hash-ref id-ctors (stx-e #'x) #'x))) #'(ctor)))
    ((@dot a x)
     (with-syntax ((a2 (translate-pure-expr #'a)))
       #'(.@ a2 x)))
    ((@list a ...)
     (with-syntax (((a2 ...) (stx-map translate-pure-expr #'(a ...))))
       #'(@list a2 ...)))
    ((@tuple a ...)
     (with-syntax (((a2 ...) (stx-map translate-pure-expr #'(a ...))))
       #'(vector a2 ...)))
    ((@record (fld a) ...)
     (with-syntax (((a2 ...) (stx-map translate-pure-expr #'(a ...))))
       #'(.o (fld a2) ...)))
    ((λ params _ body ...)
     (with-syntax (((x ...) (stx-map head-id #'params))
                   ((b ...) (translate-pure-stmts #'(body ...))))
       #'(λ (x ...) b ...)))
    ((@make-interaction . _) (error '@make-interaction "used out of context, expected a pure-expr"))
    ((digest a ...)
     (with-syntax (((t ...) (stx-map expr-type-methods-expr #'(a ...))))
       #'(digest [(cons t a) ...])))
    ((input ty str)
     (with-syntax ((t (expr-type-methods-expr stx))
                   (s (translate-pure-expr #'str)))
       #'(input t s)))
    (lit (stx-atomic-literal? #'lit) #'lit)
    (_ stx)))

;; --------------------------------------------------------

;; expr-type-methods-expr : ExprStx -> SchemeStx
;; Produces a Scheme expression that evaluates to a Poo object
;; containing the methods for the type of the identifier, including un/marshaling
(def (expr-type-methods-expr stx)
  (def t (get-has-type stx))
  (unless t
    (error 'expr-type-methods-expr "missing type for" (syntax->datum stx)))
  (type-methods-expr t))

;; type-methods-expr : Type -> SchemeStx
(def (type-methods-expr t)
  (match t
    ((type:name 'Signature) #'Signature)
    ((type:name x) (get-tysym-methods-id x))
    ((type:name-subtype x _) (get-tysym-methods-id x))
    ((type:var _) (error 'type-methods-expr "type variables not supported" t))
    ((type:app f as)
     (cons* '%%app (type-methods-expr f) (map type-variance-pair-methods-expr as)))
    ((type:tuple as)
     (cons* #'Tuple (map type-methods-expr as)))
    (_ (error 'type-methods-expr "unknown type" t))))

;; type-variance-pair-methods-expr : TypeVariancePair -> SchemeStx
(def (type-variance-pair-methods-expr tvp)
  (match tvp
    ([#f #f] #'#f)
    ([_ t] (type-methods-expr t))
    ([t #f] (type-methods-expr t))))

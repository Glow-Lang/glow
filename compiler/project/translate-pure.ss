(export #t)

(import :std/format :std/iter :std/stxutil
        :std/misc/list :std/sort :std/srfi/1
        (only-in :std/misc/rbtree symbol-cmp)
        <expander-runtime>
        (for-template :mukn/glow/compiler/syntax-context)
        :mukn/glow/compiler/syntax-context
        :mukn/glow/compiler/common
        :mukn/glow/compiler/alpha-convert/fresh
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
    ((defdata _ variant ...)
     (flatten1 (stx-map translate-defdata-variant #'(variant ...))))
    ((deftype . _) [])
    ((ann v _) (translate-pure-expr #'v))
    ((switch c cases ...)
     [(restx1 stx (cons* 'match (translate-pure-expr #'c) (stx-map translate-pure-switch-case #'(cases ...))))])))

;; translate-defdata-variant : DefdataVariantStx -> [Listof SchemeStx]
(def (translate-defdata-variant stx)
  (syntax-case stx ()
    (name (identifier? #'name)
     (with-syntax ((name2 (identifier-fresh #'name)))
       (hash-put! id-ctors (stx-e #'name) #'name2)
       [#'(defstruct name2 () transparent: #t)
        #'(def name (name2))]))
    ((name t ...)
     (with-syntax (((field ...) (stx-map (compose identifier-fresh head-id) #'(t ...))))
       [#'(defstruct name (field ...) transparent: #t)]))))

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
       #'(ctor a2 ...)))
    (blank (and (identifier? #'blank) (free-identifier=? #'blank #'_)) #'_)
    ((@list a ...)
     (with-syntax (((a2 ...) (stx-map translate-switch-pat #'(a ...))))
       #'(@list a2 ...)))
    ((@tuple a ...)
     (with-syntax (((a2 ...) (stx-map translate-switch-pat #'(a ...))))
       #'(vector a2 ...)))
    ((@record (fld a) ...)
     (with-syntax (((a2 ...)
                    (map translate-switch-pat
                         (sort-record-fields->exprs #'((fld a) ...)))))
       #'(vector a2 ...)))
    ((@or-pat a ...)
     (with-syntax (((a2 ...) (stx-map translate-switch-pat #'(a ...))))
       #'(or a2 ...)))
    (lit (stx-atomic-literal? #'lit) #'lit)))

;; translate-pure-expr : ExprStx -> SchemeStx
(def (translate-pure-expr stx)
  (syntax-case stx (ann @app @app-ctor @list @tuple @record λ @make-interaction)
    ((ann a _) (translate-pure-expr #'a))
    ((@app f a ...)
     (with-syntax (((f2 a2 ...) (stx-map translate-pure-expr #'(f a ...))))
       #'(%%app f2 a2 ...)))
    ((@app-ctor x)
     (with-syntax ((ctor (hash-ref id-ctors (stx-e #'x) #'x))) #'(ctor)))
    ((@list a ...)
     (with-syntax (((a2 ...) (stx-map translate-pure-expr #'(a ...))))
       #'(@list a2 ...)))
    ((@tuple a ...)
     (with-syntax (((a2 ...) (stx-map translate-pure-expr #'(a ...))))
       #'(vector a2 ...)))
    ((@record (fld a) ...)
     (with-syntax (((a2 ...)
                    (map translate-pure-expr
                         (sort-record-fields->exprs #'((fld a) ...)))))
       #'(vector a2 ...)))
    ((λ params _ body ...)
     (with-syntax (((x ...) (stx-map head-id #'params))
                   ((b ...) (translate-pure-stmts #'(body ...))))
       #'(λ (x ...) b ...)))
    ((@make-interaction . _) (error '@make-interaction "used out of context, expected a pure-expr"))
    (lit (stx-atomic-literal? #'lit) #'lit)
    (_ stx)))

;; sort-record-fields->exprs : [StxListof [StxList Id ExprStx]] -> [Listof ExprStx]
(def (sort-record-fields->exprs stx)
  (map cadr
       (sort (stx-map syntax->list stx)
             (lambda (a b) (symbol<? (stx-e (car a)) (stx-e (car b)))))))

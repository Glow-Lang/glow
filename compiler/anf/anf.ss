(export #t)

(import :std/iter :std/srfi/1
        <expander-runtime>
        (for-template :glow/compiler/syntax-context)
        :glow/compiler/syntax-context
        ../common
        ../alpha-convert/fresh)

;; Conversion to A-Normal Form https://en.wikipedia.org/wiki/A-normal_form
;; We assume that variables have already been alpha-converted, typed, etc.

;; Put all the code into a form that is strongly sequential:
;; - Function calls and primitive calls must be "reduced" and only have constants and variables as arguments
;; - a definition must have a constant, variable or reduced call on the right-hand side
;; - When that is not the case, we reduce the nested expression to a sequence of temporary-variable definitions followed by a simple call or some tail expression
;; - instead of having a special "link" field in every object, we just assume an outer list that contains
;;   reduced expressions and definitions and ends in a tail expression.

;; Treatment of conditionals, which includes if's and switch's (maybe in the future have if a macro for switch?) [and choice's?]:
;; - assuming these branches are simple enough and do not introduce escaping bindings, and do not do non-local exits or other transaction-breaking side-effects, we can have branching on the right side of a definition.

;; anf-stmts : [Listof StmtStx] [Listof StmtStx] -> [Listof StmtStx]
;; the first argument is list of statements *to reduce*
;; the second argument is a reversed list of accumulated *reduced* statements
(def (anf-stmts stmts acc)
  (for/fold (acc acc) ((stmt stmts))
    (anf-stmt stmt acc)))

;; anf-stmt : StmtStx [Listof StmtStx] -> [Listof StmtStx]
;; accumulate new reduced statements for the current unreduced statement,
;; in reversed order at the beginning of the accumulator acc
(def (anf-stmt stx acc)
  (syntax-case stx (@ : quote def deftype defdata publish! verify!)
    ((@ _ _) (error 'anf-stmt "TODO: deal with @"))
    ((defdata . _) (cons stx acc))
    ((deftype . _) (cons stx acc))
    ((publish! . _) (cons stx acc))
    ((verify! . _) (cons stx acc))
    ((def . _) (anf-def stx acc))
    (expr (let-values (((reduced-expr acc) (anf-expr stx acc)))
            (cons reduced-expr acc)))))

;; anf-def : StmtStx [Listof StmtStx] -> [Listof StmtStx]
(def (anf-def stx acc)
  (syntax-case stx (def :)
    ((d x : type expr) (identifier? #'x)
     (let-values (((reduced-expr acc) (anf-expr #'expr acc)))
       (cons (restx stx [#'d #'x ': #'type reduced-expr]) acc)))
    ((d x expr) (identifier? #'x)
     (let-values (((reduced-expr acc) (anf-expr #'expr acc)))
       (cons (restx stx [#'d #'x reduced-expr]) acc)))))

;; trivial-expr? : ExprStx -> bool
;; is this expression trivial enough to be used in a call?
;; this implies evaluating it has no side-effect whatsoever and can commute with anything.
(def (trivial-expr? expr)
  (or (identifier? expr) (stx-atomic-literal? expr)))

;; generate-trivial-handle : ExprStx -> ExprStx
;; generates a trivial expression handle for expressions that aren't already trivial,
;; leaves already trivial expressions identical
(def (generate-trivial-handle expr)
  (if (trivial-expr? expr)
    expr
    (identifier-fresh #'tmp)))

;; anf-exprs : [Listof ExprStx] -> [Listof ExprStx]
;; maps each of a list of ExprStx to a trivial handle
(def (generate-trivial-handles exprs)
  (map generate-trivial-handle exprs))

;; anf-exprs : [Listof ExprStx] [Listof StmtStx] -> [Listof ExprStx] [Listof StmtStx]
;; reduces a list of ExprStx and accumulate statements to the (reversed) list of StmtStx
(def (anf-exprs exprs acc)
  (define xs (generate-trivial-handles exprs))
  (values xs
          (for/fold (acc acc) ((x xs) (expr exprs))
            (if (eq? x expr) acc
                (with-syntax ((x x) (expr expr))
                  (anf-stmt #'(def x expr) acc))))))

;; anf-standalone-expr : ExprStx -> ExprStx
;; reduces an ExprStx to a reduced block ExprStx
(def (anf-standalone-expr expr)
  (let-values (((reduced acc) (anf-expr expr [])))
    (restx expr `(,#'block ,@(reverse acc) ,reduced))))

;; anf-expr : ExprStx [Listof StmtStx] -> (values ExprStx [Listof StmtStx])
(def (anf-expr stx acc)
  (def (anf-multiarg-expr stx acc)
    (let-values (((xs acc) (anf-exprs (syntax->list (stx-cdr stx)) acc)))
      (values (restx stx [(stx-car stx) . xs]) acc)))

  (def (anf-onearg-expr stx acc)
     (let-values (((re acc) (anf-expr #'e acc)))
       (values (restx stx [(stx-car stx) re]) acc)))

  (syntax-case stx (@ ann @tuple @record @list if block switch λ require! assert! deposit! withdraw!)
    ((@ _ _) (error 'anf-expr "TODO: deal with @"))
    (x (identifier? #'x) (values stx acc))
    (lit (stx-atomic-literal? #'lit) (values stx acc))
    ((ann expr type)
     (let-values (((reduced-expr acc) (anf-expr #'expr acc)))
       (cons (restx stx [(stx-car stx) reduced-expr #'type]) acc)))
    ((@tuple e ...) (anf-multiarg-expr stx acc))
    ((@list e ...) (anf-multiarg-expr stx acc))
    ((@record (x e) ...)
     (and (stx-andmap identifier? #'(x ...))
          (check-duplicate-identifiers #'(x ...)))
     (let-values (((rs acc) (anf-exprs (syntax->list #'(e ...)) acc)))
       (values (restx stx (cons (stx-car stx) (map list #'(x ...) rs))) acc)))
    ((block b ...)
     (anf-body #'(b ...) acc))
    ((if c t e)
     (let-values (((rc acc) (anf-expr #'c acc)))
       (values (restx stx [(stx-car stx) rc (anf-standalone-expr #'t) (anf-standalone-expr #'e)])
               acc)))
    ((switch e swcase ...)
     (let-values (((re acc) (anf-expr #'e acc)))
       (values (restx stx (cons* (stx-car stx) re (stx-map anf-switch-case #'(swcase ...))))
               acc)))
    ((λ . _)
     (values (anf-lambda stx) acc))
    ((require! e) (anf-onearg-expr stx acc))
    ((assert! e) (anf-onearg-expr stx acc))
    ((deposit! e) (anf-onearg-expr stx acc))
    ((withdraw! x e) (identifier? #'x)
     (let-values (((re acc) (anf-expr #'e acc)))
       (restx stx [(stx-car stx) #'x re]) acc))
    ((f a ...) (identifier? #'f)
     (let-values (((ras acc) (anf-exprs (syntax->list #'(a ...)) acc)))
       (values (restx stx (cons (stx-car stx) ras)) acc)))))

;; anf-body : StmtsStx [Listof StmtStx] -> (values ExprStx [Listof StmtStx])
(def (anf-body stx acc)
  (syntax-case stx ()
    ((b ... e)
     (anf-expr #'e (anf-stmts (syntax->list #'(b ...)) acc)))))

;; anf-standalone-body : StmtsStx -> [Listof StmtStx]
(def (anf-standalone-body stx)
  (let-values (((e acc) (anf-body stx [])))
    (append-reverse acc (list e))))

;; anf-switch-case : SwitchCaseStx -> SwitchCaseStx
(def (anf-switch-case stx)
  (syntax-case stx ()
    ((pat body ...)
     (restx stx (cons #'pat (anf-standalone-body #'(body ...)))))))

;; anf-lambda : LambdaStx -> LambdaStx
(def (anf-lambda stx)
  (syntax-case stx (: λ)
    ((l params : out-type body ...)
     (restx stx `(,#'l params : ,#'out-type ,@(anf-standalone-body #'(body ...)))))
    ((l params body ...)
     (restx stx `(,#'l params ,@(anf-standalone-body #'(body ...)))))))

;; Conform to pass convention.
;; anf : [Listof StmtStx] UnusedTable Env -> (values [Listof StmtStx] UnusedTable Env)
(def (anf stmts unused-table env)
  (parameterize ((current-unused-table unused-table))
    (values (reverse (anf-stmts stmts [])) unused-table env)))

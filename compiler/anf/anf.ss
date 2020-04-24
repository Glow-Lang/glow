(export #t)

(import :std/iter :std/srfi/1
        :std/misc/repr :clan/utils/debug ;; DEBUG
        <expander-runtime>
        (for-template :glow/compiler/syntax-context)
        :glow/compiler/syntax-context
        :clan/utils/base
        ../common
        ../alpha-convert/fresh)

;; Conversion to A-Normal Form https://en.wikipedia.org/wiki/A-normal_form
;; We assume that variables have already been alpha-converted, typed, etc.

;; TODO: introduce explicit return's in ANF, so we can treat them specially wrt CPS and safe-pointing?
;; Or do that in txify?

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
;; INVARIANT: (anf-stmts stmts acc) = (append (anf-stmts stmts []) acc)
(def (anf-stmts stmts acc)
  (for/fold (acc acc) ((stmt stmts))
    (anf-stmt stmt acc)))

;; anf-stmt : StmtStx [Listof StmtStx] -> [Listof StmtStx]
;; accumulate new reduced statements for the current unreduced statement,
;; in reversed order at the beginning of the accumulator acc
;; INVARIANT: (anf-stmt stx acc) = (append (anf-stmt stx []) acc)
(def (anf-stmt stx acc)
  (syntax-case stx (@ @interaction @verifiably @publicly splice : quote def deftype defdata publish! verify! deposit!)
    ((@interaction x s) (append (anf-at-interaction stx) acc))
    ;; TODO: delete cases for @verifiably and @publicly once they are desugared away in a previous pass
    ((@verifiably . _) (cons stx acc))
    ((@publicly . _) (cons stx acc))
    ((@ p s) (identifier? #'p) (append (anf-at-participant stx) acc))
    ((splice s ...) (anf-stmts (syntax->list #'(s ...)) acc))
    ((defdata . _) (anf-defdata stx acc))
    ((deftype . _) (cons stx acc))
    ((publish! . _) (cons stx acc))
    ;; TODO: delete case for verify! once it's desugared away in a previous pass
    ((verify! . _) (cons stx acc))
    ((def . _) (anf-def stx acc))
    (expr (let-values (((reduced-expr acc) (anf-expr stx acc)))
            (cons reduced-expr acc)))))

;; anf-at-interaction : StmtStx -> [Listof StmtStx]
(def (anf-at-interaction stx)
  (syntax-case stx ()
    ((a x s)
     (match (anf-stmt #'s [])
       ([s2 . more]
        (assert! (null? more))
        [(restx stx [#'a #'x s2])])))))

;; anf-at-participant : StmtStx -> [Listof StmtStx]
;; Returns the result statements in reverse order
(def (anf-at-participant stx)
  (syntax-case stx ()
    ((at p s) (identifier? #'p)
     (map (lambda (s2) (restx1 stx [#'at #'p s2])) (anf-stmt #'s [])))))

;; anf-defdata : StmtStx [Listof StmtStx] -> [Listof StmtStx]
(def (anf-defdata stx acc)
  (cons
   (syntax-case stx ()
     ((_ spec variant ... with: rtvalue)
      (retail-stx stx `(,#'spec ,@(syntax->list #'(variant ...))
                                with: ,(anf-standalone-expr #'rtvalue))))
     ((_ spec variant ...) stx))
   acc))

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

;; anf-arg-expr : ExprStx [Listof StmtStx] -> (values ExprStx [ListofStmtStx])
;; Produces an output expr that's trivial, as an argument in a function application
(def (anf-arg-expr expr stmts)
  (let-values (((expr stmts) (anf-expr expr stmts)))
    (if (trivial-expr? expr)
      (values expr stmts)
      (let ((t (identifier-fresh #'tmp)))
        (values t
                (with-syntax ((t t) (expr expr))
                  (anf-stmt #'(def t expr) stmts)))))))

;; anf-arg-exprs : [Listof ExprStx] [Listof StmtStx] -> [Listof ExprStx] [Listof StmtStx]
;; reduces a list of ExprStx and accumulate statements to the (reversed) list of StmtStx
(def (anf-arg-exprs exprs stmts)
  (let loop ((exprs exprs) (ts []) (stmts stmts))
    (match exprs
      ([] (values (reverse ts) stmts))
      ([expr . rst]
       (let-values (((t stmts) (anf-arg-expr expr stmts)))
         (loop rst (cons t ts) stmts))))))

;; anf-standalone-expr : ExprStx -> ExprStx
;; reduces an ExprStx to a reduced block ExprStx
(def (anf-standalone-expr expr)
  (let-values (((reduced acc) (anf-expr expr [])))
    (if (null? acc) reduced
        (restx expr `(,#'block ,@(reverse acc) ,reduced)))))

;; anf-multiarg-expr : ExprStx [Listof StmtStx] -> (values ExprStx [Listof StmtStx])
(def (anf-multiarg-expr stx acc)
  (syntax-case stx ()
    ((hd args ...)
     (let-values (((xs acc) (anf-arg-exprs (syntax->list #'(args ...)) acc)))
       (values (restx stx [#'hd . xs]) acc)))))

;; anf-expr : ExprStx [Listof StmtStx] -> (values ExprStx [Listof StmtStx])
(def (anf-expr stx acc)
  (syntax-case stx (@ ann @tuple @record @list @dot @app and or if block splice switch λ == input require! assert! deposit! withdraw!)
    ((@ _ _) (error 'anf-expr "TODO: deal with @"))
    (x (identifier? #'x) (values stx acc))
    (lit (stx-atomic-literal? #'lit) (values stx acc))
    ((ann expr type)
     (let-values (((reduced-expr acc) (anf-arg-expr #'expr acc)))
       (values (restx stx [(stx-car stx) reduced-expr #'type]) acc)))
    ((@tuple e ...) (anf-multiarg-expr stx acc))
    ((@list e ...) (anf-multiarg-expr stx acc))
    ((@record (x e) ...)
     (and (stx-andmap identifier? #'(x ...))
          (check-duplicate-identifiers #'(x ...)))
     (let-values (((rs acc) (anf-arg-exprs (syntax->list #'(e ...)) acc)))
       (values (retail-stx stx (map list (syntax->list #'(x ...)) rs)) acc)))
    ((@dot x f)
     (let-values (((reduced-expr acc) (anf-arg-expr #'x acc)))
       (values (restx #'stx ['@dot reduced-expr #'f]) acc)))
    ((block b ...) (anf-body #'(b ...) acc))
    ((splice b ...) (anf-body #'(b ...) acc))
    ((if c t e)
     (let-values (((rc acc) (anf-arg-expr #'c acc)))
       (values (retail-stx stx [rc (anf-standalone-expr #'t) (anf-standalone-expr #'e)])
               acc)))
    ((switch e swcase ...)
     (let-values (((re acc) (anf-arg-expr #'e acc)))
       (values (restx stx (cons* (stx-car stx) re
                                 (stx-map anf-switch-case #'(swcase ...))))
               acc)))
    ((λ . _) (values (anf-lambda stx) acc))
    ((== a b) (anf-multiarg-expr stx acc))
    ((input type tag)
     (let-values (((rtag acc) (anf-arg-expr #'tag acc)))
       (values (restx stx [(stx-car stx) #'type rtag]) acc)))
    ((require! e) (anf-multiarg-expr stx acc))
    ((assert! e) (anf-multiarg-expr stx acc))
    ((deposit! e) (anf-multiarg-expr stx acc))
    ((withdraw! x e) (identifier? #'x) (anf-multiarg-expr stx acc))
    ((digest e) (anf-multiarg-expr stx acc))
    ((sign e) (anf-multiarg-expr stx acc))
    ((@app e ...) (anf-multiarg-expr stx acc))))

;; anf-body : StmtsStx [Listof StmtStx] -> (values ExprStx [Listof StmtStx])
(def (anf-body stx acc)
  (syntax-case stx ()
    ((b ... e)
     (anf-expr #'e (anf-stmts (syntax->list #'(b ...)) acc)))))

;; anf-standalone-body : StmtsStx -> [Listof StmtStx]
(def (anf-standalone-body stx)
  (let-values (((e acc) (anf-body stx [])))
    (reverse (cons e acc))))

;; anf-switch-case : SwitchCaseStx -> SwitchCaseStx
(def (anf-switch-case stx)
  (syntax-case stx ()
    ((pat body ...)
     (retail-stx stx (anf-standalone-body #'(body ...))))))

;; anf-lambda : LambdaStx -> LambdaStx
(def (anf-lambda stx)
  (syntax-case stx (: λ)
    ((l params : out-type body ...)
     (restx stx (cons* #'l #'params ': #'out-type (anf-standalone-body #'(body ...)))))
    ((l params body ...)
     (restx stx (cons* #'l #'params (anf-standalone-body #'(body ...)))))))

;; anf : [Listof StmtStx] UnusedTable -> [Listof StmtStx]
(def (anf stmts unused-table)
  (parameterize ((current-unused-table unused-table))
    (reverse (anf-stmts stmts []))))

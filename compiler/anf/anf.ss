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

;; Put all the code into a form that is strongly sequential:
;; - Function calls and primitive calls must be "reduced" and only have constants and variables as arguments
;; - a definition must have a constant, variable or reduced call on the right-hand side
;; - When that is not the case, we reduce the nested expression to a sequence of temporary-variable definitions followed by a simple call or some tail expression
;; - instead of having a special "link" field in every object, we just assume an outer list that contains
;;   reduced expressions and definitions and ends in a tail expression.

;; Treatment of conditionals, which includes if's and switch's (maybe in the future have if a macro for switch?) [and choice's?]:
;; - assuming these branches are simple enough and do not introduce escaping bindings, and do not do non-local exits or other transaction-breaking side-effects, we can have branching on the right side of a definition.

;; type MaybeParticipant = (Or ParticipantIdentifier '#f)

;; anf-stmts : [Listof StmtStx] MaybeParticipant [Listof StmtStx] -> [Listof StmtStx]
;; the first argument is list of statements *to reduce*
;; the second argument is a reversed list of accumulated *reduced* statements
(def (anf-stmts stmts participant acc)
  (for/fold (acc acc) ((stmt stmts))
    (anf-stmt stmt participant acc)))

;; anf-stmt : StmtStx MaybeParticipant [Listof StmtStx] -> [Listof StmtStx]
;; accumulate new reduced statements for the current unreduced statement,
;; in reversed order at the beginning of the accumulator acc
(def (anf-stmt stx participant acc)
  (syntax-case stx (@ @interaction verifiably publicly : quote def deftype defdata publish! deposit!)
    ((@interaction x s) (match (anf-stmt #'s participant [])
                          ([stmt . more]
                           (assert! (null? more))
                           [(restx stx [(stx-car stx) #'x stmt]) . acc])))
    ((@ p s) (identifier? #'p) (anf-stmt #'s #'p acc))
    ((defdata . _) (cons stx acc))
    ((deftype . _) (cons stx acc))
    ((publish! . _) (cons (at-stx participant stx) acc))
    ((def . _) (anf-def stx participant acc))
    (expr (let-values (((reduced-expr acc) (anf-expr stx participant acc)))
            (cons (at-stx participant reduced-expr) acc)))))

;; anf-def : StmtStx MaybeParticipant [Listof StmtStx] -> [Listof StmtStx]
(def (anf-def stx participant acc)
  (syntax-case stx (def :)
    ((d x : type expr) (identifier? #'x)
     (let-values (((reduced-expr acc) (anf-expr #'expr participant acc)))
       (cons (at-stx participant (restx stx [#'d #'x ': #'type reduced-expr])) acc)))
    ((d x expr) (identifier? #'x)
     (let-values (((reduced-expr acc) (anf-expr #'expr participant acc)))
       (cons (at-stx participant (restx stx [#'d #'x reduced-expr])) acc)))))

;; trivial-expr? : ExprStx -> bool
;; is this expression trivial enough to be used in a call?
;; this implies evaluating it has no side-effect whatsoever and can commute with anything.
(def (trivial-expr? expr)
  (or (identifier? expr) (stx-atomic-literal? expr)))

;; anf-exprs : [Listof ExprStx] MaybeParticipant [Listof StmtStx] -> [Listof ExprStx] [Listof StmtStx]
;; reduces a list of ExprStx and accumulate statements to the (reversed) list of StmtStx
(def (anf-exprs exprs participant stmts)
  (cons->values
   (for/fold (acc [[] . stmts]) ((expr exprs))
     (let-values (((ts stmts) (cons->values acc)))
       (if (trivial-expr? expr)
         (cons (cons expr ts) stmts)
         (let-values (((expr stmts) (anf-expr expr participant stmts)))
           (let ((t (identifier-fresh #'tmp)))
             (cons (cons t ts)
                   (with-syntax ((t t) (expr expr))
                     (anf-stmt #'(def t expr) participant stmts))))))))))

;; anf-standalone-expr : ExprStx MaybeParticipant -> ExprStx
;; reduces an ExprStx to a reduced block ExprStx
(def (anf-standalone-expr expr participant)
  (let-values (((reduced acc) (anf-expr expr participant [])))
    (if (null? acc) reduced
        (restx expr `(,#'block ,@(reverse acc) ,reduced)))))

;; anf-multiarg-expr : ExprStx MaybeParticipant [Listof StmtStx] -> (values ExprStx [Listof StmtStx])
(def (anf-multiarg-expr stx participant acc)
  (syntax-case stx ()
    ((hd args ...)
     (let-values (((xs acc) (anf-exprs (syntax->list #'(args ...)) participant acc)))
       (values (restx stx [#'hd . xs]) acc)))))

;; anf-expr : ExprStx MaybeParticipant [Listof StmtStx] -> (values ExprStx [Listof StmtStx])
(def (anf-expr stx participant acc)
  (syntax-case stx (@ ann @tuple @record @list @app if block splice switch λ require! assert! deposit! withdraw!)
    ((@ _ _) (error 'anf-expr "TODO: deal with @"))
    (x (identifier? #'x) (values stx acc))
    (lit (stx-atomic-literal? #'lit) (values stx acc))
    ((ann expr type)
     (let-values (((reduced-expr acc) (anf-expr #'expr participant acc)))
       (values (restx stx [(stx-car stx) reduced-expr #'type]) acc)))
    ((@tuple e ...) (anf-multiarg-expr stx participant acc))
    ((@list e ...) (anf-multiarg-expr stx participant acc))
    ((@record (x e) ...)
     (and (stx-andmap identifier? #'(x ...))
          (check-duplicate-identifiers #'(x ...)))
     (let-values (((rs acc) (anf-exprs (syntax->list #'(e ...)) participant acc)))
       (values (restx stx (cons (stx-car stx) (map list (syntax->list #'(x ...)) rs))) acc)))
    ((block b ...) (anf-body #'(b ...) participant acc))
    ((splice b ...) (anf-body #'(b ...) participant acc))
    ((if c t e)
     (let-values (((rc acc) (anf-expr #'c participant acc)))
       (values (restx stx [(stx-car stx) rc (anf-standalone-expr #'t participant)
                           (anf-standalone-expr #'e participant)])
               acc)))
    ((switch e swcase ...)
     (let-values (((re acc) (anf-expr #'e participant acc)))
       (values (restx stx (cons* (stx-car stx) re
                                 (stx-map (cut anf-switch-case <> participant) #'(swcase ...))))
               acc)))
    ((λ . _) (values (anf-lambda stx participant) acc))
    ((require! e) (anf-multiarg-expr stx participant acc))
    ((assert! e) (anf-multiarg-expr stx participant acc))
    ((deposit! e) (anf-multiarg-expr stx participant acc))
    ((@app e ...) (anf-multiarg-expr stx acc))
    ((withdraw! x e) (identifier? #'x) (anf-multiarg-expr stx acc))))

;; anf-body : StmtsStx [Listof StmtStx] -> (values ExprStx [Listof StmtStx])
(def (anf-body stx participant acc)
  (syntax-case stx ()
    ((b ... e)
     (anf-expr #'e participant (anf-stmts (syntax->list #'(b ...)) participant acc)))))

;; anf-standalone-body : StmtsStx MaybeParticipant -> [Listof StmtStx]
(def (anf-standalone-body stx participant)
  (let-values (((e acc) (anf-body stx participant [])))
    (reverse (anf-stmt e participant acc))))

;; anf-switch-case : SwitchCaseStx MaybeParticipant -> SwitchCaseStx
(def (anf-switch-case stx participant)
  (syntax-case stx ()
    ((pat body ...)
     (restx stx (cons #'pat (anf-standalone-body #'(body ...) participant))))))

;; anf-lambda : LambdaStx -> LambdaStx
(def (anf-lambda stx participant)
  (syntax-case stx (: λ)
    ((l params : out-type body ...)
     (restx stx (cons* #'l #'params ': #'out-type (anf-standalone-body #'(body ...) participant))))
    ((l params body ...)
     (restx stx (cons* #'l #'params (anf-standalone-body #'(body ...) participant))))))

;; Conform to pass convention.
;; anf : [Listof StmtStx] UnusedTable Env -> (values [Listof StmtStx] UnusedTable Env)
(def (anf stmts unused-table env)
  (parameterize ((current-unused-table unused-table))
    (values (reverse (anf-stmts stmts #f [])) unused-table env)))

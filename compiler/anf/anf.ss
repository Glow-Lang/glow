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

;; type MaybeParticipant = (Or ParticipantIdentifier '#f)

;; anf-stmts : [Listof StmtStx] MaybeParticipant [Listof StmtStx] -> [Listof StmtStx]
;; the first argument is list of statements *to reduce*
;; the second argument is a reversed list of accumulated *reduced* statements
(def (anf-stmts stmts participant acc)
  (for/fold (acc acc) ((stmt stmts))
    (anf-stmt stmt participant acc)))

;; anf-at : StmtStx MaybeParticipant -> StmtStx
(def (anf-at stx participant)
  (with-syntax ((s stx) (p participant))
    (if participant #'(@ p s) stx)))

;; anf-stmt : StmtStx MaybeParticipant [Listof StmtStx] -> [Listof StmtStx]
;; accumulate new reduced statements for the current unreduced statement,
;; in reversed order at the beginning of the accumulator acc
(def (anf-stmt stx participant acc)
  (syntax-case stx (@ @interaction verifiably publicly : quote def deftype defdata publish! deposit!)
    ((@interaction x s) (restx stx [(stx-car stx) #'x (anf-stmt #'s participant acc)]))
    ((@ p s) (identifier? #'p) (anf-stmt #'s #'p acc))
    ((defdata . _) (cons stx acc))
    ((deftype . _) (cons stx acc))
    ((publish! . _) (with-syntax ((p participant)) (cons #'(@ p stx) acc)))
    ((deposit! e) (anf-at (anf-onearg-expr stx participant acc) participant))
    ((def . _) (anf-def stx participant acc))
    (expr (let-values (((reduced-expr acc) (anf-expr stx participant acc)))
            (cons (anf-at reduced-expr participant) acc)))))

;; anf-def : StmtStx MaybeParticipant [Listof StmtStx] -> [Listof StmtStx]
(def (anf-def stx participant acc)
  (syntax-case stx (def :)
    ((d x : type expr) (identifier? #'x)
     (let-values (((reduced-expr acc) (anf-expr #'expr participant acc)))
       (cons (anf-at (restx stx [#'d #'x ': #'type reduced-expr]) participant) acc)))
    ((d x expr) (identifier? #'x)
     (let-values (((reduced-expr acc) (anf-expr #'expr participant acc)))
       (cons (anf-at (restx stx [#'d #'x reduced-expr]) participant) acc)))))

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

;; anf-exprs : [Listof ExprStx] MaybeParticipant [Listof StmtStx] -> [Listof ExprStx] [Listof StmtStx]
;; reduces a list of ExprStx and accumulate statements to the (reversed) list of StmtStx
(def (anf-exprs exprs participant acc)
  (define xs (generate-trivial-handles exprs))
  (values xs
          (for/fold (acc acc) ((x xs) (expr exprs))
            (if (eq? x expr) acc
                (with-syntax ((x x) (expr expr))
                  (anf-stmt #'(def x expr) participant acc))))))

;; anf-standalone-expr : ExprStx MaybeParticipant -> ExprStx
;; reduces an ExprStx to a reduced block ExprStx
(def (anf-standalone-expr expr participant)
  (let-values (((reduced acc) (anf-expr expr participant [])))
    (restx expr `(,#'block ,@(reverse acc) ,reduced))))

;; anf-multiarg-expr : ExprStx MaybeParticipant [Listof StmtStx] -> (values ExprStx [Listof StmtStx])
(def (anf-multiarg-expr stx participant acc)
  (let-values (((xs acc) (anf-exprs (syntax->list (stx-cdr stx)) participant acc)))
    (values (restx stx [(stx-car stx) . xs]) acc)))

;; anf-onearg-expr : ExprStx MaybeParticipant [Listof StmtStx] -> (values ExprStx [Listof StmtStx])
(def (anf-onearg-expr stx participant acc)
  (let-values (((re acc) (anf-expr #'e participant acc)))
    (values (restx stx [(stx-car stx) re]) acc)))

;; anf-expr : ExprStx MaybeParticipant [Listof StmtStx] -> (values ExprStx [Listof StmtStx])
(def (anf-expr stx participant acc)
  (syntax-case stx (@ ann @tuple @record @list if block switch λ require! assert! deposit! withdraw!)
    ((@ _ _) (error 'anf-expr "TODO: deal with @"))
    (x (identifier? #'x) (values stx acc))
    (lit (stx-atomic-literal? #'lit) (values stx acc))
    ((ann expr type)
     (let-values (((reduced-expr acc) (anf-expr #'expr participant acc)))
       (cons (restx stx [(stx-car stx) reduced-expr #'type]) acc)))
    ((@tuple e ...) (anf-multiarg-expr stx participant acc))
    ((@list e ...) (anf-multiarg-expr stx participant acc))
    ((@record (x e) ...)
     (and (stx-andmap identifier? #'(x ...))
          (check-duplicate-identifiers #'(x ...)))
     (let-values (((rs acc) (anf-exprs (syntax->list #'(e ...)) participant acc)))
       (values (restx stx (cons (stx-car stx) (map list #'(x ...) rs))) acc)))
    ((block b ...)
     (anf-body #'(b ...) participant acc))
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
    ((λ . _)
     (values (anf-lambda stx participant) acc))
    ((require! e) (anf-onearg-expr stx participant acc))
    ((assert! e) (anf-onearg-expr stx participant acc))
    ((deposit! e) (anf-onearg-expr stx participant acc))
    ((withdraw! x e) (identifier? #'x)
     (let-values (((re acc) (anf-expr #'e participant acc)))
       (restx stx [(stx-car stx) #'x re]) acc))
    ((f a ...) (identifier? #'f)
     (let-values (((ras acc) (anf-exprs (syntax->list #'(a ...)) participant acc)))
       (values (restx stx (cons (stx-car stx) ras)) acc)))))

;; anf-body : StmtsStx [Listof StmtStx] -> (values ExprStx [Listof StmtStx])
(def (anf-body stx participant acc)
  (syntax-case stx ()
    ((b ... e)
     (anf-expr #'e participant (anf-stmts (syntax->list #'(b ...)) participant acc)))))

;; anf-standalone-body : StmtsStx -> [Listof StmtStx]
(def (anf-standalone-body stx participant)
  (let-values (((e acc) (anf-body stx participant [])))
    (append-reverse acc (list e))))

;; anf-switch-case : SwitchCaseStx MaybeParticipant -> SwitchCaseStx
(def (anf-switch-case stx participant)
  (syntax-case stx ()
    ((pat body ...)
     (restx stx (cons #'pat (anf-standalone-body #'(body ...) participant))))))

;; anf-lambda : LambdaStx -> LambdaStx
(def (anf-lambda stx participant)
  (syntax-case stx (: λ)
    ((l params : out-type body ...)
     (restx stx `(,#'l params : ,#'out-type ,@(anf-standalone-body #'(body ...) participant))))
    ((l params body ...)
     (restx stx `(,#'l params ,@(anf-standalone-body #'(body ...) participant))))))

;; Conform to pass convention.
;; anf : [Listof StmtStx] UnusedTable Env -> (values [Listof StmtStx] UnusedTable Env)
(def (anf stmts unused-table env)
  (parameterize ((current-unused-table unused-table))
    (values (reverse (anf-stmts stmts #f [])) unused-table env)))

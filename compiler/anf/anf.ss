(export #t)

(import :std/iter :std/misc/hash :std/srfi/1 :std/sugar
        <expander-runtime>
        (for-template :mukn/glow/compiler/syntax-context)
        :mukn/glow/compiler/syntax-context
        :mukn/glow/compiler/typecheck/stx-prop
        :clan/base
        ../common
        ../alpha-convert/fresh)
(import :std/misc/repr :clan/debug) ;; XXX DEBUG

;; Conversion to A-Normal Form https://en.wikipedia.org/wiki/A-normal_form
;; We assume that variables have already been alpha-converted, typed, etc.

;; TODO: explain the output grammar of it in Scribble, in docs/internal/anf
;; TODO: do we want phi-nodes for conditionals?

;; Put all the code into a form that is strongly sequential,
;; where all remaining "expressions" are trivial, i.e. variables and constants:
;; - Function calls and primitive calls must be "reduced"
;;   by only having constants and variables as arguments
;; - A definition must have a constant, variable, reduced computation or reduced function call
;;   on the right-hand side.
;; - A reduced computation must be the right-hand side of a definition.
;; - A reduced function call must be the right-hand side of a definition or a return (tail call)
;; - In any nested source expression for which the above doesn't hold,
;;   we reduce the nested expression to a sequence of temporary-variable definitions
;;   followed by a simple call or some tail expression
;; - A variable may be assigned multiple times---in the many mutually exclusive branches
;;   of a conditional expression. That is how "the value" of the expression percolates to the end.
;; - Instead of having a special "link" field in every statement object that links to
;;   the next statement (or possible multiple next statements), "just" assume
;;   an outer list that contains reduced expressions and definitions and
;;   ends in a tail expression.
;; - While reducing expressions, we pass a "reduced continuation", which is either
;;   the left hand of an assignment, or of a return statement;
;;   thus we can properly compile tail calls, etc.
;; - Conditionals expressions thus become statements that differently define a same variable (or return)
;;   at the end.


;; A syntactic continuation is one of ['ignore!] ['def #'v] or ['return],
;; and indicates whether the value of current expression will be ignored
;; (wherein the expression is used for side-effect only), or bound to a variable,
;; or used to return a value from the current function.
;; type KontStx = (#'ignore) | (#'def IdentifierStx) | (#'return)

;; This table is used to allocate a fresh identifier *after* an expression is otherwise expanded
(def current-tmp (make-parameter (hash)))
(def (tmp-var tag) (hash-ensure-ref (current-tmp) tag (cut identifier-fresh 'tmp)))
(def (deftmp tag expr)
  (with-syntax ((v (tmp-var tag)) (e expr))
    (set-has-type! #'v (get-has-type #'e))
    (restx1 tag #'(def v e))))

(def (anf-kontinue-expr k x acc)
  (def ke (syntax-case k (ignore! return new def)
            ((new tag) (deftmp #'tag x))
            ((ignore!) [#'ignore! x])
            ((return) [#'return x])
            ((def v) [#'def #'v x])))
  (cons (restx1 k ke) acc))

;; A statement returns unit.
(def (anf-kontinue-stmt k acc)
  (syntax-case k (ignore! return new def)
    ((ignore!) acc) ;; ignore
    ((return) (cons #'(return (@tuple)) acc)) ;; fall through, return unit
    ((new tag) (cons (deftmp #'tag #'(@tuple)) acc))
    ((def v) (cons #'(def v (@tuple)) acc)))) ;; fall through, assign unit

;; anf-stmts : KontStx [Listof StmtStx] [Listof StmtStx] -> [Listof StmtStx]
;; the first argument is the syntactic continuation for the last expression
;; the second argument is list of statements *to reduce*
;; the third argument is a reversed list of accumulated *reduced* statements
;; the result is a list of reduced statements in the accumulated (i.e. reverse) order
(def (anf-stmts k stmts acc)
  (match stmts
    ([] (anf-kontinue-stmt k acc))
    ([x] (anf-stmt k x acc))
    ([x . tl] (anf-stmts k tl (anf-stmt #'(ignore!) x acc)))))

;; anf-stmt : KontStx StmtStx [Listof StmtStx] -> [Listof StmtStx]
;; accumulate new reduced statements for the current unreduced statement,
;; in reversed order at the beginning of the accumulator acc,
;; with the syntactic continuation k to the current statement.
;; INVARIANT: (anf-stmt stx acc) = (append (anf-stmt stx []) acc)
(def (anf-stmt k stx acc)
  (syntax-case stx (@ splice @debug-label : quote def deftype defdata publish! deposit!)
    ((@ p s) (identifier? #'p) (append (anf-at-participant k stx) acc))
    ((splice . body) (anf-body k #'body acc))
    ((@debug-label . _) (anf-kontinue-stmt k (cons stx acc)))
    ((defdata . _) (anf-kontinue-stmt k (cons stx acc)))
    ((deftype . _) (anf-kontinue-stmt k (cons stx acc)))
    ((publish! . _) (anf-kontinue-stmt k (cons stx acc)))
    ((def . _) (anf-kontinue-stmt k (anf-def stx acc)))
    (expr (anf-expr k stx acc))))

;; anf-at-participant : KontStx StmtStx -> [Listof StmtStx]
;; Returns the result statements in reverse order
(def (anf-at-participant k stx)
  (syntax-case stx ()
    ((at p s) (identifier? #'p)
     (map (lambda (s2) (restx1 stx [#'at #'p s2])) (anf-stmt k #'s [])))))

;; anf-def : StmtStx [Listof StmtStx] -> [Listof StmtStx]
(def (anf-def stx acc)
  (syntax-case stx ()
    ((d x expr)
     (anf-expr (restx1 stx #'(d x)) #'expr acc))))

;; anf-arg-expr : ExprStx [Listof StmtStx] -> (values TrivialExprStx [ListofStmtStx])
;; Produces an output expr that's trivial, as an argument in a function application
(def (anf-arg-expr expr stmts)
  (if (trivial-expr? expr)
    (values expr stmts)
    (anf-tmpify expr stmts)))

;; anf-tmpify : ExprStx [Listof StmtStx] -> (values IdentifierStx [ListofStmtStx])
(def (anf-tmpify expr stmts)
  (def stmts2 (anf-stmt (restx1 expr [#'new expr]) expr stmts))
  (def t (tmp-var expr))
  (set-has-type! t (get-has-type expr))
  (values t stmts2))

;; anf-arg-exprs : [Listof ExprStx] [Listof StmtStx] -> [Listof TrivialExprStx] [Listof StmtStx]
;; reduces a list of ExprStx and accumulate statements to the (reversed) list of StmtStx
(def (anf-arg-exprs exprs stmts)
  (let loop ((exprs exprs) (ts []) (stmts stmts))
    (match exprs
      ([] (values (reverse ts) stmts))
      ([expr . rst]
       (let-values (((t stmts) (anf-arg-expr expr stmts)))
         (loop rst (cons t ts) stmts))))))

;; Reduce an ExprStx to a reduced block StmtStx
;; Note that what in previous pass was an ExprStx in the next pass becomes a [Listof StmtStx]
;; anf-standalone-expr : KontStx ExprStx -> [Listof StmtStx]
(def (anf-standalone-expr k expr)
  (reverse (anf-expr k expr [])))

;; type SimpleExprStx = TrivialExprStx | (#'@app TrivialExprStx ...)

;; Simplify an expression with a known head constructor
;; anf-multiarg-expr : ExprStx [Listof StmtStx] -> (values ExprStx [Listof StmtStx])
(def (anf-multiarg-expr stx acc)
  (syntax-case stx ()
    ((hd args ...)
     (let-values (((xs acc2) (anf-arg-exprs (syntax->list #'(args ...)) acc)))
       (values (restx stx [#'hd . xs]) acc2)))))

;; Case where the expr constructor returns a value that is not simple
;; anf-k-multiarg-expr : KontStx ExprStx [Listof StmtStx] -> [Listof StmtStx]
(def (anf-k-multiarg-expr k stx acc)
  (defvalues (reduced-expr acc2) (anf-multiarg-expr stx acc))
  (anf-k-reduced-expr k reduced-expr acc2))

;; Deal with a reduced expr that is not simple
;; anf-k-reduced-expr : KontStx ExprStx [Listof StmtStx] -> [Listof StmtStx]
(def (anf-k-reduced-expr k expr acc)
  (syntax-case k (return)
    ((return)
     (with-syntax ((t (identifier-fresh #'tmp))
                   (e expr))
       (set-has-type! #'t (get-has-type #'e))
       (cons* #'(return t) #'(def t e) acc)))
    ((x ...) (anf-kontinue-expr k expr acc))))

;; Case where the expr constructor returns unit / is actually a statement in next phase
;; anf-k-multiarg-expr : KontStx ExprStx [Listof StmtStx] -> [Listof StmtStx]
(def (anf-k-multiarg-stmt k stx acc)
  (defvalues (stmt acc2) (anf-multiarg-expr stx acc))
  (anf-kontinue-stmt k (cons stmt acc2)))

;; anf-expr : KontStx ExprStx [Listof StmtStx] -> [Listof StmtStx]
(def (anf-expr k stx acc)
  (syntax-case stx (@make-interaction @tuple @list @record @dot block splice if switch λ == input require! assert! deposit! withdraw! digest sign @app)
    (x (trivial-expr? #'x) (anf-kontinue-expr k #'x acc))
    ((@make-interaction . _) (anf-make-interaction k stx acc))
    ((@tuple . _) (anf-k-multiarg-expr k stx acc))
    ((@list . _) (anf-k-multiarg-expr k stx acc))
    ((@record (x e) ...)
     (and (stx-andmap identifier? #'(x ...))
          (check-duplicate-identifiers #'(x ...)))
     (let-values (((rs acc2) (anf-arg-exprs (syntax->list #'(e ...)) acc)))
       (anf-k-reduced-expr k (retail-stx stx (map list (syntax->list #'(x ...)) rs)) acc2)))
    ((@dot x f)
     (let-values (((reduced-expr acc2) (anf-arg-expr #'x acc)))
       (anf-k-reduced-expr k (retail-stx stx [reduced-expr #'f]) acc2)))
    ((block b ...) (anf-body k #'(b ...) acc))
    ((splice b ...) (anf-body k #'(b ...) acc))
    ((switch e swcase ...)
     (let-values (((re acc2) (anf-arg-expr #'e acc)))
       (cons (restx stx (cons* (stx-car stx) re
                               (stx-map (cut anf-switch-case k <>) #'(swcase ...))))
             acc2)))
    ((λ . _) (anf-k-reduced-expr k (anf-lambda stx) acc))
    ((== a b) (anf-k-multiarg-expr k stx acc))
    ((input type tag)
     (let-values (((rtag acc2) (anf-arg-expr #'tag acc)))
       (anf-k-reduced-expr k (retail-stx stx [#'type rtag]) acc2)))
    ((require! . _) (anf-k-multiarg-stmt k stx acc))
    ((assert! . _) (anf-k-multiarg-stmt k stx acc))
    ((deposit! . _) (anf-k-deposit-withdraw k stx acc))
    ((withdraw! . _) (anf-k-deposit-withdraw k stx acc))
    ((digest . _) (anf-k-multiarg-expr k stx acc)) ;; TODO: make an explicit tuple?
    ((sign . _) (anf-k-multiarg-expr k stx acc))
    ((@app . _)
     (let-values (((reduced-expr acc2) (anf-multiarg-expr stx acc)))
       (anf-kontinue-expr k reduced-expr acc2)))))

(def (anf-k-deposit-withdraw k stx acc)
  (syntax-case stx (@record)
    ((_ lbl p (@record (x e) ...))
     (let-values (((rs acc2) (anf-arg-exprs (syntax->list #'(e ...)) acc)))
       (anf-kontinue-stmt k
         (cons
           (retail-stx stx [#'lbl #'p (cons '@record (stx-map list #'(x ...) rs))])
           acc2))))))

;; anf-body : KontStx StmtsStx [Listof StmtStx] -> [Listof StmtStx]
(def (anf-body k stx acc)
  (anf-stmts k (syntax->list stx) acc))

;; anf-standalone-body : KontStx StmtsStx -> [Listof StmtStx]
(def (anf-standalone-body k stx)
  (reverse (anf-body k stx [])))

;; anf-switch-case : SwitchCaseStx -> SwitchCaseStx
(def (anf-switch-case k stx)
  (syntax-case stx ()
    ((pat body ...)
     (retail-stx stx (anf-standalone-body k #'(body ...))))))

;; anf-lambda : LambdaStx -> LambdaStx
(def (anf-lambda stx)
  (syntax-case stx ()
    ((l params . body)
     (retail-stx stx [#'params (anf-standalone-body #'(return) #'body) ...]))))

;; anf-make-interaction : StmtStx -> [Listof StmtStx]
(def (anf-make-interaction k stx acc)
  (syntax-case stx ()
    ((_ ip lp . body)
     (anf-kontinue-expr k (retail-stx stx [#'ip #'lp (anf-standalone-body #'(return) #'body) ...])
                        acc))))

;; anf : ModuleStx UnusedTable TypeTable -> ModuleStx
(def (anf module unused-table tytbl)
  (parameterize ((current-unused-table unused-table)
                 (current-has-type-table tytbl)
                 (current-tmp (hash)))
    (syntax-case module (@module)
      ((@module stmts ...)
       (retail-stx module (reverse (anf-stmts #'(return) (syntax->list #'(stmts ...)) [])))))))

;;(trace! anf anf-stmts anf-stmt anf-expr anf-body anf-kontinue-expr anf-kontinue-stmt anf-k-multiarg-stmt anf-multiarg-expr anf-k-reduced-expr anf-arg-expr);;XXX DEBUG

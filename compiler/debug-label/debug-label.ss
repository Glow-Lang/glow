(export #t)

(import
  :gerbil/expander
  :std/iter
  :std/format
  :clan/pure/dict/symdict
  (for-template :mukn/glow/compiler/syntax-context)
  :mukn/glow/compiler/syntax-context
  :mukn/glow/compiler/common
  :mukn/glow/compiler/alpha-convert/fresh)

;; debug-label

;; Insert @debug-label statements with unique generated names
;; (unique from each other, not from program variables)

;; Preserve the original source location

;; debug-label : ModuleStx -> ModuleStx
(def (debug-label module)
  (parameterize ((current-unused-table (make-unused-table)))
    (syntax-case module (@module)
      ((@module stmts ...)
       (let ((stmts2 (debug-label-stmts (syntax->list #'(stmts ...)))))
         (retail-stx module stmts2))))))

;; debug-label-expr : ExprStx -> ExprStx
;; includes lambdas within "expressions" for the purpose of debug-labeling
(def (debug-label-expr stx)
  (define dle debug-label-expr)
  (syntax-case stx (@ ann @dot @dot/type @tuple @list @record if and or block splice == sign switch λ input digest require! assert! deposit! withdraw! @app @app-ctor)
    ((@ attr expr) (retail-stx stx [#'attr (dle #'expr)]))
    ((ann expr type) (retail-stx stx [(dle #'expr) #'type]))
    (x (identifier? #'x) #'x)
    (lit (stx-atomic-literal? #'lit) #'lit)
    ((@dot e x) (retail-stx stx [(dle #'e) #'x]))
    ((@dot/type t x) stx)
    ((@tuple e ...) (debug-label-keyword/sub-exprs stx))
    ((@list e ...) (debug-label-keyword/sub-exprs stx))
    ((@record (x e) ...)
     (retail-stx stx (stx-map (lambda (x e) [x (dle e)]) #'(x ...) #'(e ...))))
    ((block b ...)
     (retail-stx stx (debug-label-body (syntax->list #'(b ...)))))
    ((splice b ...)
     (retail-stx stx (debug-label-body (syntax->list #'(b ...)))))
    ((if c t e)  (retail-stx stx [(dle #'c) (dle #'t) (dle #'e)]))
    ((and e ...) (debug-label-keyword/sub-exprs stx))
    ((or e ...)  (debug-label-keyword/sub-exprs stx))
    ((== a b)    (debug-label-keyword/sub-exprs stx))
    ((sign e)    (debug-label-keyword/sub-exprs stx))
    ((switch e swcase ...)
     (retail-stx stx (cons (dle #'e) (stx-map dl-switch-case #'(swcase ...)))))
    ((λ . _) (dl-expr-function stx))
    ((input type tag) (retail-stx stx [#'type (dle #'tag)]))
    ((digest e ...) (debug-label-keyword/sub-exprs stx))
    ((require! e) (retail-stx stx [(dle #'e)]))
    ((assert! e) (retail-stx stx [(dle #'e)]))
    ((deposit! x e) (retail-stx stx [#'x (dle #'e)]))
    ((withdraw! x e) (retail-stx stx [#'x (dle #'e)]))
    ((@app-ctor f a ...) (debug-label-keyword/sub-exprs stx))
    ((@app f a ...) (debug-label-keyword/sub-exprs stx))
    ((f a ...) (restx stx (stx-map dle stx)))))

;; Used by special forms with a variable number of arguments that are regular expressions,
;; like @list, @tuple, and, or.
;; It could even be used by other keywords that have a constant number of arguments, but isn't.
;; debug-label-keyword/sub-exprs : StmtStx-> StmtStx
(def (debug-label-keyword/sub-exprs stx)
  (retail-stx stx (stx-map debug-label-expr (stx-cdr stx))))

;; debug-label-body : [Listof StmtStx] -> [Listof StmtStx]
(def (debug-label-body body)
  (match body
    ([] [['@debug-label (symbol-fresh 'dlb)]])
    ([e]
     (let ((dlb (symbol-fresh 'dlb)))
       [['@debug-label dlb] (debug-label-stmt e)]))
    ([stmt . rst]
     (let ((dlb (symbol-fresh 'dlb))
           (stmt2 (debug-label-stmt stmt)))
       (cons* ['@debug-label dlb] stmt2 (debug-label-body rst))))))

;; dl-expr-function : ExprStx -> ExprStx
(def (dl-expr-function stx)
  (syntax-case stx (:)
    ((l params : out-type body ...)
     (restx stx
            (cons* #'l #'params ': #'out-type
                   (debug-label-body (syntax->list #'(body ...))))))
    ((l params body ...)
     (restx stx
            (cons* #'l #'params
                   (debug-label-body (syntax->list #'(body ...))))))))

;; dl-switch-case : SwitchCaseStx -> SwitchCaseStx
(def (dl-switch-case stx)
  (syntax-case stx ()
    ((pat body ...)
     (restx stx
            (cons #'pat
                  (debug-label-body (syntax->list #'(body ...))))))))

;; debug-label-stmts : [Listof StmtStx] -> [Listof StmtStx]
(def (debug-label-stmts stmts)
  (let loop ((stmts stmts) (accstmts []))
    (match stmts
      ([] (reverse accstmts))
      ([stmt . rst]
       (let* ((dlb (symbol-fresh 'dlb))
              (stmt2 (debug-label-stmt stmt)))
         (loop rst (cons* stmt2 ['@debug-label dlb] accstmts)))))))

;; debug-label-stmt : StmtStx -> StmtStx
;; only inserts debug labels for statements nested *within* `stx`,
;; does not insert a label for `stx` itself
(def (debug-label-stmt stx)
  (syntax-case stx (@ interaction verifiably! publicly! @interaction @verifiably! @publicly! splice def deftype defdata publish! verify!)
    ((@ attr s) (retail-stx stx [#'attr (debug-label-stmt #'s)]))
    ((@interaction attr s) (retail-stx stx [#'attr (debug-label-stmt #'s)]))
    ((@verifiably! attr s) (retail-stx stx [#'attr (debug-label-stmt #'s)]))
    ((@publicly! attr s) (retail-stx stx [#'attr (debug-label-stmt #'s)]))
    ((splice s ...) (retail-stx stx (debug-label-stmts (syntax->list #'(s ...)))))
    ((deftype . _) stx)
    ((defdata . _) stx)
    ((def . _) (dl-stmt-def stx))
    ((publish! . _) stx)
    ((verify! . _) stx)
    (expr
     (debug-label-expr #'expr))))

;; dl-stmt-def : StmtStx -> StmtStx
(def (dl-stmt-def stx)
  (syntax-case stx (@ : quote def λ)
    ((d x : type expr) (identifier? #'x)
     (restx stx [#'d #'x ': #'type (debug-label-expr #'expr)]))
    ((d x expr) (identifier? #'x)
     (restx stx [#'d #'x (debug-label-expr #'expr)]))))

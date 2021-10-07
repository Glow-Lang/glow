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

(def pure-stmt list)

;; project : ModuleStx UnusedTable CheckpointInfoTable -> ModuleStx
;; Expect:
;;   exactly 1 (def id (@make-interacation ((@list participant ...)) . _))
;;   any number of non-def-interaction staments
(def (project prog unused cpit)
  (parameterize ((current-unused-table unused))
    (syntax-case prog (@module)
      ((@module begin-end stmt ...)
       (let ((stmts (syntax->list #'(stmt ...))))
         (retail-stx prog (cons #'begin-end (project-body stmts cpit #f))))))))

;; --------------------------------------------------------

;; project-expr : ExprStx CpiTable MPart -> ExprStx
(def (project-expr stx cpit this-p)
  (syntax-case stx (@make-interaction)
    ((@make-interaction . _) (project-make-interaction stx cpit this-p))
    (_ stx)))

;; project-make-interaction : ExprStx CpiTable MPart -> ExprStx
(def (project-make-interaction stx cpit this-p)
  (assert! (not this-p) "`@make-interaction` is only allowed in the consensus")
  (syntax-case stx (@make-interaction @record @list participants assets)
    ((@make-interaction
       ((@record (participants (@list p ...)) (assets (@list a ...))))
       params
       (from to)
       . body)
     (let ((ps (syntax->datum #'(p ...))) (bs (syntax->list #'body)))
       (retail-stx stx
         (cons*
           #'((@record (participants (@list p ...)) (assets (@list a ...))))
           #'params
           #'(from to)
           (cons #f (project-body bs cpit #f))
           (for/collect ((p ps))
             (cons p (project-body bs cpit p)))))))))

;; project-body : [StxListof StmtStx] CpiTable MPart -> [Listof StmtStx]
(def (project-body stmts cpit this-p)
  (flatten1 (stx-map (cut project-stmt <> cpit this-p) stmts)))

;; project-stmt : StmtStx CpiTable MPart -> [Listof StmtStx]
(def (project-stmt stx cpit this-p)
  (syntax-case stx (@ @label @debug-label deftype defdata publish! def return ignore! switch require! assert! deposit! withdraw!)
    ((@label x) (pure-stmt stx))
    ((@debug-label x) (pure-stmt stx))
    ((@ p s) (identifier? #'p)
     (append (project-set-participant #'p cpit this-p)
             (cond ((eq? (stx-e #'p) this-p) (pure-stmt #'s))
                   (else []))))
    ((publish! . _) (project-publish stx cpit this-p))
    ((deposit! . _) (project-deposit stx cpit this-p))
    ((withdraw! _ p e) (project-withdraw stx cpit this-p))
    ((require! v) (project-require stx cpit this-p))
    ((assert! v) (project-assert stx cpit this-p))
    ((switch c cases ...) (project-switch stx cpit this-p))
    ((def v e) (retail-stx stx [#'v (project-expr #'e cpit this-p)]))
    ((ignore! e) (pure-stmt stx))
    ((return e) (pure-stmt stx))
    ((defdata . _) (pure-stmt stx))
    ((deftype . _) (pure-stmt stx))))

;; TODO: if the active participant is already statically known to be `p`,
;;       don't need to produce anything, or, insert set-participant in checkpointify instead of project
(def (project-set-participant p cpit this-p)
  (cond
    (this-p [[#'participant:set-participant p]])
    (else   [[#'consensus:set-participant p]])))

;; project-publish : StmtStx CpiTable MPart -> [Listof StmtStx]
(def (project-publish stx cpit this-p)
  (syntax-case stx ()
    ((_ p x)
     (append (project-set-participant #'p cpit this-p)
             (cond
               ((eq? (stx-e #'p) this-p) [#'(add-to-publish 'x x)])
               (else [#'(def x (expect-published 'x))]))))))

;; project-deposit : StmtStx CpiTable MPart -> [Listof StmtStx]
(def (project-deposit stx cpit this-p)
  (syntax-case stx ()
    ((_ _ p n)
     (append (project-set-participant #'p cpit this-p)
             (cond
               ((eq? (stx-e #'p) this-p) [#'(add-to-deposit n)])
               (else [#'(expect-deposited n)]))))))

;; project-withdraw : StmtStx CpiTable MPart -> [Listof StmtStx]
(def (project-withdraw stx cpit this-p)
  (syntax-case stx ()
    ((_ _ p n)
     (cond
       (this-p [#'(participant:withdraw p n)])
       (else [#'(consensus:withdraw p n)])))))

;; project-require : StmtStx CpiTable MPart -> [Listof StmtStx]
(def (project-require stx cpit this-p)
  (syntax-case stx (require!)
    ;; TODO: use exceptions that enable the backtracking on failure
    ;; TODO: provide compile-time srclocs for runtime handler
    ;; TODO: add participant and project-set-participant
    ((require! x) [stx])))

;; project-assert : StmtStx CpiTable MPart -> [Listof StmtStx]
(def (project-assert stx cpit this-p)
  (syntax-case stx (assert!)
    ;; TODO: use exceptions that enable the backtracking on failure
    ;; TODO: provide compile-time srclocs for runtime handler
    ((assert! x) [stx])))

;; project-switch : StmtStx CpiTable MPart -> [Listof StmtStx]
(def (project-switch stx cpit this-p)
  (syntax-case stx ()
    ((switch c cases ...)
     [(restx1 stx
              (cons* 'switch #'c
                     (stx-map (cut project-switch-case <> cpit this-p) #'(cases ...))))])))

;; project-switch-case : SwcaseStx CpiTable MPart -> [Listof StmtStx]
(def (project-switch-case stx cpit this-p)
  (syntax-case stx ()
    ((pat body ...)
     (restx stx
            (cons #'pat
                  (project-body #'(body ...) cpit this-p))))))

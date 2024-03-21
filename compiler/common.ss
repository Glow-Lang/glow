(export #t)

(import
  :gerbil/expander
  (only-in :std/format format fprintf)
  (only-in :std/iter for for/collect for/fold)
  (only-in :std/misc/list length=n?)
  (only-in :std/misc/hash hash->list/sort hash-ref/default)
  (only-in :std/misc/symbol symbol<?)
  (only-in :clan/exception with-logged-exceptions)
  :mukn/glow/compiler/syntax-context ;; import the syntax context
  (for-template :mukn/glow/compiler/syntax-context))

;; hash-kref : [Hashof K V] K [-> V] -> V
;; I wish the normal `hash-ref` had better error messages
;; But this should not be used for looking up confidential information,
;; or looking up in hashes that contain confidential information, because
;; it prints the hash contents on error.
(def (hash-kref h k (d (hash-kref-error h k)))
  (hash-ref/default h k d))

(def ((hash-kref-error h k))
  (with-logged-exceptions ()
    (def msg (format "hash-kref Unbound key: ~r not found in ~r\n(hash-kref ~r ~r)\n" k (hash-keys h) h k))
    ;(eprintf "~a" msg)
    ;(display-continuation-backtrace kont (current-error-port))
    (error msg h k)))

;; transpose : [Listof [Listof A]] -> [Listof [Listof A]]
;; Like transposing a matrix, n-ary cartesian product, or n-ary zip,
;; ((a1 a2 a3) (b1 b2 b3)) <-> ((a1 b1) (a2 b2) (a3 b3))
(def (transpose lol)
  (when (null? lol) (error 'transpose "can't transpose empty lists"))
  (apply map list lol))

;; transpose/nlist : {n : Nat} [Listof [NListof n A]] -> [NListof n [Listof A]]
(def (transpose/nlist n lonl)
  (if (null? lonl) (make-list n []) (transpose lonl)))

;; format-symbol : FormatString Any ... -> Symbol
;; Like format, but produces a symbol
(def (format-symbol fmt . vs) (string->symbol (apply format fmt vs)))

;; stx-atomic-literal? : Any -> Bool
(def (stx-atomic-literal? stx)
  (def e (stx-e stx))
  (or (integer? e) (string? e) (u8vector? e) (boolean? e) (stx-unit? e)))

(def (stx-unit? stx)
  (syntax-case stx (@tuple)
    ((@tuple) #t)
    (_ #f)))

;; type TrivialExprStx = IdentifierStx | AtomicLiteralStx
;; trivial-expr? : ExprStx -> Bool
;; is this expression trivial enough to be used in a call?
;; this implies evaluating it has no side-effect whatsoever and can commute with anything.
(def (trivial-expr? expr)
  (or (identifier? expr) (stx-atomic-literal? expr)))

;; stx-leaf? : Any -> Bool
(def (stx-leaf? v) (or (identifier? v) (stx-atomic-literal? v)))

;; head-id : Stx -> (U Id #f)
(def (head-id stx)
  (cond ((identifier? stx) stx)
        ((stx-pair? stx) (head-id (stx-car stx)))
        (else #f)))

;; restx1 : Stx Any -> Stx
;; restore the lexical-context and source-location from stx onto e
;; TODO: also preserve any "syntax properties" such as type, visibility, etc.
(def (restx1 stx e)
  (datum->syntax (head-id stx) e (stx-source stx)))

;; restx : Stx Any -> Stx
;; multiple levels for a nested list / tree
(def (restx stx e)
  (cond ((and (list? e) (stx-list? stx) (= (length e) (stx-length stx)))
         (restx1 stx (stx-map restx stx e)))
        ((and (pair? e) (stx-pair? stx))
         (let ((p (cons (restx (stx-car stx) (car e))
                        (restx (stx-cdr stx) (cdr e)))))
           (if (pair? stx) p (restx1 stx p))))
        (else
         (restx1 stx e))))

;; stx-sexpr=? : Stx Stx -> Bool
(def (stx-sexpr=? a b) (equal? (syntax->datum a) (syntax->datum b)))

;; stx-sexpr=?/recur : Stx Stx [Stx Stx -> Bool] -> Bool
;; The rec=? function is only called on sub-pieces of the syntax,
;; so it can be used for recursion based on decreasing size
(def (stx-sexpr=?/recur a b rec=?)
  (cond
    ((or (stx-list? a) (stx-list? b))
     (and (stx-list? a) (stx-list? b) (= (stx-length a) (stx-length b))
          (andmap rec=? (syntax->list a) (syntax->list b))))
    ((or (stx-pair? a) (stx-pair? b))
     (and (stx-pair? a) (stx-pair? b)
          (rec=? (stx-car a) (stx-car b))
          (let ((a2 (stx-cdr a)) (b2 (stx-cdr b)))
            (cond ((and (stx-pair? a2) (stx-pair? b2))
                   (stx-sexpr=?/recur a2 b2 rec=?))
                  (else
                   (rec=? a2 b2))))))
    ((or (stx-leaf? a) (stx-leaf? b))
     (stx-sexpr=? a b))
    (else (error 'stx-sexpr=?/recur "unknown syntax structure:" a b))))

;; stx-shallow-source=? : Stx Stx -> Bool
;; only cares about source locations at the very top
(def (stx-shallow-source=? a b) (equal? (stx-source a) (stx-source b)))

;; stx-deep-source=? : Stx Stx -> Bool
;; only cares about source locations and structure of lists/containers
;; does not care about leaves such as symbols or literals
(def (stx-deep-source=? a b)
  (and (stx-shallow-source=? a b)
       (cond
         ((or (stx-leaf? a) (stx-leaf? b))
          (and (stx-leaf? a) (stx-leaf? b)))
         (else
          (stx-sexpr=?/recur a b stx-deep-source=?)))))

;; on-substx : Stx [Stx -> Any] -> Void
(def (on-substx stx f)
  (cond
    ((stx-list? stx) (stx-for-each f stx))
    ((stx-pair? stx) (f (stx-car stx)) (f (stx-cdr stx)) (void))
    ((stx-leaf? stx) (void))
    (else (error "on-substx: unknown syntax structure" (syntax->datum stx)))))

;; TODO: do we need to properly parameterize the context (?)
(def (read-sexp-module file)
  ; (first-and-only (read-syntax-from-file file)))
  (match (read-syntax-from-file file)
    ([stx] stx)
    (stxs
     (error 'read-sexp-module "expected exactly 1 sexp, given" (length stxs) "in" file))))

;; NB: this is lossy of source location and identifier resolution
(def (write-sexp-module module (port (current-output-port)))
  (fprintf port "~y" (syntax->datum module)))

(def (write-syntax-list stxs (port (current-output-port)))
  (for ((s (syntax->datum stxs))) (fprintf port "~y" s)))

;; Splice multiple statements if needed, otherwise just include a single one
;; spice-stmts : Stx [listof StmtStx] -> StmtStx
(def (splice-stmts stx stmts)
  (if (length=n? stmts 1) (car stmts) (restx stx (cons #'splice stmts))))

;; Recursively splice out any splice statement into the list of statements, in reverse
;; spice-stmts : [listof StmtStx] [listof StmtStx] -> [listof StmtStx]
(def (reverse-unsplice-stmts stmts (acc []))
  (for/fold (acc acc) ((stmt stmts))
    (syntax-case stmt (splice)
      ((splice . body) (reverse-unsplice-stmts (syntax->list #'body) acc))
      (_ (cons stmt acc)))))

;; Recursively splice out any splice statement into the list of statements, in reverse
;; spice-stmts : [listof StmtStx] -> [listof StmtStx]
(def (unsplice-stmts stmts)
  (reverse (reverse-unsplice-stmts stmts [])))


;; at-stx : MaybeParticipant StmtStx -> StmtStx
(def (at-stx participant stx)
  (if participant
    (restx stx [#'@ participant stx])
    stx))

;; retail-stx : Stx [listof Stx] -> Stx
(def (retail-stx stx tail)
  (restx stx [(stx-car stx) . tail]))

;; retail-stx1 : Stx [listof Stx] -> Stx
(def (retail-stx1 stx tail)
  (restx1 stx [(stx-car stx) . tail]))

;; Given the left-hand-side of a definition (def foo expr) or (def (foo args ...) expr),
;; extract the identifier foo.
;; definition-lhs->id : Stx -> Identifier
(def (definition-lhs->id stx)
  (syntax-case stx ()
    ((id . _) (identifier? #'id) #'id)
    (id (identifier? #'id) #'id)))

;; symbol->repr-sexpr : Symbol -> Sexpr
;; repr-sexpr->symbol : Sexpr -> Symbol
(def (symbol->repr-sexpr s) `',s)
(def (repr-sexpr->symbol s) (match s (['quote x] x)))

;; list->repr-sexpr : [Listof V] [V -> Sexpr] -> Sexpr
;; repr-sexpr->list : Sexpr [Sexpr -> V] -> [Listof V]
(def (list->repr-sexpr l v->s) `(@list ,@(map v->s l)))
(def (repr-sexpr->list s s->v)
  (match s
    ((cons '@list l) (map s->v l))
    (_ (error 'repr-sexpr->list "expected `@list`"))))

;; hash->repr-sexpr : [K -> Sexpr] [V -> Sexpr] [K K -> Bool] -> [[Hashof K V]  -> Sexpr]
;; repr-sexpr->hash : [Sexpr -> K] [Sexpr -> V] -> [Sexpr -> [Hashof K V]]
(def ((hash->repr-sexpr k->s v->s k<?) h)
  (cons 'hash
        (for/collect ((p (hash->list/sort h k<?)))
          (with (([k . v] p))
            [(k->s k) (v->s v)]))))

(def ((repr-sexpr->hash s->k s->v) s)
  (match s
    ((cons 'hash ents)
     (def h (make-hash-table))
     (for ((ent ents))
       (with (([ks vs] ent))
         (def k (s->k ks))
         (def v (s->v vs))
         (hash-put! h k v)))
     h)
    (_ (error 'repr-sexpr->hash "expected `hash`"))))

(defrules cond/compare (else)
  ((_ (a b) (else body)) body)
  ((_ (a b) (t? body) . others)
   (cond ((and (t? a) (t? b)) body) ((t? a) #t) ((t? b) #f)
         (else (cond/compare (a b) . others)))))

;; symbols < bools < real-numbers < strings < null < pairs < others
(def (sexpr<? a b)
  (cond/compare
   (a b)
   (symbol? (symbol<? a b))
   (boolean? (and (not a) b))
   (real? (< a b))
   (string? (string<? a b))
   (null? #f)
   (pair?
    (or (sexpr<? (car a) (car b))
        (and (equal? (car a) (car b))
             (sexpr<? (cdr a) (cdr b)))))
   (else (string<? (format "~s" a) (format "~s" b)))))

;; TODO: move this to std/misc/repr ?
#|
(defmethod {:pr AST}
  (λ (object (port (current-output-port)) (options (current-representation-options)))
    (def (d x) (display x port))
    (def (w x) (write x port))
    (d "(begin0 #") (d (object->serial-number object)) (d " #'") (w (syntax->datum object)) (d ")"))
  rebind: #t) ;; make this idempotent
|#

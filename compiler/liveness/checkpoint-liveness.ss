
(export checkpoint-liveness
        read-checkpoint-liveness-table
        write-checkpoint-liveness-table
        checkpoint-liveness-table=?)

(import :std/iter
        :std/format
        :std/misc/list
        :std/misc/repr
        :std/misc/hash
        <expander-runtime>
        :clan/base
        :mukn/glow/compiler/common
        :mukn/glow/compiler/checkpointify/checkpointify)

;; A CheckpointLivenessTable is a [Hashof CheckpointSymbol CheckpointLiveness]
;; A CheckpointLiveness is a [Listof Symbol]
;; mapping each checkpoint to the set of variables that are
;; potentially live there by consensus-access

;; --------------------------------------------------------
;; Checkpoint Liveness from Checkpoint and Transition Info

;; checkpoint-liveness : CheckpointInfoTable -> (values CheckpointInfoTable CheckpointLivenessTable)
(def (checkpoint-liveness cpit)
  (def cps (hash-keys cpit))
  (def ends (filter (compose null? ci-outgoing-transitions (cut hash-ref cpit <>)) cps))
  (unless (pair? ends) (error 'checkpoint-liveness "no ending points"))
  (def cplt (make-hash-table-eq))
  (for-each (cut hash-put! cplt <> []) ends)
  ;; ats: already have checkpoint-liveness, not analyzed incoming-transitions yet
  ;; seen: already seen incoming-transitions as well as checkpoint-liveness
  (let loop ((ats ends) (seen []))
    (match ats
      ([] (void))
      ([at . rst]
       (def cpi (hash-ref cpit at))
       (def cpl (hash-ref cplt at))
       (def intis (ci-incoming-transitions cpi))
       (def incps (map ti-from intis))
       (for ((inti intis))
         (def ds (ti-variables-publicly-introduced inti))
         (def us (ti-variables-publicly-used inti))
         (def ls (remove* ds (append-nonmem us cpl)))
         ;; TODO add variable-eliminated too:
         ;; (def elim (remove* cpl (ti-variables-publicly-used inti)))
         (hash-update! cplt (ti-from inti) (cut append-nodup ls <>) []))
       (def seen2 (cons at seen))
       (def nexts (remove* seen2 incps))
       (loop (append nexts rst) seen2))))
  (for ((k (hash-keys cpit)))
    (def cpi (hash-ref cpit k))
    (def ls (hash-ref cplt k))
    (ci-variables-live-set! cpi ls)
    (for ((outti (ci-outgoing-transitions cpi)))
      (def next-ls (hash-ref cplt (ti-to outti)))
      (def elims (remove* next-ls ls))
      (ti-variables-eliminated-set! outti (map (cut cons <> #f) elims))))
  (values cpit cplt))

;; --------------------------------------------------------
;; Read, Write, and Equal for CheckpointLivenessTable

;; read-checkpoint-liveness-table : PathString -> CheckpointLivenessTable
(def (read-checkpoint-liveness-table file)
  (repr-sexpr->checkpoint-liveness-table (call-with-input-file file read)))

;; write-checkpoint-liveness-table : CheckpointLivenessTable OutputPort -> Void
(def (write-checkpoint-liveness-table cpit out)
  (fprintf out "~y" (checkpoint-liveness-table->repr-sexpr cpit)))

;; checkpoint-liveness-table=? : CheckpointLivenessTable CheckpointLivenessTable -> Boolean
(def (checkpoint-liveness-table=? a b)
  (equal? (checkpoint-liveness-table->repr-sexpr a)
          (checkpoint-liveness-table->repr-sexpr b)))

;; checkpoint-liveness->repr-sexpr : CheckpointLiveness -> Sexpr
(def (checkpoint-liveness->repr-sexpr cl)
  (list->repr-sexpr cl symbol->repr-sexpr))

;; repr-sexpr->checkpoint-liveness : Sexpr -> CheckpointLiveness
(def (repr-sexpr->checkpoint-liveness s)
  (repr-sexpr->list s repr-sexpr->symbol))

;; checkpoint-liveness-table->repr-sexpr : CheckpointLivenessTable -> Sexpr
(def checkpoint-liveness-table->repr-sexpr
  (hash->repr-sexpr identity checkpoint-liveness->repr-sexpr symbol<?))

;; repr-sexpr->checkpoint-liveness-table : Sexpr -> CheckpointLivenessTable
(def repr-sexpr->checkpoint-liveness-table
  (repr-sexpr->hash identity repr-sexpr->checkpoint-liveness))

;; --------------------------------------------------------

;; remove every n from ps
(def (remove* ns ps)
  (filter (lambda (p) (not (member p ns))) ps))

(def (append-nonmem2 new old)
  (append (remove* old new) old))

(def (append-nonmem . args)
  (foldr append-nonmem2 [] args))

(def (append-nodup . args)
  (unique (apply append args)))

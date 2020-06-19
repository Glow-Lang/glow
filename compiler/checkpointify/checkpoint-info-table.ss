
(export read-checkpoint-info-table
        write-checkpoint-info-table
        checkpoint-info-table=?)

(import :std/iter
        :std/format
        :std/misc/repr
        :glow/compiler/common
        :glow/compiler/checkpointify/checkpointify)

;; type CheckpointInfoTable
;; from :glow/compiler/checkpointify/checkpointify

;; type CheckpointInfo
;; CI from :glow/compiler/checkpointify/checkpointify

;; type TransitionInfo
;; TI from :glow/compiler/checkpointify/checkpointify

;; read-checkpoint-info-table : InputPort -> CheckpointInfoTable
(def (read-checkpoint-info-table in)
  (repr-sexpr->checkpoint-info-table (read in)))

;; write-checkpoint-info-table : CheckpointInfoTable OutputPort -> Void
(def (write-checkpoint-info-table cpit out)
  (prn cpit out))

;; checkpoint-info-table=? : CheckpointInfoTable CheckpointInfoTable -> Boolean
(def (checkpoint-info-table=? a b)
  (equal? a b))


;; repr-sexpr->checkpoint-info-table : Sexpr -> CheckpointInfoTable
(def (repr-sexpr->checkpoint-info-table s)
  (match s
    ((cons 'hash entries)
     (def h (make-hash-table))
     (for ((e entries))
       (with (([k v] e))
         (hash-put! h (repr-sexpr->symbol k) (repr-sexpr->checkpoint-info v))))
     h)
    (_ (error 'checkpoint-info-table "expected `hash`"))))

;; repr-sexpr->checkpoint-info : Sexpr -> CheckpointInfo
(def (repr-sexpr->checkpoint-info s)
  (match s
    (['ci checkpoint variables-live incoming-transitions outgoing-transitions]
     (ci (repr-sexpr->symbol checkpoint)
         (repr-sexpr->list variables-live repr-sexpr->symbol)
         (repr-sexpr->list incoming-transitions repr-sexpr->transition-info)
         (repr-sexpr->list outgoing-transitions repr-sexpr->transition-info)))
    (_ (error 'checkpoint-info "expected `ci`"))))

;; repr-sexpr->transition-info : Sexpr -> TransitionInfo
(def (repr-sexpr->transition-info s)
  (match s
    (['ti from to participant effects variables-introduced variables-used variables-eliminated]
     (ti (repr-sexpr->symbol from)
         (and to (repr-sexpr->symbol to))
         (and participant (repr-sexpr->symbol participant))
         effects
         (repr-sexpr->list variables-introduced repr-sexpr->symbol)
         (repr-sexpr->list variables-used repr-sexpr->symbol)
         (repr-sexpr->list variables-eliminated repr-sexpr->symbol)))
    (_ (error 'transition-info "expected `ti`"))))

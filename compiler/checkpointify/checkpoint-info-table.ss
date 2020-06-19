
(export read-checkpoint-info-table
        write-checkpoint-info-table
        checkpoint-info-table=?)

(import :std/format
        :std/misc/repr)

;; type CheckpointInfoTable
;; from :glow/compiler/checkpointify/checkpointify

;; read-checkpoint-info-table : InputPort -> CheckpointInfoTable
(def (read-checkpoint-info-table in)
  (make-hash-table))

;; write-checkpoint-info-table : CheckpointInfoTable OutputPort -> Void
(def (write-checkpoint-info-table cpit out)
  (prn cpit out))

;; checkpoint-info-table=? : CheckpointInfoTable CheckpointInfoTable -> Boolean
(def (checkpoint-info-table=? a b)
  (equal? a b))

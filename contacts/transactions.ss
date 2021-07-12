;;;; Glow transactions.

(export check-txid! start-transaction transaction-output)

(import
 :gerbil/gambit/ports ; process-status
 :gerbil/gambit/threads ; spawn
 :std/db/dbi ; SQL
 :std/format ; format
 :std/misc/hash ; hash-ref-set!
 :std/misc/ports ; read-all-as-lines
 :std/misc/process ; run-process
 :std/srfi/1 ; first
 :std/sugar ; let-hash
 :mukn/glow-contacts/contacts)

(def (check-txid! txid)
  (assert! (> txid 0) "Invalid transaction ID"))

(def (make-command action args)
  (match action
    ("export-identities"
     '("glow" "export-identities"))
    ("faucet"
     ;; Faucet does not currently support an amount, but probably should.
     (let (;(amount (hash-ref args 'amount))
           (source-network (hash-ref (hash-ref args 'source) 'network))
           (to (hash-ref (hash-ref args 'source) 'address)))
       `("glow" "faucet"
         "--evm-network" ,source-network
         ;"--value" ,amount
         "--to" ,to)))
    ("generate-identity"
     (let ((nickname (hash-ref args 'nickname)))
       `("glow" "generate-identity" "-J" "-N" ,nickname)))
    ("list-identities"
     '("glow" "list-identities"))
    ("transfer-from"
     (set! (values (hash-ref args 'source) (hash-ref args 'dest))
           (values (hash-ref args 'dest) (hash-ref args 'source)))
     (make-command "transfer-to" args))
    ("transfer-to"
     (let ((amount (hash-ref args 'amount))
           (source-network (hash-ref (hash-ref args 'source) 'network))
           (dest-network (hash-ref (hash-ref args 'dest) 'network))
           (from (hash-ref (hash-ref args 'source) 'address))
           (to (hash-ref (hash-ref args 'dest) 'address)))
       (unless (string=? source-network dest-network)
         (error (format "Can't transfer across networks ~a/~a" source-network dest-network)))
       `("glow" "transfer"
         "--evm-network" ,source-network
         "--value" ,amount
         "--from" ,from
         "--to" ,to)))
    (else
     (error (format "Unsupported action ~a" action)))))

(def (read-from-process process
                        (update (lambda (output) (display output)))
                        (finalize (lambda (status) status))
                        (interval 0.01)
                        (max-poll 1000))
  (input-port-timeout-set! process -inf.0)
  (let poll ((count 0)
             (status #f))
    (if (>= count max-poll)
        (finalize -1) ; timeout
        (begin
          (update (read-all-as-string process))
          (set! status (process-status process 0 #f))
          (if (not status)
              (begin (thread-sleep! interval) (poll (1+ count) status))
              (finalize (/ status 256)))))))

(def (spawn-command txid command)
  (check-txid! txid)
  (def db (ensure-contact-db!))
  (def (update-transaction output)
    (sql-eval db "UPDATE txnlog SET output = output || $1 WHERE txid=$2" output txid))
  (def (finish-transaction status)
    (sql-eval db "UPDATE txnlog SET status = $1 WHERE txid=$2" status txid))
  (spawn run-process command
         coprocess: (cut read-from-process <> update-transaction finish-transaction)
         stderr-redirection: #t))

(def (start-transaction action args)
  (let* ((command (make-command action args))
         (txid
          ;; This currently depends on the SQLite-specific `rowid` pseudo-column.
          (first (sql-eval-query (ensure-contact-db!)
                                 "INSERT INTO txnlog (command) VALUES ($1) RETURNING rowid"
                                 (string-join command " ")))))
    (spawn-command txid command)
    txid))

(def (transaction-output txid)
  (let ((txns (sql-eval-query (ensure-contact-db!)
                              "SELECT output, status FROM txnlog WHERE txid=$1"
                              txid)))
    (if (null? txns)
        (error "No such transaction")
        (apply values (vector->list (first txns))))))

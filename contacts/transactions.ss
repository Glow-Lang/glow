;;;; Glow transactions.

(export check-txid! start-transaction transaction-output)

(import
 :clan/ffi
 :gerbil/gambit/ports
 :gerbil/gambit/threads
 :std/assert
 :std/db/dbi
 :std/format
 :std/misc/hash
 :std/misc/ports
 :std/misc/process
 :std/sort
 (only-in :std/srfi/1 first)
 :std/sugar
 :std/text/json
 :mukn/glow/contacts/db)

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
    ("run-dapp"
     (let* ((dapp (hash-ref args 'dapp))
            (assets (hash-ref args 'assets))
            (my-role (hash-ref args 'my_role))
            (participants (hash-ref args 'participants))
            (my-identity (hash-ref participants (string->symbol my-role)))
            (params (hash-ref args 'params))
            (tcp-options (if (string=? my-role
                                       (first (sort (map symbol->string
                                                         (hash-keys participants))
                                                    string<?)))
                             "{\"listen\": 10337, \"connect\": \"localhost:10338\"}"
                             "{\"connect\": \"localhost:10337\", \"listen\": 10338}"))
            (input (hash-ref args 'input)))
       (when (string? input)
         (setenv "INPUT" input))
       `("glow" "start-interaction"
         "--max-initial-block" "%10000"
         "--timeout-in-blocks" "1000"
         "--glow-app" ,dapp
         "--role" ,my-role
         "--database" ,my-role
         "--evm-network" ,(hash-ref my-identity 'network)
         "--my-identity" ,(hash-ref my-identity 'nickname)
         "--tcp" ,tcp-options
         "--assets" ,(json-object->string assets)
         "--participants" ,(json-object->string
                            (list->hash-table
                             (hash-map
                              (lambda (k v) (cons k (hash-ref v 'address)))
                              participants)))
         "--params" ,(json-object->string params))))
    (else
     (error (format "Unsupported action ~a" action)))))

(def (read-from-process process
                        (update (lambda (output) (display output)))
                        (finalize (lambda (status) status))
                        (interval 0.1)
                        (max-poll 600))
  (input-port-timeout-set! process -inf.0)
  (let poll ((count 0)
             (status #f))
    (if (>= count max-poll)
        (begin
          (kill (process-pid process))
          (finalize -1)) ; timeout
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

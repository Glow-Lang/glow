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
 :std/text/json ; JSON
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
    ((or "buy-sig" "sell-sig")
     (let* ((amount (string->number (hash-ref args 'amount)))
            (price (format "0x~a" (number->string amount 16))) ; ~x 0-pads
            (digest (hash-ref args 'digest))
            (role (match action
                    ("buy-sig" "Buyer")
                    ("sell-sig" "Seller")))
            (buy/sell-source (match action
                               ("buy-sig" 'source)
                               ("sell-sig" 'dest)))
            (buy/sell-dest (match action
                               ("buy-sig" 'dest)
                               ("sell-sig" 'source)))
            (nickname (hash-ref (hash-ref args 'source) 'nickname))
            (source-network (hash-ref (hash-ref args 'source) 'network))
            (dest-network (hash-ref (hash-ref args 'dest) 'network))
            (buyer-addr (hash-ref (hash-ref args buy/sell-source) 'address))
            (seller-addr (hash-ref (hash-ref args buy/sell-dest) 'address))
            (participants (hash (Buyer buyer-addr) (Seller seller-addr)))
            (parameters (hash (digest digest) (price price)))
            (handshake (match action ; FIXME
                         ("buy-sig" "nc -l -p 3141")
                         ("sell-sig" "nc localhost 3141"))))
       (unless (string=? source-network dest-network)
         (error (format "Can't buy signature across networks ~a/~a" source-network dest-network)))
       `("glow" "start-interaction"
         "--glow-app" "buy_sig"
         "--role" ,role
         "--my-identity" ,nickname
         "--database" ,nickname
         "--evm-network" ,source-network
         "-B" "3000000" ; FIXME
         "-T" "1000" ; FIXME
         "--handshake" ,handshake
         "--participants" ,(json-object->string participants)
         "--params" ,(json-object->string parameters))))
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

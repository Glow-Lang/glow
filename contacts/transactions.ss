;;;; Glow transactions.

(export check-txid! start-transaction transaction-output)

(import
 :clan/ffi
 :gerbil/gambit/ports
 :gerbil/gambit/threads
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
                         ("buy-sig" "nc -l 3141")
                         ("sell-sig" "nc localhost 3141"))))
       (unless (string=? source-network dest-network)
         (error (format "Can't buy signature across networks ~a/~a" source-network dest-network)))
       `("glow" "start-interaction"
         "--max-initial-block" "%10000"
         "--timeout-in-blocks" "1000"
         "--glow-app" "buy_sig"
         "--role" ,role
         "--my-identity" ,nickname
         "--database" ,nickname
         "--evm-network" ,source-network
         "--handshake" ,handshake
         "--assets" "{\"DefaultToken\": \"PET\"}" ; FIXME
         "--participants" ,(json-object->string participants)
         "--params" ,(json-object->string parameters))))
    ((or "rps-A" "rps-B")
     (let* ((amount (string->number (hash-ref args 'amount)))
            (price (format "0x~a" (number->string amount 16))) ; ~x 0-pads
            (hand (hash-ref args 'hand))
            (role (match action
                    ("rps-A" "A")
                    ("rps-B" "B")))
            (a (hash-ref args 'a))
            (b (hash-ref args 'b))
            (nickname (hash-ref (match action ("rps-A" a) ("rps-B" b)) 'nickname))
            (a-network (hash-ref (hash-ref args 'a) 'network))
            (b-network (hash-ref (hash-ref args 'b) 'network))
            (a-addr (hash-ref (hash-ref args 'a) 'address))
            (b-addr (hash-ref (hash-ref args 'b) 'address))
            (participants (hash (A a-addr) (B b-addr)))
            (parameters (hash (wagerAmount price)))
            (handshake (match action ; FIXME
                         ("rps-A" "nc -l 3141")
                         ("rps-B" "nc localhost 3141"))))
       (unless (string=? a-network b-network)
         (error (format "Can't play across networks ~a/~a" a-network b-network)))
       (when (string? hand)
         (setenv "INPUT" hand))
       `("glow" "start-interaction"
         "--max-initial-block" "%10000"
         "--timeout-in-blocks" "1000"
         "--glow-app" "rps_simple"
         "--role" ,role
         "--my-identity" ,nickname
         "--database" ,nickname
         "--evm-network" ,a-network
         "--handshake" ,handshake
         "--assets" "{\"DefaultToken\": \"PET\"}" ; FIXME
         "--participants" ,(json-object->string participants)
         "--params" ,(json-object->string parameters))))
    ((or "swap-A" "swap-B")
     (let* ((t-amount (string->number (hash-ref args 't_amount)))
            (t-price (format "0x~a" (number->string t-amount 16)))
            (t-token (hash-ref args 't_token))
            (u-amount (string->number (hash-ref args 'u_amount)))
            (u-price (format "0x~a" (number->string u-amount 16)))
            (u-token (hash-ref args 'u_token))
            (role (match action
                    ("swap-A" "A")
                    ("swap-B" "B")))
            (a (hash-ref args 'a))
            (b (hash-ref args 'b))
            (nickname (hash-ref (match action ("swap-A" a) ("swap-B" b)) 'nickname))
            (a-network (hash-ref (hash-ref args 'a) 'network))
            (b-network (hash-ref (hash-ref args 'b) 'network))
            (a-addr (hash-ref (hash-ref args 'a) 'address))
            (b-addr (hash-ref (hash-ref args 'b) 'address))
            (participants (hash (A a-addr) (B b-addr)))
            (parameters (hash (t t-price) (u u-price)))
            (assets (hash (T t-token) (U u-token)))
            (handshake (match action ; FIXME
                         ("swap-A" "nc -l 3141")
                         ("swap-B" "nc localhost 3141"))))
       (unless (string=? a-network b-network)
         (error (format "Can't swap across networks ~a/~a" a-network b-network)))
       `("glow" "start-interaction"
         "--max-initial-block" "%10000"
         "--timeout-in-blocks" "1000"
         "--glow-app" "asset_swap"
         "--role" ,role
         "--my-identity" ,nickname
         "--database" ,nickname
         "--evm-network" ,a-network
         "--handshake" ,handshake
         "--assets" ,(json-object->string assets)
         "--participants" ,(json-object->string participants)
         "--params" ,(json-object->string parameters))))
    ("run-dapp"
     (let* ((dapp (hash-ref args 'dapp))
            (assets (hash-ref args 'assets))
            (my-role (hash-ref args 'my_role))
            (participants (hash-ref args 'participants))
            (my-identity (hash-ref participants (string->symbol my-role)))
            (params (hash-ref args 'params))
            (handshake (if (string=? my-role
                                     (first (sort (map symbol->string
                                                       (hash-keys participants))
                                                  string<?)))
                           "nc -l 3141"
                           "nc localhost 3141"))
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
         "--handshake" ,handshake
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

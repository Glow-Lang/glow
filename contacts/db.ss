;;;; Glow contacts database routines.

(export contact-db ensure-contact-db! close-contact-db!
        list-networks list-contacts
        add-contact delete-contact delete-contact-by-name update-contact
        get-identities add-identity delete-identity-by-address delete-identity-by-nickname)

(import
 (only-in :clan/base vector->values)
 (only-in :clan/config xdg-config-home)
 :std/db/dbi
 (only-in :std/db/sqlite sqlite-open)
 (only-in :std/iter for for/collect)
 (only-in :std/misc/hash hash-ref-set!)
 (only-in :std/srfi/1 append-map first)
 :std/sugar
 :std/text/json)

;; The global contacts database connection handle.
(def contact-db #f)

;; The default path for the contacts database.
(def (default-contact-db-path)
  (xdg-config-home "glow" "db" "contacts.db"))

;; Open the global connection to the contacts database.
(def (ensure-contact-db! path: (path #f) reopen: (reopen? #f))
  (unless (and contact-db (not reopen?))
    (when contact-db (close-contact-db!))
    (set! contact-db (sqlite-open (or path (default-contact-db-path))))
    (sql-eval contact-db "PRAGMA foreign_keys=ON;")) ; SQLite-specific
  contact-db)

;; Close the global connection to the contacts database.
(def (close-contact-db!)
  (when contact-db (sql-close contact-db))
  (set! contact-db #f))

;; Convert the result of an SQL query evaluation to a hash table
;; keyed on symbolicated column names.
(def (hash<-sql-eval-query db query . args)
  (let* ((stmt (sql-prepare db query))
         (columns (map make-symbol (sql-columns stmt))))
    (when args (apply sql-bind stmt args))
    (map (lambda (row)
           (plist->hash-table (append-map list
                                          columns
                                          (for/collect ((value row))
                                            (match value
                                              (#f (void)) ; JSON null
                                              (else value))))))
         (sql-query stmt))))

;; Commands for editlog playback.
(def edit-commands (make-hash-table))

;; Insert an entry into the editlog.
(def (editlog command . args)
  (sql-eval (ensure-contact-db!)
            "INSERT INTO editlog (command, arguments) VALUES ($1, json($2))"
            (cond ((string? command) command)
                  ((symbol? command) (symbol->string command))
                  (else (error "Command must designate a string")))
            (with-output-to-string
              (lambda () (write-json args)))))

;; Define and register a function that automatically inserts
;; an entry into the editlog if its body succeeds.
(defrule (define-edit-command (name . params) body ...)
  (begin
    (def (name . params)
      (begin0
       (begin body ...)
       (editlog 'name . params)))
    (set! (hash-ref edit-commands 'name) name)))

;; Delete all contacts and identities.
(define-edit-command (delete-all-contacts)
  (sql-eval (ensure-contact-db!) "DELETE FROM contact"))

;; Delete contacts and play back (part of) the editlog.
;; TODO: Should be one transaction.
;; TODO: Start from last delete-all-contacts.
(def (playback ending-timestamp)
  (delete-all-contacts)
  (for ((log (sql-eval-query (ensure-contact-db!)
                             "SELECT command, arguments FROM editlog WHERE timestamp <= $1"
                             ending-timestamp)))
    (let-values (((command arguments) (vector->values log)))
      (set! command (string->symbol command))
      (set! arguments (string->json-object arguments))
      (def function (hash-ref edit-commands command))
      (apply function arguments))))

;; List the known networks and their metadata.
(def (list-networks)
  (hash<-sql-eval-query (ensure-contact-db!) "SELECT * FROM network"))

;; List a/the known contact/s.
(def (list-contacts (cid #f))
  (let ((contacts
         (if cid
             (hash<-sql-eval-query (ensure-contact-db!) "SELECT cid, name FROM contact WHERE cid=$1" cid)
             (hash<-sql-eval-query (ensure-contact-db!) "SELECT cid, name FROM contact ORDER BY name"))))
    (for/collect ((contact contacts))
      (let-hash contact
        (set! (hash-ref contact 'identities) (get-identities .cid)))
      contact)))

;; Insert a list of identities (hash tables) at once.
;; An internal helper function.
(def (insert-identities cid identities)
  (let ((stmt (sql-prepare (ensure-contact-db!) "INSERT INTO identity (cid, network, address, nickname, public_key, secret_key_path) VALUES ($1, $2, $3, $4, $5, $6)")))
    (for ((identity identities))
      (let-hash identity
        (sql-bind stmt cid .network .address .?nickname .?public_key .?secret_key_path))
      (sql-exec stmt))))

;; Add one identity to an existing contact.
(define-edit-command (add-identity cid identity)
  (insert-identities cid (list identity)))

;; Add a new contact with some identities.
(define-edit-command (add-contact name identities)
  (let ((db (ensure-contact-db!)))
    (try
      (sql-txn-begin db)
      ;; This INSERT currently depends on the SQLite-specific `rowid` pseudo-column.
      (let ((cid (first (sql-eval-query db "INSERT INTO contact (name) VALUES ($1) RETURNING rowid" name))))
        (when (and identities (> (length identities) 0))
          (insert-identities cid identities))
        cid)
      (catch sql-error? => (lambda _ (sql-txn-abort db)))
      (finally (sql-txn-commit db)))))

;; Delete an existing identity by unique (cid, network, address) triple.
(define-edit-command (delete-identity-by-address cid network address)
  (sql-eval-query (ensure-contact-db!)
                  "DELETE FROM identity WHERE cid=$1 AND address=$2 AND network=$3"
                  cid address network))

;; Delete an existing identity by unique nickname.
(define-edit-command (delete-identity-by-nickname nickname)
  (sql-eval-query (ensure-contact-db!) "DELETE FROM identity WHERE nickname=$2" nickname))

;; Delete an existing contact.
(define-edit-command (delete-contact cid)
  (sql-eval-query (ensure-contact-db!) "DELETE FROM contact WHERE cid=$1" cid))

;; Delete an existing contact by name.
(define-edit-command (delete-contact-by-name name)
  (sql-eval-query (ensure-contact-db!) "DELETE FROM contact WHERE name=$1" name))

;; Update an existing contact's name and (optionally) identities.
(define-edit-command (update-contact cid name identities)
  (let ((db (ensure-contact-db!)))
    (try (sql-txn-begin db)
      (sql-eval db "UPDATE contact SET name=$1 WHERE cid=$2" name cid)
      (when identities
        (sql-eval db "DELETE FROM identity WHERE cid=$1" cid)
        (insert-identities cid identities))
      (catch sql-error? => (lambda _ (sql-txn-abort db)))
      (finally (sql-txn-commit db)))))

;; List the identities of an existing contact.
(def (get-identities cid)
  (hash<-sql-eval-query
   (ensure-contact-db!)
   "SELECT network, address, nickname, public_key, secret_key_path FROM identity WHERE cid=$1" cid))

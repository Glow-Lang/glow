;;;; HTTP/JSON endpoints for contact & identity management.

(export #t)

(import
 (only-in :clan/config xdg-config-home)
 (only-in :gerbil/gambit path-expand)
 (only-in :gerbil/gambit/exceptions error-exception?)
 (only-in :gerbil/gambit/threads thread-join! thread-state thread-state-running?)
 (only-in :std/srfi/1 first)
 (only-in :std/srfi/13 string-trim)
 :clan/multicall
 :drewc/ftw
 :std/format :std/getopt :std/iter :std/misc/hash :std/sugar :std/text/json
 ./db ./transactions)

;;; First, the client is just a bunch of static files we must serve.

(def wwwroot (xdg-config-home "glow" "wwwroot"))

(def index.html (path-expand "index.html" wwwroot))

(define-endpoint index.html "^/$")
(def (index.html/GET)
  (http-response-static-file* index.html))

(def (request-static-file? req (path (http-request-path req)))
  (def fn (path-expand (string-trim path #\/) wwwroot))
  (if (file-exists? fn) [fn] #f))

(define-endpoint static-file request-static-file?
  priority: 1042)

(define static-file/GET http-response-static-file*)

(def (respond/no-content)
  (http-response-write* 204 [] ""))

(def (respond/JSON code: (code 200) json)
  (let ((o (assget "Origin" (http-request-headers*)))
        (js (if (string? json) json (json-object->string json))))
    (http-response-write* code
                          `(("Content-Type" . "application/json")
                            ("Access-Control-Allow-Origin" . ,(or o "*"))
                            ("Access-Control-Allow-Credentials" . "true"))
                          js)))

(def (respond/JSON-error code: (code 400) error (other-info ()))
  (let ((error (hash (error (match error
                              ((? string?) error)
                              ((? error?) (error-message error)))))))
    (for ((other other-info))
      (match other
        ([key value]
         (set! (hash-ref error key) value))))
    (respond/JSON code: code error)))

(def (http-request-body-json*)
  (with-input-from-u8vector (http-request-body*) read-json))

;;; Endpoints in FTW! are run in priority order.
;;; By default, the priority of all endpoints is ~42~.

;; List all non-native assets.
(define-endpoint list-assets "^/contacts/assets$")
(def (list-assets/GET)
  (respond/JSON (list-assets)))

;; List all networks and their metadata.
(define-endpoint list-networks "^/contacts/networks$")
(def (list-networks/GET)
  (respond/JSON (list-networks)))

;; List all contacts.
(define-endpoint list-contacts "^/contacts/contacts$")
(def (list-contacts/GET)
  (respond/JSON (list-contacts)))

;; Access an individual contact.
(define-endpoint contact "^/contacts/contact$")

;; Can't get a generic contact.
(def (contact/GET)
  (respond/JSON-error code: 404 "Try /contacts/contact/CID"))

;; Add a new contact.
(def (contact/POST)
  (let ((body (http-request-body-json*)))
    (if (not (and (hash-table? body)
                  (hash-get body 'name)))
        (respond/JSON-error code: 400 "Invalid contact description")
        (let-hash body
          (let ((cid (add-contact .name .?identities)))
            (respond/no-content))))))

(define-endpoint contact-by-id "^/contacts/contact/(\\d+)$")

;; Get the metadata and identities of a contact.
(def (contact-by-id/GET cid)
  (let ((contacts (list-contacts cid)))
    (match (length contacts)
      (0 (respond/JSON-error code: 404 "No such contact"))
      (1 (respond/JSON (first contacts)))
      (else (respond/JSON-error code: 500 "Too many contacts")))))

;; Add an identity to an existing contact.
(def (contact-by-id/POST cid)
  (let ((identity (http-request-body-json*)))
    (if (not (and (hash-table? identity)
                  (hash-get identity 'network)
                  (hash-get identity 'address)))
        (respond/JSON-error "Invalid identity description")
        (try
          (add-identity cid identity)
          (catch (e) (respond/JSON-error e))
          (finally (respond/no-content))))))

;; Update an existing contact.
(def (contact-by-id/PUT cid)
  (let ((body (http-request-body-json*)))
    (if (not (and (hash-table? body)
                  (hash-get body 'name)))
        (respond/JSON-error "Invalid contact description")
        (try (let-hash body
               (update-contact cid .name .?identities))
          (catch (e) (respond/JSON-error e))
          (finally (respond/no-content))))))

;; Delete an existing contact.
(def (contact-by-id/DELETE cid)
  (try (delete-contact cid)
    (catch (e) (respond/JSON-error e))
    (finally (respond/no-content))))

;; Run a glow command and report its status & output.
(define-endpoint run-transaction "^/contacts/transaction$")

;; Can't get a generic transaction.
(def (run-transaction/GET)
  (respond/JSON-error code: 404 "Try /contacts/transaction/TXID"))

;; Start a new transaction.
(def (run-transaction/POST)
  (let ((body (http-request-body-json*))
        (txid -1))
    (if (not (and (hash-table? body)
                  (hash-get body 'action)
                  (hash-get body 'args)))
        (respond/JSON-error code: 400 "Invalid transaction description")
        (try
          (let-hash body
            (set! txid (start-transaction .action .args))
            (check-txid! txid))
          (catch (e) (respond/JSON-error code: 400 e `((txid ,txid))))
          (finally (respond/JSON (hash (txid txid) (status #f))))))))

;; Check for output on a transaction.
(define-endpoint transaction-by-id "^/contacts/transaction/(\\d+)$")

(def (transaction-by-id/GET txid)
  (set! txid (string->number txid))
  (if (not txid)
      (respond/JSON-error code: 404 "Invalid transaction ID")
      (try
        (let-values (((output status) (transaction-output txid)))
          (respond/JSON (hash (txid txid)
                              (status (if (number? status)
                                          (format "~d" status)
                                          (void)))
                              (output output))))
        (catch _ (respond/JSON-error code: 404 "No such transaction"
                                     `((txid ,txid)))))))

(define-entry-point (start-server address: (address #f) port: (port #f))
  (help: "Start the contacts API server"
   getopt: [(option 'address "-A" "--address" help: "IP address to listen on")
            (option 'port "-P" "--port" help: "Port to listen on")])
  (let* ((address (or address "127.0.0.1"))
         (port (or port "6742"))
         (address (format "~a:~a" address port))
         (server (start-ftw-http-server! address)))
    (when (thread-state-running? (thread-state server))
      (printf "Glow contacts server running at ~a~%" address))
    ;; TODO: Deamonize.
    (thread-join! server)))

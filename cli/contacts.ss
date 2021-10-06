(export #t)

(import
  :std/format :std/getopt :std/iter :std/misc/hash :std/misc/number :std/sort :std/srfi/13 :std/sugar
  :clan/config :clan/crypto/secp256k1 :clan/files :clan/json :clan/multicall :clan/path :clan/syntax
  :clan/poo/brace :clan/poo/cli :clan/poo/io :clan/poo/mop :clan/poo/object :clan/poo/type
  (only-in :clan/poo/number Nat)
  :mukn/ethereum/cli :mukn/ethereum/hex :mukn/ethereum/ethereum :mukn/ethereum/known-addresses
  :mukn/glow/cli/utils
  (only-in ./identities Identity)
  (rename-in ../contacts/db (add-contact add-contact.db) (list-contacts list-contacts.db)))

(define-type Contact
  (.+
   (Record
    cid: [Nat]
    name: [String]
    identities: [(List Identity)])
   {.json<-: (lambda (contact)
               (with-slots (cid name identities) contact
                 (hash (cid cid)
                       (name name)
                       (identities (map (cut json<- Identity <>) identities)))))}))

(def ContactList (Map String -> Contact))

(def (default-contacts-file) (xdg-config-home "glow" "contacts.json"))

(def (load-contacts.json contacts-file)
  (unless (string? contacts-file) (set! contacts-file (default-contacts-file)))
  (def contacts-json (with-catch
                      (lambda (_) (make-hash-table))
                      (cut read-file-json contacts-file)))
  (<-json ContactList contacts-json))

;; Contact list <-
(def (load-contacts.db)
  (for/collect (contact (list-contacts.db))
    (force-object ; for immediate key registration
     (.o (:: @ Contact)
         (cid (hash-ref contact 'cid))
         (name (hash-ref contact 'name))
         (identities
          (for/collect (identity (hash-ref contact 'identities []))
            (<-json Identity identity)))))))

;; Contact list <- String?
(def (load-contacts contacts-file)
  (match contacts-file
    ((? string?) (load-contacts.json contacts-file))
    (#f (load-contacts.db))))

(def options/contacts
  (make-options
   [(flag 'json "-J" "--json" help: "write contacts as JSON")]))

(define-entry-point (add-contact
                     name: (name #f)
                     json: (json #f))
  (help: "Add contact"
   getopt: (make-options
            [(option 'name "-N" "--name" help: "name of contact")]
            []
            [options/contacts options/help]))
  ;; TODO: Provide some way for entrypoint to introspect
  ;; options, maybe via gerbil-poo?
  ;; e.g. (.@ self options)
  (def options
       (hash
        (name name)
        (json json)))
  ;; NOTE: Added scope to avoid `name` being bound to #!void during macro expansion.
  (let ((name (get-or-ask options 'name (lambda () (ask-string "Enter contact name: "))))
        (cid (add-contact.db name [])))
    (if json
        (displayln (string<-json (hash (cid cid) (name name))))
        (displayln "Added contact " name ", cid " cid))))

(define-entry-point (remove-contact
                     cid: (cid #f)
                     name: (name #f)
                     json: (json #f))
  (help: "Remove contact"
   getopt: (make-options [(option 'cid "-C" "--cid" help: "contact ID")
                          (option 'name "-N" "--name" help: "name of contact")]
                         []
                         [options/contacts]))
  (cond (cid (delete-contact cid))
        (name (delete-contact-by-name name))
        (else (error "Must supply cid or name")))
  (if json
      (displayln (string<-json (hash (removed (or cid name)))))
      (displayln "Removed contact " (or cid name))))

(define-entry-point (list-contacts
                     contacts: (contacts-file #f)
                     json: (json #f))
  (help: "List contacts" getopt: options/contacts)
  (def contacts (sort (load-contacts contacts-file)
                      (lambda (c1 c2) (< (.@ c1 cid) (.@ c2 cid)))))
  (if json
      (displayln (string<-json (map (cut json<- Contact <>) contacts)))
      (for-each
        (lambda (contact)
          (with-slots (cid name identities) contact
            (printf "~d. ~a~%" cid (match name
                                     ((? string?) name)
                                     ((? void?) "<anonymous>")))
            (for-each
              (lambda (identity)
                (with-slots (nickname network address keypair) identity
                  (printf " ↳ ~a ~a~a~a~%"
                          network
                          (0x<-address address)
                          (if (string? nickname) (format " (~a)" nickname) "")
                          (if (and keypair (keypair-secret-key keypair)) " 🔑" ""))))
              identities)))
        contacts)))

(define-entry-point (list-identities . args)
  (help: "Alias for list-contacts" getopt: options/contacts)
  (apply list-contacts args))

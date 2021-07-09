(export #t)

(import
  :std/format :std/getopt :std/iter :std/misc/hash :std/misc/number :std/srfi/13 :std/sugar
  :clan/config :clan/files :clan/json :clan/multicall :clan/path :clan/syntax
  :clan/poo/brace :clan/poo/cli :clan/poo/io :clan/poo/mop :clan/poo/object :clan/poo/type
  (only-in :clan/poo/number Nat)
  :mukn/ethereum/cli :mukn/ethereum/hex :mukn/ethereum/ethereum :mukn/ethereum/known-addresses
  (only-in ./identities Identity load-identities options/identities)
  (rename-in :mukn/glow-contacts/contacts
             (add-contact add-contact-db)
             (list-contacts list-contacts-db)))

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

(def (load-contacts.db)
  (for/collect (contact (list-contacts-db))
    (.o (:: @ Contact)
        (cid (hash-ref contact 'cid))
        (name (hash-ref contact 'name))
        (identities
         (for/collect (identity (hash-ref contact 'identities []))
           (let* ((nickname (hash-ref identity 'nickname))
                  (network (make-symbol (hash-ref identity 'network)))
                  (address (address<-0x (hash-ref identity 'address)))
                  (public-key (hash-get identity 'public_key))
                  (keypair (hash-get keypair-by-address address)))
             (.call Identity .make nickname network address public-key keypair)))))))

(def (load-contacts contacts-file identities-file)
  (load-identities from: identities-file)
  (match contacts-file
    ((? string?) (load-contacts.json contacts-file))
    (#f (load-contacts.db))))

(def options/contacts
  (make-options
   [(flag 'json "-J" "--json" help: "write contacts as JSON")]
   []
   [options/identities]))

(define-entry-point (add-contact name: (name #f) json: (json #f))
  (help: "Add contact"
   getopt: (make-options
            [(option 'name "-N" "--name" help: "name of contact")]
            []
            [options/contacts]))
  (unless name (error "missing name"))
  (def cid (add-contact-db name []))
  (if json
      (displayln (string<-json (hash (cid cid) (name name))))
      (displayln "Added contact " name ", cid " cid)))

(define-entry-point (remove-contact cid: (cid #f) name: (name #f) json: (json #f))
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
                     identities: (identities-file #f)
                     json: (json #f))
  (help: "List contacts" getopt: options/contacts)
  (def contacts (load-contacts contacts-file identities-file))
  (if json
      (displayln (string<-json (map (cut json<- Contact <>) contacts)))
      (for-each
        (lambda (contact)
          (with-slots (cid name identities) contact
            (printf "~d. ~a~%" cid name)
            (for-each
              (lambda (identity)
                (with-slots (nickname network address) identity
                  (printf "↳ ~a ~a~a~%"
                          network
                          (0x<-address address)
                          (if (string? nickname) (format " (~a)" nickname) ""))))
              identities)))
        contacts)))

(define-entry-point (list-identities . args)
  (help: "Alias for list-contacts" getopt: options/contacts)
  (apply list-contacts args))

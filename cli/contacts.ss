(export #t)

(import
  :std/getopt :std/misc/hash :std/srfi/13
  :clan/config :clan/json :clan/multicall :clan/syntax
  :clan/poo/brace :clan/poo/cli :clan/poo/io :clan/poo/mop :clan/poo/object :clan/poo/type
  :mukn/ethereum/cli :mukn/ethereum/hex :mukn/ethereum/known-addresses :mukn/ethereum/signing)

(define-type Contact
  (Record
    nickname: [String]
    address: [Address]
    type: [Symbol]
    tags: [(List String)]))

(def ContactList (Map String <- Contact))

(def (load-contacts contacts-file)
  (def contacts-json (read-file-json contacts-file))
  (hash-key-value-map (lambda (k v) (cons k (<-json Contact v))) contacts-json))

(def (store-contacts contacts-file contacts)
  (def contacts-json (hash-key-value-map (lambda (k v) (cons k (json<- Contact v))) contacts))
  (write-file-json contacts-file contacts-json))

(def (with-contacts contacts-file f)
  (def contacts
    (if (file-exists? contacts-file)
      (load-contacts contacts-file)
      (make-hash-table)))
  (f contacts)
  (store-contacts contacts-file contacts))

(def options/contacts
  (make-options
    [(option 'contacts "-C" "--contacts" default: (xdg-config-home "glow" "contacts.json")
             help: "file to load and store contacts")] []))

(define-entry-point (add-contact . arguments)
  "Add contact"
  (def options/add
    (make-options
      [(option 'nickname "-N" "--nickname"
               help: "nickname of contact")
       (option 'address "-A" "--address"
               help: "address of contact")
       (option 'type "-T" "--type" default: #f
               help: "type of contact address")]
      []
      [options/test options/contacts]))
  (def options (process-options options/add arguments))
  (def nickname (hash-get options 'nickname))
  (def address (parse-address (hash-get options 'address)))
  (def contacts-file (hash-get options 'contacts))
  (def new-contact
    {nickname
     address
     ;; TODO: Is there a function for getting the full module path to a definition?
     type: ':mukn/ethereum/signing#Address
     tags: []})
  (with-contacts contacts-file
    (cut hash-put! <> (string-downcase nickname) new-contact))
  (displayln "Added " nickname " [ " (string<- Address address) " ]"))

(define-entry-point (remove-contact . arguments)
  "Remove contact"
  (def options/remove
    (make-options
      [(option 'nickname "-N" "--nickname")] [] [options/contacts]))
  (def options (process-options options/remove arguments))
  (def contacts-file (hash-get options 'contacts))
  (def nickname (hash-get options 'nickname))
  (with-contacts contacts-file
    (cut hash-remove! <> (string-downcase nickname)))
  (displayln "Removed " nickname))

(define-entry-point (list-contacts . arguments)
  "List contacts"
  (def options (process-options options/contacts arguments))
  (def contacts-file (hash-get options 'contacts))
  (def contacts (load-contacts contacts-file))
  (for-each
    (match <>
      ([_ . contact]
        (displayln (.@ contact nickname) " [ " (string<- Address (.@ contact address)) " ]")))
    (hash->list contacts)))

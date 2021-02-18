(export #t)

(import
  :clan/json :clan/multicall :std/getopt :clan/poo/cli :clan/poo/object
  :mukn/ethereum/cli :mukn/ethereum/hex :mukn/ethereum/known-addresses :mukn/ethereum/signing)

(def (load-contacts contacts-file)
  (read-file-json contacts-file))

(def (store-contacts contacts-file contacts)
  (write-file-json contacts-file contacts))

(def (with-contacts contacts-file f)
  (def contacts
    (if (file-exists? contacts-file)
      (load-contacts contacts-file)
      (make-hash-table)))
  (f contacts)
  (store-contacts contacts-file contacts))

(def options/contacts
  (make-options
    [(option 'file "-F" "--file" default: "run/contacts.json"
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
  (def contacts-file (hash-get options 'file))
  (with-contacts contacts-file
    (cut hash-put! <> nickname (.call Address .json<- address)))
  (displayln "Added " nickname " [ " (.call Address .string<- address) " ]"))

(define-entry-point (remove-contact . arguments)
  "Remove contact"
  (def options/remove
    (make-options
      [(option 'nickname "-N" "--nickname")] [] [options/contacts]))
  (def options (process-options options/remove arguments))
  (def contacts-file (hash-get options 'file))
  (def nickname (hash-get options 'nickname))
  (with-contacts contacts-file
    (cut hash-remove! <> nickname))
  (displayln "Removed " nickname))

(define-entry-point (list-contacts . arguments)
  "List contacts"
  (def options (process-options options/contacts arguments))
  (def contacts-file (hash-get options 'file))
  (def contacts (load-contacts contacts-file))
  (for-each
    (cut match <>
      ([nickname . address]
        (displayln nickname " [ " address " ]")))
    (hash->list contacts)))

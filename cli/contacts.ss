(export #t)

(import
  :std/getopt :std/misc/hash :std/srfi/13
  :clan/config :clan/files :clan/json :clan/multicall :clan/syntax
  :clan/poo/brace :clan/poo/cli :clan/poo/io :clan/poo/mop :clan/poo/object :clan/poo/type
  :mukn/ethereum/cli :mukn/ethereum/hex :mukn/ethereum/ethereum :mukn/ethereum/known-addresses)

(define-type Contact
  (Record
    nickname: [String]
    address: [Address]
    type: [Symbol]
    blockchain: [Symbol]
    tags: [(List String)]))

(def ContactList (Map String -> Contact))

(def (load-contacts contacts-file)
  (def contacts-json (with-catch (lambda (_) (make-hash-table)) (cut read-file-json contacts-file)))
  (<-json ContactList contacts-json))

(def (store-contacts contacts-file contacts)
  (def contacts-json (json<- ContactList contacts))
  (clobber-file contacts-file (string<-json contacts-json) salt?: #t))

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

(define-entry-point (add-contact nickname: (nickname #f)
                                 address: (address #f)
                                 type: (type #f)
                                 blockchain: (blockchain #f)
                                 contacts: (contacts-file #f))
  (help: "Add contact"
   getopt: (make-options
            ;; TODO: Store all non-required options as tags
            ;; TODO: Add network parameter?
            [(option 'nickname "-N" "--nickname"
                     help: "nickname of contact")
             (option 'address "-A" "--address"
                     help: "address of contact")
             (option 'type "-T" "--type" default: 'ethereum
                     help: "type of contact address")
             (option 'blockchain "-B" "--blockchain" default: 'ethereum ;; use -E from ??? ?
                     help: "blockchain of the address")]
            []
            [options/test options/contacts]))
  (def new-contact
    {nickname
     address
     type: (or type 'ethereum)
     blockchain: (or blockchain 'ethereum)
     tags: []})
  (with-contacts contacts-file
    (cut hash-put! <> (string-downcase nickname) new-contact))
  (displayln "Added contact " nickname " [ " (string<- Address address) " ]"))

(define-entry-point (remove-contact nickname: (nickname #f) contacts: (contacts-file #f))
  (help: "Remove contact"
   getopt: (make-options [(option 'nickname "-N" "--nickname")] [] [options/contacts]))
  (with-contacts contacts-file
    (cut hash-remove! <> (string-downcase nickname)))
  (displayln "Removed contact " nickname))

(define-entry-point (list-contacts contacts: (contacts-file #f))
  (help: "List contacts" getopt: options/contacts)
  (def contacts (load-contacts contacts-file))
  (for-each
    (match <>
      ([_ . contact]
        (displayln (.@ contact nickname) " [ " (string<- Address (.@ contact address)) " ]")))
    (hash->list/sort contacts string<?)))

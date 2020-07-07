(export #t)

(import
  :std/format :clan/utils/json
  :clan/poo/io
  (only-in :clan/poo/type Map)
  ./hex ./types ./signing)

;; TODO: handle collisions, exceptions
(def address-by-nickname (make-hash-table))
(def nickname-by-address (make-hash-table))
(def (register-address nickname address)
  (hash-put! nickname-by-address address nickname)
  (hash-put! address-by-nickname nickname address))
(def (nickname<-address address)
  (hash-get nickname-by-address address))
  ;; (or (get-nickname-of-address address) (error "No registered nickname for address" (0x<-address address)))
(def (address<-nickname nickname)
  (hash-get address-by-nickname nickname))
  ;; (or (address<-nickname nickname) (error "No registered nickname" nickname)))
(def (nicknamed-string<-address address)
  (def s (0x<-address address))
  (def n (nickname<-address address))
  (if n (format "~a (~a)" n s) s))
(def (unregister-address nickname)
  (def address (address<-nickname nickname))
  (hash-remove! address-by-nickname nickname)
  (hash-remove! nickname-by-address address))



(def keypair-by-address (make-hash-table))
(def (register-keypair nickname keypair)
  (def address (keypair-address keypair))
  (hash-put! keypair-by-address address keypair)
  (register-address nickname address))
(def (unregister-keypair nickname)
  (def address (address<-nickname nickname))
  (hash-remove! keypair-by-address address)
  (unregister-address nickname))
(def (keypair<-address address)
  (hash-get keypair-by-address address))
;; (or (keypair<-address address) (error "No registered keypair for address" address))

;; TODO: Add a layer of encryption for these files.
(def (register-file-keypairs file)
  (hash-for-each register-keypair (<-json (Map Keypair <- String) (read-file-json file))))

(def (addresses-with-registered-keypair)
  (hash-keys keypair-by-address))

(def (nicknames-with-registered-keypair)
  (filter identity (map nickname<-address (addresses-with-registered-keypair))))

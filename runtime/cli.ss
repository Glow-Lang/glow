(export #t)

(import
  :std/getopt :std/misc/hash :std/sugar
  :clan/base :clan/json :clan/multicall :clan/path-config :clan/syntax
  :clan/poo/debug
  :clan/persist/db
  :mukn/ethereum/cli :mukn/ethereum/types :mukn/ethereum/json-rpc
  ./ethereum-runtime ./reify-contract-parameters)

(def (json<-cli-input cli-input)
  (cond
   ((equal? cli-input "-")
    (json<-port (current-input-port)))
   ((and (string? cli-input)
         (< 0 (string-length cli-input))
         (string-index "[{\"0123456789-" (string-ref cli-input 0)))
    (json<-string cli-input))
   ((string? cli-input)
    (read-file-json cli-input))
   (else (error "invalid input specifier" 'json<-cli-input cli-input))))

;; TODO: also accept local interaction parameters
;; TODO: accept alternative ethereum networks, etc
(define-entry-point (start-interaction . arguments)
  "Start an interaction based on an agreement"
  (def gopt
    (getopt
     ;;(flag 'test-identities "--test-identities" default: #f help: "name of ethereum network")
     (option 'ethereum-network "-E" "--ethereum-network" default: "pet"
             help: "name of ethereum network")
     (option 'database "-D" "--database" default: (run-path "testdb")
             help: "path to local DApp state database")
     (argument 'role help: "role to play in the interaction")
     (argument 'agreement help: "JSON agreement for the interaction")))
  (def opt (getopt-parse gopt arguments))
  (defrule {symbol} (hash-get opt 'symbol))
  ;; TODO: validate role, with nice user-friendly error message
  (def role (string->symbol {role}))
  ;; TODO: validate agreement, with nice user-friendly error message
  (def agreement (<-json InteractionAgreement (json<-cli-input {agreement})))
  ;; TODO: validate ethereum network, with nice user-friendly error message
  (ensure-ethereum-connection {ethereum-network})
  ;; TODO: validate database, with nice user-friendly error message
  (ensure-db-connection {database})
  (DDT start-interaction:
       Symbol role
       InteractionAgreement agreement
       String {ethereum-network}
       String {database})
  (def environment (run role agreement))
  (displayln "Final environment:")
  ;; TODO: get run to include type t, and print v using type t
  (for-each (match <> ([k . v] (display-poo-ln k "=> " v)))
            (hash->list/sort environment symbol<?)))

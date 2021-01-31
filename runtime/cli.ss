(export #t)

(import
  :gerbil/expander
  :std/getopt :std/misc/hash :std/sugar
  :clan/base :clan/cli :clan/exit :clan/json :clan/multicall :clan/path-config :clan/syntax
  :clan/poo/debug
  :clan/persist/db
  :mukn/ethereum/cli :mukn/ethereum/types :mukn/ethereum/json-rpc
  ./participant-runtime ./reify-contract-parameters ./configuration)

(def common-options
  [(flag 'test "--test"
         help: "enable testing including test identities")
   (flag 'backtrace "--backtrace"
         help: "enable backtraces for debugging purposes")
   (option 'ethereum-network "-E" "--ethereum-network" default: "pet"
           help: "name of ethereum network")
   (option 'database "-D" "--database" default: (run-path "testdb")
           help: "path to local DApp state database")])

(def (getopt/common-options . options)
  (apply getopt (append common-options options)))

(def process-common-options
  (lambda-opt
   (backtrace-on-abort? {backtrace})
   (cond
    ({test} (import-module ':mukn/ethereum/testing #t #t))
    (else (load-secret-key-ring)))))

;; TODO: also accept local interaction parameters
;; TODO: accept alternative ethereum networks, etc
(define-entry-point (start-interaction . arguments)
  "Start an interaction based on an agreement"
  (def gopt
    (getopt/common-options
     (argument 'role help: "role to play in the interaction")
     (argument 'agreement help: "JSON agreement for the interaction")))
  (def opt (getopt-parse gopt arguments))
  (process-common-options opt)
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
  ;; TODO: get run to include type t and pre-alpha-converted labels,
  ;; and output the entire thing as JSON omitting shadowed variables (rather than having conflicts)
  (for-each (match <> ([k . v] (display-poo-ln k "=> " v)))
            (hash->list/sort environment symbol<?)))

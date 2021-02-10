(export #t)

(import
  :gerbil/expander :gerbil/gambit/ports
  :std/format :std/getopt :std/iter :std/misc/hash :std/misc/repr :std/sugar :std/text/json
  :clan/base :clan/cli :clan/exit :clan/json :clan/multicall :clan/path-config :clan/syntax
  :clan/poo/brace :clan/poo/debug :clan/poo/object
  :clan/persist/db :clan/persist/content-addressing :clan/versioning
  :mukn/ethereum/assets :mukn/ethereum/cli :mukn/ethereum/ethereum :mukn/ethereum/network-config
  :mukn/ethereum/signing :mukn/ethereum/testing :mukn/ethereum/types :mukn/ethereum/json-rpc :mukn/ethereum/known-addresses
  ./participant-runtime ./reify-contract-parameters ./configuration ./program ./terminal-codes
  (only-in ../compiler/alpha-convert/alpha-convert init-syms)
  ../compiler/passes
  ../compiler/multipass
  ../compiler/syntax-context)

(def (flush-input port)
   (input-port-timeout-set! port 0.001)
   (let loop () (if (not (eof-object? (read-line port))) (loop)))
   (input-port-timeout-set! port +inf.0))

(def (read-line-from-console)
   (let ((port (console-port)))
     (flush-input port)
     (read-line port)))

(def common-options
  [(flag 'test "--test"
         help: "enable testing including test identities")
   (flag 'backtrace "--backtrace"
         help: "enable backtraces for debugging purposes")
   (option 'ethereum-network "-E" "--ethereum-network" default: "pet"
           help: "name of ethereum network")
   (option 'database "-D" "--database" default: #f
           help: "path to local DApp state database")
   (option 'contract "-C" "--contract" default: #f
           help: "path to Glow contract")
   (option 'interaction "-I" "--interaction" default: #f
           help: "name of interaction within contract")
   (option 'role "-R" "--role" default: #f
           help: "name of role you want to play in the interaction")])

(def (getopt/common-options . options)
  (apply getopt (append common-options options)))

(def process-common-options
  (lambda-opt
   (backtrace-on-abort? {backtrace})
   (cond
    ({test} (import-module ':mukn/ethereum/testing #t #t))
    (else (load-secret-key-ring)))))

(def (ask-option name options)
  (def options-count (length options))
  ;; TODO: Move this somewhere else, probably gerbil-utils?
  (def (range a b)
    (def ∆ (- b a))
    (if (<= 0 ∆) (iota ∆ a) (iota (- ∆) a -1)))
  (def indexed-options (map cons (range 0 options-count) options))
  (displayln BOLD name END)
  (for ((indexed-option indexed-options))
    (def index (car indexed-option))
    (display (string-append (number->string (+ index 1)) ") "))
    (match (cdr indexed-option)
      ([option . annotation]
        ;; TODO: Case on annotation type? Or type directed printing?
        (displayln option " - " (0x<-address annotation)))
      (option
        (displayln option))))
  (display-prompt "Enter number")
  (let* ((input (read-line-from-console))
         (response (string->number input)))
    (if (<= 1 response options-count)
      (begin
        (displayln)
        (match (list-ref options (- response 1))
          ([option . _]
            option)
          (option
            option)))
      (begin
        (displayln FAIL "Invalid selection\n" END)
        (ask-option name options)))))

(def (ask-string name)
  (display-prompt name)
  (def result (<-json String (read-json (current-input-port))))
  (displayln)
  result)

(def (ask-number name)
  (display-prompt name)
  (def result (<-json Nat (read-json (current-input-port))))
  (displayln)
  result)

;; TODO: Catch parsing errors and show example inputs.
(def (ask-address name)
  (display-prompt (string-append "Enter address of " name))
  (def port (current-input-port))
  (try
    (<-json Address (read-line-from-console))
    (catch (e)
      (displayln FAIL "\nError parsing address: " (error-message e) END)
      (displayln "Input should be 0x followed by 40 hexadecimal characters.")
      (displayln "E.g., \"0x73e27C9B8BF6F00A38cD654079413aA3eDBC771A\"\n")
      (flush-input port)
      (ask-address name))))

(def (display-prompt name)
  (displayln CYAN name)
  (display (string-append "> " END)))

(def (ask-contract)
  (ask-option "Choose contract" ["mukn/glow/examples/buy_sig"]))

(def (ask-interaction contract)
  (ask-option "Choose interaction" ["payForSignature"]))

(def (ask-participants selected-identity selected-role role-names)
  (displayln BOLD "Assign roles" END)
  (let (participants (make-hash-table))
    (for ((role-name role-names))
      (def role-address
        (if (equal? selected-role role-name)
          (hash-get address-by-nickname selected-identity)
          (ask-address (symbol->string role-name))))
      (hash-put! participants role-name role-address))
    participants))

(def (ask-input type name)
  (display-prompt
    (string-append
      "Enter "
      (if (u8vector? name) (bytes->string name) (symbol->string name))
      " : "
      (symbol->string (.@ type sexp))))
  (try
    (let (input (<-json type (read-json (current-input-port))))
      (json<- type input))
    (catch (e)
      (displayln FAIL "\nError parsing input: " (error-message e) END)
      (flush-input (console-port))
      (ask-input type name))))

(def (ask-parameters program)
  (let ((parameter-names (@ program parameter-names))
        (parameters (make-hash-table)))
    (for ((name parameter-names))
      (def input (ask-input (lookup-type program name) name))
      (hash-put! parameters name input))
    parameters))

;; TODO: also accept local interaction parameters
;; TODO: accept alternative ethereum networks, etc
(define-entry-point (start-interaction . arguments)
  "Start an interaction based on an agreement"
  (nest
    (let (gopt (getopt/common-options)))
    (let (opt (getopt-parse gopt arguments)))
    (begin
      (process-common-options opt))
    ;; TODO: Load test keypairs if in test mode, otherwise let users use their own.
    (let (test (hash-get opt 'test)))
    (let (database (hash-get opt 'database)))
    (begin
      (ensure-db-connection (or database (run-path (if test "testdb" "userdb")))))
    ;; TODO: validate ethereum network, with nice user-friendly error message
    (let (ethereum-network (hash-get opt 'ethereum-network)))
    (begin
      (ensure-ethereum-connection ethereum-network)
      (displayln))
    (let (contract-name (or (hash-get opt 'contract) (ask-contract))))
    ;; TODO: Extract interaction names from contract
    (let (interaction-name (or "payForSignature"
                               (hash-get opt 'interaction)
                               (ask-interaction contract-name))))
    ;; TODO: Extract path from contract-name
    (let (contract.glow (source-path "examples/buy_sig.glow")))
    (let (compiler-output (run-passes contract.glow pass: 'project show?: #f)))
    (let (program (parse-compiler-output compiler-output)))
    (let (role-names (filter identity (hash-keys (@ program interactions)))))
    (let (selected-identity (ask-option "Choose your identity" (hash->list address-by-nickname))))
    (let (selected-role (symbolify (or (hash-get opt 'role)
                                       (ask-option "Choose your role" role-names)))))
    (let (participants-table (ask-participants selected-identity selected-role role-names)))
    (let (parameters (ask-parameters program)))
    (let (current-block-number (eth_blockNumber)))
    (let (max-initial-block
            (ask-number
              (string-append "Max initial block "
                             "[Current block number: " (number->string current-block-number) "]"))))

    ;; TODO: Validate agreement, with nice user-friendly error message.
    ;; TODO: Output agreement as a command for the other user to copy paste and automatically fill in arguments.
    (let (agreement
            {interaction: (string-append contract-name "#" interaction-name)
             participants: (.<-alist (hash->list participants-table))
             parameters
             glow-version: (software-identifier)
             code-digest: (digest<-file contract.glow)
             reference: {}
             options: {blockchain: "Private Ethereum Testnet"
                      escrowAmount: (void)
                      timeoutInBlocks: (* 10 (ethereum-timeout-in-blocks))
                      maxInitialBlock: max-initial-block}}))
    (let (environment (run selected-role agreement))
      (displayln "Final environment:")
      ;; TODO: get run to include type t and pre-alpha-converted labels,
      ;; and output the entire thing as JSON omitting shadowed variables (rather than having conflicts)
      (for-each (match <> ([k t . v] (display-object-ln k "=> " t v)))
                (hash->list/sort environment symbol<?)))))

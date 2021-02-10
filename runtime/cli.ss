(export #t)

(import
  :gerbil/expander :gerbil/gambit/ports
  :std/format :std/getopt :std/iter :std/misc/hash :std/misc/repr :std/sugar :std/text/json
  :clan/base :clan/cli :clan/exit :clan/json :clan/multicall :clan/path-config :clan/syntax
  :clan/poo/brace :clan/poo/debug :clan/poo/object
  :clan/persist/db :clan/persist/content-addressing :clan/versioning
  :mukn/ethereum/assets :mukn/ethereum/cli :mukn/ethereum/ethereum :mukn/ethereum/network-config
  :mukn/ethereum/signing :mukn/ethereum/testing :mukn/ethereum/types :mukn/ethereum/json-rpc :mukn/ethereum/known-addresses
  ./participant-runtime ./reify-contract-parameters ./configuration ./program
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
  (displayln name)
  (for ((indexed-option indexed-options))
    (match indexed-option
      ([index . option]
        (displayln (+ index 1) ") " option))))
  (display-prompt "Enter number")
  (let* ((input (read-line-from-console))
         (response (string->number input)))
    (if (<= 1 response options-count)
      (list-ref options (- response 1))
      (begin
        (displayln "Invalid selection\n")
        (ask-option name options)))))

(def (ask-string name)
  (display-prompt name)
  (<-json String (read-json (current-input-port))))

(def (ask-number name)
  (display-prompt name)
  (<-json Nat (read-json (current-input-port))))

;; TODO: Catch parsing errors and show example inputs.
(def (ask-address name)
  (display-prompt (string-append "Enter address of " name))
  (def port (current-input-port))
  (try
    (<-json Address (read-json port))
    (catch (e)
      (displayln "\nError parsing address: " (error-message e))
      (displayln "Input should be 0x followed by 36 hexadecimal characters.")
      (displayln "E.g., \"0x73e27C9B8BF6F00A38cD654079413aA3eDBC771A\"\n")
      (flush-input port)
      (ask-address name))))

(def (display-prompt name)
  (displayln name)
  (display "> "))

(def (ask-contract)
  (ask-option "Choose contract" ["mukn/glow/examples/buy_sig"]))

(def (ask-interaction contract)
  (ask-option "Choose interaction" ["payForSignature"]))

(def (ask-participants selected-identity selected-role role-names)
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
      (displayln "\nError parsing input - " (error-message e))
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
  (let* ((gopt (getopt/common-options))
         (opt (getopt-parse gopt arguments)))
    (process-common-options opt)
    (def contract-name (or (hash-get opt 'contract) (ask-contract)))
    (def interaction-name (or (hash-get opt 'interaction) (ask-interaction contract-name)))

    (def interaction (string-append contract-name "#" interaction-name))
    ;; TODO: Extract path from contract-name
    (def contract.glow (source-path "examples/buy_sig.glow"))
    (def compiler-output (run-passes contract.glow pass: 'project show?: #f))
    (def program (parse-compiler-output compiler-output))
    (def role-names (filter identity (hash-keys (@ program interactions))))

    (def selected-identity
      (ask-option "Choose your identity" (hash-keys address-by-nickname)))
    (def selected-role
      (symbolify (or (hash-get opt 'role)
                     (ask-option "Choose your role" role-names))))
    (def participants-table (ask-participants selected-identity selected-role role-names))
    (def parameters (ask-parameters program))

    (def test (hash-get opt 'test))
    ;; TODO: validate ethereum network, with nice user-friendly error message
    (def database (hash-get opt 'database))
    (ensure-db-connection (or database (run-path (if test "testdb" "userdb"))))
    (def ethereum-network (hash-get opt 'ethereum-network))
    (ensure-ethereum-connection ethereum-network)

    (let (block (eth_blockNumber))
      (def agreement
        {interaction
        participants: (.<-alist (hash->list participants-table))
        parameters
        glow-version: (software-identifier)
        code-digest: (digest<-file contract.glow)
        reference: {}
        options: {blockchain: "Private Ethereum Testnet"
                  escrowAmount: (void)
                  timeoutInBlocks: (* 10 (ethereum-timeout-in-blocks))
                  maxInitialBlock: (+ block timeoutInBlocks)}})

      ;; TODO: validate agreement, with nice user-friendly error message
      (ensure-addresses-prefunded)
      (def environment (run selected-role agreement))
      (displayln "Final environment:")
      ;; TODO: get run to include type t and pre-alpha-converted labels,
      ;; and output the entire thing as JSON omitting shadowed variables (rather than having conflicts)
      (for-each (match <> ([k t . v] (display-object-ln k "=> " t v)))
                (hash->list/sort environment symbol<?)))))

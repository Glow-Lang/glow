(export #t)

(import
  :gerbil/expander :gerbil/gambit/ports
  :std/format :std/generic :std/getopt :std/iter :std/misc/hash :std/misc/repr :std/misc/string :std/pregexp :std/sort :std/srfi/13 :std/sugar :std/text/json
  :clan/base :clan/cli :clan/exit :clan/json :clan/multicall :clan/path-config :clan/syntax
  :clan/poo/brace :clan/poo/cli :clan/poo/debug :clan/poo/object
  :clan/persist/db :clan/persist/content-addressing :clan/versioning
  :mukn/ethereum/assets :mukn/ethereum/cli :mukn/ethereum/ethereum :mukn/ethereum/network-config
  :mukn/ethereum/signing :mukn/ethereum/types :mukn/ethereum/json-rpc :mukn/ethereum/known-addresses
  :mukn/glow/runtime/participant-runtime :mukn/glow/runtime/reify-contract-parameters :mukn/glow/runtime/configuration
  :mukn/glow/runtime/program :mukn/glow/runtime/terminal-codes
  (only-in :mukn/glow/compiler/alpha-convert/alpha-convert init-syms)
  :mukn/glow/compiler/passes :mukn/glow/compiler/multipass :mukn/glow/compiler/syntax-context
  :mukn/glow/cli/contacts)

(def options/interaction
  (make-options
    [(flag 'backtrace "--backtrace"
           help: "enable backtraces for debugging purposes")
     (option 'agreement "-A" "--agreement" default: #f
             help: "interaction parameters as JSON")
     (option 'contract "-C" "--contract" default: #f
             help: "path to Glow contract")
     (option 'interaction "-I" "--interaction" default: #f
             help: "name of interaction within contract")
     (option 'role "-R" "--role" default: #f
             help: "role you want to play in the interaction")
     (option 'max-initial-block "-B" default: #f
             help: "maximum block number the contract can begin at")]
    []
    [options/database options/ethereum-network options/test options/contacts]))

(def (ask-option name options)
  (def options-count (length options))
  ;; TODO: Move this somewhere else, probably gerbil-utils?
  (def (range a b)
    (def ∆ (- b a))
    (if (<= 0 ∆) (iota ∆ a) (iota (- ∆) a -1)))
  (def indexed-options (map cons (range 0 options-count) options))
  (displayln BOLD name ":" END)
  (for ((indexed-option indexed-options))
    (def index (car indexed-option))
    (display (string-append (number->string (+ index 1)) ") "))
    (match (cdr indexed-option)
      ([option . (type . annotation)]
        (displayln option " - " (.call type .string<- annotation)))
      ([option . option-short]
        (displayln option-short))
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
  (def result (read-line-from-console))
  (displayln)
  result)

(def (ask-number name)
  (display-prompt name)
  (def result (.call Nat .<-string (read-line-from-console)))
  (displayln)
  result)

;; TODO: Catch parsing errors and show example inputs.
(def (ask-address name contacts)
  (ask-option name
    (map
      (cut match <>
        ([nickname . address]
          (cons nickname [Address . address])))
      (hash->list contacts))))
    ;(let (input (read-line-from-console))
    ;  (address<-0x input))
    ; (catch (e)
    ;   (displayln FAIL "\nError parsing address: " (error-message e) END)
    ;   (displayln "Input should be 0x followed by 40 hexadecimal characters.")
    ;   (displayln "E.g., 0x73e27C9B8BF6F00A38cD654079413aA3eDBC771A\n")
    ;   (ask-address name contacts))))

(def (display-prompt name)
  (displayln CYAN name)
  (display (string-append "> " END)))

(def (ask-contract)
  (ask-option "Choose contract"
    [(cons "mukn/glow/examples/coin_flip" "coin_flip")
     (cons "mukn/glow/examples/buy_sig" "buy_sig")
     (cons "mukn/glow/examples/rps_simple" "rps_simple")]))

(def (ask-participants selected-identity selected-role role-names contacts)
  (displayln BOLD "Assign roles" END)
  (let (participants (make-hash-table))
    (for ((role-name role-names))
      (def role-address
        (if (equal? selected-role role-name)
          (hash-get address-by-nickname selected-identity)
          (ask-address (symbol->string role-name) contacts)))
      (hash-put! participants role-name role-address))
    (displayln)
    participants))

(def (console-input type name tag)
  (display-prompt
    (string-append
      "Enter "
      (if (u8vector? name) (bytes->string name) (symbol->string name))
      (if tag (str tag) "")))
  (try
    (let (input (read-line-from-console))
      (.call type .<-string input))
    (catch (e)
      (displayln FAIL "\nError parsing input: " (error-message e) END)
      (flush-input (console-port))
      (console-input type name tag))))

(def (ask-parameters program)
  (displayln BOLD "Define parameters" END)
  (let ((parameter-names (@ program parameter-names))
        (parameters (make-hash-table)))
    (for ((name parameter-names))
      (def type (lookup-type program name))
      (def input (console-input type name #f))
      (hash-put! parameters name (json<- type input)))
    (displayln)
    parameters))

(def (print-command agreement)
  (displayln MAGENTA "One line command for other participants to generate the same agreement:" END)
  (display "./glow start-interaction --agreement ")
  (def agreement-string (string<-json (json<- InteractionAgreement agreement)))
  (if (string-contains agreement-string "'")
    (pr agreement-string)
    (display (string-append "'" agreement-string "'")))
  (displayln))

(def (get-or-ask options option ask-function)
  (if-let (option-value (hash-get options option))
    option-value
    (let (input (apply ask-function []))
      (hash-put! options option input)
      input)))

(def (ask-role options role-names)
  (get-or-ask options
    'role
    (λ () (ask-option "Choose your role" role-names))))

(def (ask-identity options)
  (get-or-ask options
    'identity
    (λ () (ask-option
            "Choose your identity"
            (map (match <>
                  ([nickname . address]
                    (let (dependent-pair [Address . address])
                      [nickname . dependent-pair])))
                (hash->list/sort address-by-nickname string<?))))))

(def (ask-max-initial-block options current-block-number)
  (get-or-ask options
    'max-initial-block
    (λ ()
      (ask-number
        (string-append
          "Max initial block "
          "[ Current block number is " (number->string current-block-number) " ]")))))

(def (compile-contract contract.glow)
  (def compiler-output (run-passes contract.glow pass: 'project show?: #f))
  (parse-compiler-output compiler-output))

(def (extract-contract-path contract-name)
  (match (pregexp-match "mukn\\/glow\\/([^#]*)#?.*" contract-name)
    ([_ path] (string-append "./" path ".glow"))
    (else (error "Bad contract name" contract-name))))

;; TODO: also accept local interaction parameters
;; TODO: accept alternative ethereum networks, etc
(define-entry-point (start-interaction . arguments)
  "Start an interaction based on an agreement"
  (def options (process-options options/interaction arguments))
  (displayln)

  (defvalues (agreement selected-role)
    (if-let (agreement-json-string (hash-get options 'agreement))
      ;; TODO: Validate that selected role matches selected identity in the agreement's participant table.
      (let* ((selected-identity (ask-identity options))
             (agreement (<-json InteractionAgreement (json<-string agreement-json-string)))
             (contract.glow (source-path (extract-contract-path (.@ agreement interaction))))
             (program (compile-contract contract.glow))
             (role-names (sort (filter identity (hash-keys (@ program interactions))) symbol<?))
             (selected-role (ask-role options role-names)))
        (values agreement selected-role))
      (nest
        (let (contract-name (get-or-ask options 'contract (λ () (ask-contract)))))
        (let (contract-path (extract-contract-path contract-name)))
        (let (contract.glow (source-path contract-path)))
        (let (program (compile-contract contract.glow)))
        (let (role-names (sort (filter identity (hash-keys (@ program interactions))) symbol<?)))

        (let (selected-identity (ask-identity options)))
        (let (selected-role (ask-role options role-names)))

        (let (contacts (load-contacts (hash-get options 'file))))
        (let (participants-table (ask-participants selected-identity (symbolify selected-role) role-names contacts)))
        (let (parameters (ask-parameters program)))
        (let (current-block-number (eth_blockNumber)))
        (let (max-initial-block (ask-max-initial-block options current-block-number)))

        ;; TODO: Validate agreement, with nice user-friendly error message.
        ;; TODO: Output agreement as a command for the other user to copy paste and automatically fill in arguments.
        (let (agreement
          {interaction: (string-append contract-name "#" (symbol->string (@ program name)))
          participants: (object<-hash participants-table)
          parameters
          glow-version: (software-identifier)
          code-digest: (digest<-file contract.glow)
          reference: {}
          options: {blockchain: "Private Ethereum Testnet"
                    escrowAmount: (void)
                    timeoutInBlocks: (* 10 (ethereum-timeout-in-blocks))
                    maxInitialBlock: max-initial-block}}))
        (begin
          (.call InteractionAgreement .validate agreement)
          (print-command agreement)
          (values agreement selected-role)))))
  (let (environment (run (symbolify selected-role) agreement))
    (displayln "Final environment:")
    ;; TODO: get run to include type t and pre-alpha-converted labels,
    ;; and output the entire thing as JSON omitting shadowed variables (rather than having conflicts)
    (for-each (match <> ([k t . v] (display-object-ln k "=> " t v)))
              (hash->list/sort environment symbol<?))))

;; UTILS
(def (flush-input port)
   (input-port-timeout-set! port 0.001)
   (let loop () (if (not (eof-object? (read-line port))) (loop)))
   (input-port-timeout-set! port +inf.0))

(def (read-line-from-console)
   (let ((port (console-port)))
     (flush-input port)
     (read-line port)))
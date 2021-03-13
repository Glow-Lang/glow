(export #t)

(import
  :gerbil/expander :gerbil/gambit/ports
  :std/format :std/generic :std/getopt :std/iter :std/misc/hash :std/misc/repr :std/misc/string :std/pregexp :std/sort :std/srfi/13 :std/sugar :std/text/json
  :clan/base :clan/cli :clan/exit :clan/hash :clan/json :clan/multicall :clan/path-config :clan/syntax
  :clan/poo/brace :clan/poo/cli :clan/poo/debug :clan/poo/object
  :clan/persist/db :clan/persist/content-addressing :clan/versioning :clan/pure/dict/symdict
  :mukn/ethereum/assets :mukn/ethereum/cli :mukn/ethereum/ethereum :mukn/ethereum/network-config
  :mukn/ethereum/types :mukn/ethereum/json-rpc :mukn/ethereum/known-addresses
  :mukn/glow/runtime/participant-runtime :mukn/glow/runtime/reify-contract-parameters
  :mukn/glow/runtime/program :mukn/glow/runtime/terminal-codes
  (only-in :mukn/glow/compiler/alpha-convert/alpha-convert init-syms)
  :mukn/glow/compiler/passes :mukn/glow/compiler/multipass :mukn/glow/compiler/syntax-context
  :mukn/glow/cli/contacts :mukn/glow/cli/identities)

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
(def (ask-address name)
  (def known-addresses
    (map
      (match <>
        ([nickname . address]
          (cons nickname [Address . address])))
      (hash->list/sort address-by-nickname string<?)))
  (def chosen-nickname (ask-option name known-addresses))
  (hash-get address-by-nickname (string-downcase chosen-nickname)))

(def (display-prompt name)
  (displayln CYAN name)
  (display (string-append "> " END)))

(def (list-applications)
  ;; List all of the applications in the search path.
  (let*
    ((all-files
      (apply append (map directory-files (glow-search-path))))
     (glow-files
       (filter
         (lambda (file)
           (string=? (path-extension file) ".glow"))
         all-files)))
    (map path-strip-extension glow-files)))

(def (ask-application)
  (let
    ((apps
       (map
        (lambda (app) (cons app app))
        (list-applications))))
    (ask-option "Choose application" apps)))

(def (ask-interaction interactions)
  (match interactions
    ([interaction]
      interaction)
    (else
      (ask-option "Choose interaction" interactions))))

(def (ask-participants selected-identity selected-role role-names contacts)
  (displayln BOLD "Assign roles" END)
  (let (participants (make-hash-table))
    (for ((role-name role-names))
      (def role-address
        (if (equal? selected-role role-name)
          (hash-get address-by-nickname selected-identity)
          (ask-address (string-append "Select address for " (symbol->string role-name)))))
      (hash-put! participants role-name role-address))
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

(def (ask-parameters program parameter-names)
  (displayln BOLD "Define parameters" END)
  (let (parameters (make-hash-table))
    (for ((name parameter-names))
      (def type (lookup-type program name))
      (def surface-name (lookup-surface-name program name))
      (def input (console-input type surface-name #f))
      (hash-put! parameters surface-name (json<- type input)))
    (displayln)
    parameters))

(def (print-command agreement)
  (displayln MAGENTA "One line command for other participants to generate the same agreement:" END)
  (display "glow start-interaction --agreement ")
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

(def (glow-search-path)
  (let ((path-str
          (with-catch
            (lambda (_) (source-path "dapps"))
            (lambda () (getenv "GLOW_PATH")))))
    (string-split path-str #\:)))

(def (find-source-file target)
  (let*
    ((choices
       (map
         (lambda (search-dir) (string-append search-dir "/" target))
         (glow-search-path)))
     (present-choices
       (filter file-exists? choices)))
    (if (null? present-choices)
      (error "Source file not found: " target)
      (car present-choices))))

(def (extract-application-source-path application-name)
  (match (pregexp-match "([^#]*)#?.*" application-name)
    ([_ path] (string-append "./" path ".glow"))
    (else (error "Bad application name" application-name))))

;; TODO: also accept local interaction parameters
;; TODO: accept alternative ethereum networks, etc
(define-entry-point (start-interaction
                     agreement: (agreement-json-string #f)
                     interaction: (interaction #f)
                     role: (role #f)
                     max-initial-block: (max-initial-block #f)
                     contacts: (contacts-file #f)
                     identities: (identities-file #f))
  (help: "Start an interaction based on an agreement"
   getopt: (make-options
            [(option 'agreement "-A" "--agreement" default: #f
                     help: "interaction parameters as JSON")
             (option 'interaction "-I" "--interaction" default: #f
                     help: "path and name of interaction")
             (option 'role "-R" "--role" default: #f
                     help: "role you want to play in the interaction")
             (option 'max-initial-block "-B" default: #f
                     help: "maximum block number the contract can begin at")]
            [(lambda (opt) (hash-remove! opt 'test))]
            [options/contacts options/identities
             options/evm-network options/database options/test options/backtrace]))
  (def options (hash (max-initial-block max-initial-block) (role role)))
  (displayln)
  (def identities (load-identities from: identities-file))
  (defvalues (agreement selected-role)
    (if agreement-json-string
      (start-interaction/with-agreement options (<-json InteractionAgreement (json<-string agreement-json-string)))
      (start-interaction/generate-agreement options contacts: contacts-file)))
  (def environment (run:terminal (symbolify selected-role) agreement))
  (displayln "Final environment:")
  ;; TODO: get run to include type t and pre-alpha-converted labels,
  ;; and output the entire thing as JSON omitting shadowed variables (rather than having conflicts)
  ;; TODO: highlight the returned value in the interaction, and have buy_sig return signature
  (for-each (match <> ([k t . v]
              (if (equal? (symbolify k) 'signature)
                (display-object-ln BOLD k END " => " t v)
                (display-object-ln k " => " t v))))
            (hash->list/sort environment symbol<?)))

;; TODO: Validate that selected role matches selected identity in the agreement's participant table.
(def (start-interaction/with-agreement options agreement)
  (let* ((selected-identity (ask-identity options))
         (interaction (.@ agreement interaction))
         (application.glow (find-source-file (extract-application-source-path interaction)))
         (program (compile-contract application.glow))
         (interaction-name (symbolify (cadr (string-split interaction #\#))))
         (interaction-info (hash-get (.@ program interactions) interaction-name))
         (role-names (sort (filter identity (hash-keys (.@ interaction-info specific-interactions))) symbol<?))
         (selected-role (ask-role options role-names)))
  (values agreement selected-role)))

(def (start-interaction/generate-agreement
      options
      contacts: contacts-file)
  (nest
    (let (application-name
            (get-or-ask options 'glow-app (λ () (ask-application)))))
    (let (application-source-path (extract-application-source-path application-name)))
    (let (application.glow (find-source-file application-source-path)))
    (let (program (compile-contract application.glow)))
    (let (interaction-name
            (get-or-ask options 'interaction (λ () (ask-interaction (hash-keys (.@ program interactions)))))))
    (let (interaction-info (hash-get (.@ program interactions) interaction-name)))
    (let (role-names
            (sort (filter identity (hash-keys (.@ interaction-info specific-interactions))) symbol<?)))

    (let (selected-identity (ask-identity options)))
    (let (selected-role (ask-role options role-names)))

    (let (contacts (load-contacts contacts-file)))
    (let (participants-table (ask-participants selected-identity (symbolify selected-role) role-names contacts)))
    (let (parameters (ask-parameters program (.@ interaction-info parameter-names))))
    (let (current-block-number (eth_blockNumber)))
    (let (max-initial-block (ask-max-initial-block options current-block-number)))

    ;; TODO: Validate agreement, with nice user-friendly error message.
    (let (agreement
      {interaction: (string-append application-name "#" (symbol->string (.@ interaction-info name)))
      participants: (object<-hash participants-table)
      parameters
      glow-version: (software-identifier)
      code-digest: (digest<-file application.glow)
      reference: {}
      options: {blockchain: (.@ (ethereum-config) name)
                escrowAmount: (void)
                timeoutInBlocks: (* 100 (ethereum-timeout-in-blocks))
                maxInitialBlock: max-initial-block}}))
    (begin
      (.call InteractionAgreement .validate agreement)
      (print-command agreement)
      (values agreement selected-role))))

;; UTILS
(def (flush-input port)
   (input-port-timeout-set! port 0.001)
   (let loop () (if (not (eof-object? (read-line port))) (loop)))
   (input-port-timeout-set! port +inf.0))

(def (read-line-from-console)
   (let ((port (console-port)))
     (flush-input port)
     (read-line port)))

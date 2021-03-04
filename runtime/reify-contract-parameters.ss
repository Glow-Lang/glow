(export run run:terminal run:special-file)
(import
  :std/format :std/iter :std/pregexp :std/misc/string :std/text/json
  :clan/debug :clan/json
  :clan/poo/object :clan/poo/mop :clan/poo/debug
  :clan/path-config :clan/pure/dict/symdict
  :mukn/ethereum/network-config :mukn/ethereum/json-rpc
  :mukn/glow/compiler/syntax-context :mukn/glow/compiler/multipass :mukn/glow/compiler/passes
  (only-in ../compiler/alpha-convert/env symbol-refer)
  ./program ./participant-runtime ./terminal-codes)

(.def io-context:terminal
  setup:
  (lambda () (assert! (input-port? (current-input-port))))
  teardown:
  void
  send-handshake:
  (lambda (handshake)
    (displayln MAGENTA "\nSend the handshake below to the other participant:" END)
    (write-json-ln (json<- AgreementHandshake handshake)))
  receive-handshake:
  (lambda ()
    (displayln MAGENTA "\nPaste below the handshake sent by the other participant:" END)
    (def handshake-json (json<-port (current-input-port)))
    (<-json AgreementHandshake handshake-json)))

;; interaction-agreement->program : InteractionAgreement -> Program
(def (interaction-agreement->program a)
  (defvalues (modpath name)
    (split-interaction-path-name (.@ a interaction)))
  (def path (glow-module-path->path modpath))
  (def compiler-output (run-passes path pass: 'project show?: #f))
  (parse-compiler-output compiler-output))

;; A TypeValuePair is a dependent pair of a type and a value of that type:
;;   (Σ (value : Type) value)

;; surface-name-environment : AlphaEnv [Hashof Symbol V] -> [Hashof Symbol V]
;; Takes an environment that uses alpha-names, produces one
;; that uses surface-names instead, with the same values
(def (surface-name-environment alpha env)
  (def new (make-hash-table-eq))
  (for ((surface-name (symdict-keys alpha)))
    (def alpha-name (symbol-refer alpha surface-name))
    (when (hash-key? env alpha-name)
      (hash-put! new surface-name (hash-get env alpha-name))))
  new)

;; program-environment-type-value-pairs :
;; Program Environment -> [Hashof Symbol TypeValuePair]
;; The symbols are alpha-names, not surface names
(def (program-environment-type-value-pairs prg env)
  (list->hash-table
   (for/collect ((ent (hash->list env)))
     (match ent
       ((cons k v)
        (cons k (cons (lookup-type prg k) v)))))))

(def (run:terminal role a)
  (run io-context:terminal role a))

(def (run:special-file role a)
  (run io-context:special-file role a))

;; run : Symbol InteractionAgreement -> [Hashof Symbol TypeValuePair]
;; Produces an environment mapping surface names to type-value-pairs
(def (run ctx role a)
  (def program (interaction-agreement->program a))
  (def runtime
    (make-Runtime role: role
                  agreement: a
                  program: program
                  io-context: ctx))
  {execute runtime}
  (printf "~a~a interaction finished~a\n" BOLD (.@ a interaction) END)
  (surface-name-environment
   (hash-get (hash-get (.@ program compiler-output) 'DebugLabelTable)
             (@ runtime current-debug-label))
   (program-environment-type-value-pairs program (@ runtime environment))))

;; --------------------------------------------------------

;; glow-module-path->path : String -> PathString
(def (glow-module-path->path s)
  (cond
   ((string-prefix? "mukn/glow/" s)
    (let (s (string-trim-prefix "mukn/glow/" s))
      (source-path (string-append s ".glow"))))
   (else
    (error 'glow-module-path->path "given:" s))))

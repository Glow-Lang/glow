(export run)
(import
  :std/format :std/iter :std/pregexp :std/misc/string :std/text/json
  :clan/debug :clan/json
  :clan/poo/object :clan/poo/mop :clan/poo/debug
  :clan/path-config
  :mukn/ethereum/network-config :mukn/ethereum/json-rpc
  :mukn/glow/compiler/syntax-context :mukn/glow/compiler/multipass :mukn/glow/compiler/passes
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
    (match (pregexp-match "^([^#]*)#([^#]*)$" (.@ a interaction))
      ([_ path name] (values path name))
      (else (error "Bad interaction name" (.@ a interaction)))))
  (def path (glow-module-path->path modpath))
  (def compiler-output (run-passes path pass: 'project show?: #f))
  (parse-compiler-output compiler-output))

;; A TypeValuePair is a dependent pair of a type and a value of that type:
;;   (Σ (value : Type) value)

;; program-environment-type-value-pairs :
;; Program Environment -> [Hashof Symbol TypeValuePair]
;; The symbols are alpha-names, not surface names
(def (program-environment-type-value-pairs prg env)
  (list->hash-table
   (for/collect ((ent (hash->list env)))
     (match ent
       ((cons k v)
        (cons k (cons (lookup-type prg k) v)))))))

;; run : Symbol InteractionAgreement -> [Hashof Symbol Any]
(def (run role a)
  (def program (interaction-agreement->program a))
  (def runtime
    (make-Runtime role: role
                  agreement: a
                  program: program
                  io-context: io-context:terminal))
  {execute runtime}
  (printf "~a finished\n" role)
  ;; TODO: change alpha-converted names to surface names for printing to the user
  ;; TODO: and return type-value pairs
  (program-environment-type-value-pairs program (@ runtime environment)))

;; --------------------------------------------------------

;; glow-module-path->path : String -> PathString
(def (glow-module-path->path s)
  (cond
   ((string-prefix? "mukn/glow/" s)
    (let (s (string-trim-prefix "mukn/glow/" s))
      (source-path (string-append s ".glow"))))
   (else
    (error 'glow-module-path->path "given:" s))))

;; monomorphic-object->hash-table : [MonomorphicPooof V] -> [Hashof Symbol V]
(def (monomorphic-object->hash-table p)
  (object-instance (force-object p)))

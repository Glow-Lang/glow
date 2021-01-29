(export run)
(import
  :std/format :std/iter :std/pregexp :std/misc/string :std/text/json
  :clan/debug :clan/json
  :clan/poo/poo :clan/poo/mop :clan/poo/debug
  :clan/path-config
  :mukn/ethereum/network-config :mukn/ethereum/json-rpc
  :mukn/glow/compiler/syntax-context :mukn/glow/compiler/multipass :mukn/glow/compiler/passes
  ./program ./participant-runtime)

(.def io-context:terminal
  setup:
  (lambda () (assert! (input-port? (current-input-port))))
  teardown:
  void
  send-handshake:
  (lambda (handshake)
    (displayln "\nPLEASE SEND THE HANDSHAKE BELOW TO THE OTHER PARTICIPANT: ---v")
    (write-json-ln (json<- AgreementHandshake handshake))
    (displayln "^___ PLEASE SEND THE HANDSHAKE ABOVE TO THE OTHER PARTICIPANT\n"))
  receive-handshake:
  (lambda ()
    (displayln "\nPLEASE PASTE BELOW THE HANDSHAKE SENT BY THE OTHER PARTICIPANT:")
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
  (@ runtime environment))

;; --------------------------------------------------------

;; glow-module-path->path : String -> PathString
(def (glow-module-path->path s)
  (cond
   ((string-prefix? "mukn/glow/" s)
    (let (s (string-trim-prefix "mukn/glow/" s))
      (source-path (string-append s ".glow"))))
   (else
    (error 'glow-module-path->path "given:" s))))

;; monomorphic-poo->hash-table : [MonomorphicPooof V] -> [Hashof Symbol V]
(def (monomorphic-poo->hash-table p)
  (poo-instance (force-poo p)))

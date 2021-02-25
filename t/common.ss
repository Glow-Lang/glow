(export #t)

(import
  :gerbil/gambit/misc
  :gerbil/expander
  :std/iter
  :clan/base :clan/exit :clan/filesystem :clan/multicall :clan/path :clan/persist/content-addressing
  :mukn/ethereum/hex
  :mukn/glow/path-config
  :mukn/glow/compiler/multipass
  :mukn/glow/compiler/passes)

;; dapps-dir
(def (dapps-dir)
  (path-simplify (path-expand "dapps" (glow-src))))

;; dapps.sexp : -> [Listof Path]
(def (dapps.sexp)
  (find-files (dapps-dir) (λ (x) (equal? (identify-layer x) 'sexp))))

;; dapps.glow : -> [Listof Path]
(def (dapps.glow)
  (find-files (dapps-dir) (λ (x) (equal? (identify-layer x) 'glow))))

(def (ppd x) (pretty-print (syntax->datum x)))

(def (ppdc x) (ppd (car x)))

(define-entry-point (process . files)
  "Fully process given files through all compiler passes"
  (for-each run-passes files))

(def (process-to-pass pass . files)
  ;; Given a pass by name, and for each specified files,
  ;; identify the language in which the file is written, by file extension,
  ;; and run all compiler passes on the file until the named pass.
  ;; Run safety checks on the results of the specified pass,
  ;; and compare these results to pre-recorded results if available;
  ;; unless the pre-recorded results exist and match, print the pass results.
  ;; Either way, print test results.
  (def pass-sym (string->symbol pass))
  (for (file files) (run-passes file pass: pass-sym)))

(register-entry-point "pass" process-to-pass
  help: "Given a pass and files, process them until the end of it")

(def (digest-command file)
  (displayln (0x<-bytes (digest<-file file))))

(register-entry-point "digest" digest-command
  help: "Create a digest of the given file")

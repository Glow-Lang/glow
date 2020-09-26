(export #t)

(import
  :gerbil/gambit/misc
  :gerbil/expander
  :std/iter
  :clan/base :clan/exit :clan/filesystem :clan/multicall :clan/path
  :mukn/glow/config/path
  :mukn/glow/compiler/multipass
  :mukn/glow/compiler/passes)

;; examples-dir
(def (examples-dir)
  (shorten-path (path-expand "examples" (glow-src))))

;; examples.sexp : -> [Listof Path]
(def (examples.sexp)
  (find-files (examples-dir) (λ (x) (equal? (identify-layer x) 'sexp))))

;; examples.glow : -> [Listof Path]
(def (examples.glow)
  (find-files (examples-dir) (λ (x) (equal? (identify-layer x) 'glow))))

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

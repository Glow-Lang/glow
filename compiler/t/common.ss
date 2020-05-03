(export #t)

(import
  :gerbil/gambit/misc
  :gerbil/expander
  :std/iter :std/misc/repr :std/sort :std/srfi/1 :std/srfi/13 :std/sugar :std/test
  :clan/utils/base :clan/utils/filesystem :clan/utils/path
  :glow/config/path :glow/compiler/multipass :glow/compiler/passes)

;; examples-dir
(def (examples-dir)
  (shorten-path (path-expand "examples" (glow-src))))

;; examples.sexp : -> [Listof Path]
(def (examples.sexp)
  (find-files (examples-dir) (λ (x) (equal? (identify-layer x) 'sexp))))

(def (test-dir? x) (equal? "t" (path-strip-directory x)))

(def (find-test-directories top)
  (map shorten-path (find-files top test-dir?)))

(def (find-test-files top)
  (sort (find-regexp-files "-test.ss" (find-test-directories top)) string<?))

(def (test-name test)
  (def module-name (path-enough (path-strip-extension (path-simplify test)) (glow-src)))
  (string-append "glow/" module-name "#" (path-strip-directory module-name)))

(def (find-file-test test)
  (import-module test #t #t)
  (eval (string->symbol (test-name test))))

(def (run-tests tests)
  (apply run-tests! (map find-file-test tests))
  (test-report-summary!)
  (eqv? 'OK (test-result)))

(def (ppd x) (pretty-print (syntax->datum x)))
(def (ppdc x) (ppd (car x)))

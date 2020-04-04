(export #t)

(import
  :gerbil/expander
  :std/iter :std/misc/repr :std/srfi/1 :std/srfi/13 :std/sugar :std/test
  :clan/utils/base :clan/utils/filesystem
  :glow/config/path :glow/compiler/multipass :glow/compiler/passes)

;; examples-dir
(def (examples-dir)
  (path-normalize (path-expand "examples" (glow-src)) 'shortest))

;; examples.sexp : -> [Listof Path]
(def (examples.sexp)
  (find-files (examples-dir) (λ (x) (equal? (identify-language x) ".sexp"))))

(def (test-dir? x) (equal? "t" (path-strip-directory x)))

(def (find-test-directories top)
  (map shorten-path (find-files top test-dir?)))

(def (find-test-files top)
  (find-regexp-files "-test.ss" (find-test-directories top)))

(def (find-file-test test)
  (import-module test #t #t)
  (def module-name (string-append "glow/" (path-strip-extension test)))
  (def test-name (string-append module-name "#" (path-strip-directory module-name)))
  (eval (string->symbol test-name)))

(def (run-tests tests)
  (apply run-tests! (map find-file-test tests))
  (test-report-summary!)
  (eqv? 'OK (test-result)))

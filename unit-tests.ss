#!/usr/bin/env gxi

(import
  :gerbil/expander
  :std/iter :std/misc/repr :std/srfi/1 :std/sugar :std/test
  :glow/compiler/syntax-context ;; important for the parsing to work (!)
  :clan/utils/filesystem
  :glow/compiler/alpha-convert/t/alpha-convert-test
  :glow/config/path :glow/compiler/passes :glow/compiler/multipass)

(current-directory (glow-src))

(def (shorten-path x) (path-normalize x 'shortest))

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

(def (bool-exit b)
  (exit (if b 0 1)))

(def (main . args)
  (bool-exit
   (match args
     ([] (main "all"))
     (["all"] (run-tests (find-test-files ".")))
     (["alpha-convert-test"] (try-alpha-convert-all))
     (["test" . files] (run-tests files))
     (["process" . files] (for-each run-passes files))
     (["pass" pass . files]
      ;; Given a pass by name, and for each specified files,
      ;; identify the language in which the file is written, by file extension,
      ;; and run all compiler passes on the file until the named pass.
      ;; Run safety checks on the results of the specified pass,
      ;; and compare these results to pre-recorded results if available;
      ;; unless the pre-recorded results exist and match, print the pass results.
      ;; Either way, print test results.
      (def pass-sym (string->symbol pass))
      (for (file files) (run-passes file pass: pass-sym))
      #t))))

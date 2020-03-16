#!/usr/bin/env gxi

(import
  :gerbil/expander
  :std/test :std/srfi/1 :std/misc/repr
  :clan/utils/base :clan/utils/filesystem
  :glow/config/path)

(current-directory (glow-src))

(def (find-test-directories top)
  (map (λ (x) (path-normalize x 'shortest))
       (find-files top (λ (x) (equal? "t" (path-strip-directory x))))))

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
     (["test" . files] (run-tests files))
     (["pass" pass . files]
      ;; TODO: given a pass by name, and for each specified files,
      ;; identify the language in which the file is written, by file extension,
      ;; and run all compiler passes on the file until the named pass.
      ;; Run safety checks on the results of the specified pass,
      ;; and compare these results to pre-recorded results if available;
      ;; unless the pre-recorded results exist and match, print the pass results.
      ;; Either way, print test results.
      (error 'not-implemented-yet #f)))))

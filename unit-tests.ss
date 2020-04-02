#!/usr/bin/env gxi

(import
  :std/test
  :clan/utils/exit
  :glow/compiler/syntax-context ;; important for the parsing to work (!)
  :glow/config/path :glow/compiler/passes :glow/compiler/multipass
  :glow/compiler/t/common
  :glow/compiler/alpha-convert/t/alpha-convert-test)

(current-directory (glow-src))

(def (main . args)
  (eval-print-exit
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
      (silent-exit)))))

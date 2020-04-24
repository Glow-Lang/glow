(import
  :std/iter :std/test
  :clan/utils/exit
  :glow/config/path
  :glow/compiler/syntax-context ;; important for the parsing to work (!)
  ;;(for-template :glow/compiler/syntax-context)
  :glow/config/path :glow/compiler/passes :glow/compiler/multipass
  :glow/compiler/t/common)

(def (main . args)
  (eval-print-exit
   (match args
     ([] (main "all"))
     (["all"] (silent-exit (run-tests (find-test-files "."))))
     (["test" . files] (silent-exit (run-tests files)))
     (["process" . files] (for-each run-passes files) (silent-exit))
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

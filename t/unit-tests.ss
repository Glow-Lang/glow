(import
  :gerbil/gambit/ports
  :std/iter
  :clan/utils/exit :clan/utils/ports
  :clan/t/test-support
  ;;:glow/compiler/syntax-context ;; important for the parsing to work (!)
  :glow/compiler/passes :glow/compiler/multipass
  :glow/t/common)

(set-current-ports-encoding-standard-unix!)

(def (main . args)
  (eval-print-exit
   (silent-exit
    (match args
      ([] (run-tests "."))
      (["meta"] (println "meta all test process pass"))
      (["all"] (run-tests "." (find-test-files ".")))
      (["test" . files] (run-tests "." files))
      (["process" . files] (for-each run-passes files) #t)
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
       #t)))))

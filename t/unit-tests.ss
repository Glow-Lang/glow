(export main)

(import
  :std/iter
  :utils/exit :utils/multicall :utils/versioning

  ;;:glow/compiler/syntax-context ;; important for the parsing to work (!)
  :glow/config/path
  :glow/compiler/passes :glow/compiler/multipass
  :glow/t/common)

(register-entry-point "process"
  (lambda files (for-each run-passes files) (silent-exit)))

(register-entry-point "pass"
  (lambda (pass . files)
    ;; Given a pass by name, and for each specified files,
    ;; identify the language in which the file is written, by file extension,
    ;; and run all compiler passes on the file until the named pass.
    ;; Run safety checks on the results of the specified pass,
    ;; and compare these results to pre-recorded results if available;
    ;; unless the pre-recorded results exist and match, print the pass results.
    ;; Either way, print test results.
    (def pass-sym (string->symbol pass))
    (for (file files) (run-passes file pass: pass-sym))
    (silent-exit)))

(register-entry-point "show-version"
  (lambda ()
    (show-version complete: #t)))

(define main call-entry-point)

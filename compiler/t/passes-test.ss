(export passes-test)

(import
  :std/format :std/misc/list :std/sugar :std/test
  :clan/base :clan/ports
  :clan/t/test-support
  :mukn/glow/compiler/multipass :mukn/glow/compiler/passes
  :mukn/glow/t/common)

(def passes-test
  (test-suite "test suite for glow/compiler/passes"
    (def (test-example dapp extension)
      (def relpath (string-append dapp extension))
      (force-current-outputs)
      (test-case (format "Running all passes on ~a" relpath)
        (run-passes relpath)))
    (for-each (cut test-example <> ".sexp") (dapps.sexp))
    (for-each (cut test-example <> ".glow") (dapps.glow))))

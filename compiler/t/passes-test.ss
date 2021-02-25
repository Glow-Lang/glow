(export passes-test)

(import
  :std/format :std/misc/list :std/sugar :std/test
  :clan/base :clan/ports
  :clan/t/test-support
  :mukn/glow/compiler/multipass :mukn/glow/compiler/passes
  :mukn/glow/t/common)

(def passes-test
  (test-suite "test suite for glow/compiler/passes"
    (def (test-example e)
      (force-current-outputs)
      (test-case (format "Running all passes on ~a" e)
        (run-passes e)))
    (for-each test-example (dapps.sexp))
    (for-each test-example (dapps.glow))))

(export passes-test)

(import
  :std/format :std/misc/list :std/sugar :std/test
  :utils/base :utils/ports
  :utils/t/test-support
  :glow/compiler/multipass :glow/compiler/passes
  :glow/t/common)

(def passes-test
  (test-suite "test suite for glow/compiler/passes"
    (def (test-example e)
      (force-current-outputs)
      (test-case (format "Running all passes on ~a" e)
        (run-passes e)))
    (for-each test-example (examples.sexp))))

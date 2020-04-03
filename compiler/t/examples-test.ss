(export #t)

(import
  :std/test
  :glow/compiler/multipass :glow/compiler/passes
  :glow/compiler/t/common)

(def examples-test
  (test-suite "examples-test"
    (test-case "all passes on all examples"
      (for-each run-passes (examples.sexp)))))

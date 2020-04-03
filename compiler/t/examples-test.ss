(export #t)

(import
  :std/format :std/iter :std/test
  :glow/compiler/multipass :glow/compiler/passes
  :glow/compiler/t/common)

(def examples-test
  (test-suite "examples-test"
    (test-case "all passes on all examples"
      (for ((e (examples.sexp)))
        (format "Running all passes on ~a~%" e)
        (run-passes e)
        (newline)))))

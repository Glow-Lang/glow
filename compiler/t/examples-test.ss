(export #t)

(import
  :std/format :std/misc/list :std/sugar :std/test
  :clan/utils/base :clan/utils/ports
  :glow/compiler/multipass :glow/compiler/passes
  :glow/compiler/t/common)

(def examples-test
  (test-suite "examples-test"
    (test-case "all passes on all examples"
      (def failures [])
      (def (test-example e)
        (force-current-outputs)
        (printf "~%Running all passes on ~a~%" e)
        (try
         (run-passes e)
         (catch (_) (push! e failures))
         (finally (newline))))
      (for-each test-example (examples.sexp))
      (unless (null? failures)
        (error "examples failed: " failures)))))

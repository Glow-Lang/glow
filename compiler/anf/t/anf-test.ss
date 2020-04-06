(export #t)

(import
  <expander-runtime>
  :gerbil/gambit/exceptions
  :gerbil/gambit/continuations
  :std/format
  :std/iter
  :std/misc/repr
  :std/test
  :clan/utils/exception
  ../../common
  ../../t/common
  ../../alpha-convert/alpha-convert
  (only-in ../../typecheck/typecheck typecheck)
  ../anf)

;; anf-display : [Listof Stmt] -> Void
(def (anf-display prog)
  (defvalues (prog2 unused-table alenv) (alpha-convert prog))
  (defvalues (prog3 unused-table3 alenv3 tyenv) (typecheck prog2 unused-table alenv))
  (defvalues (prog4 _ _ _) (anf prog3 unused-table3 alenv3 tyenv))
  (print-representation alenv) (newline)
  (write-sexps prog4))

;; test-files : (Listof Path) -> Void
(def (test-files files)
  (def progs (map read-sexp-file files))
  (for ((f files) (p progs))
    (displayln f)
    (with-catch/cont
     (lambda (e k) (display-exception-in-context e k))
     (lambda () (anf-display p)))
    (newline)))

(def anf-test
  (test-suite "anf-test"
    (test-case "testing example sexp files"
      (test-files (examples.sexp)))))

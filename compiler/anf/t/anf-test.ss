(export #t)

(import
  <expander-runtime>
  :gerbil/gambit/exceptions
  :std/format
  :std/iter
  :std/misc/repr
  :std/test
  ../../common
  ../../t/common
  ../../alpha-convert/alpha-convert
  ../anf)

;; anf-display : [Listof Stmt] -> Void
(def (anf-display prog)
  (defvalues (prog2 unused-table env) (alpha-convert prog))
  (defvalues (prog3 _ _) (anf prog2 unused-table env))
  (print-representation env) (newline)
  (write-sexps prog3))

;; test-files : (Listof Path) -> Void
(def (test-files files)
  (def progs (map read-sexp-file files))
  (for ((f files) (p progs))
    (displayln f)
    (with-catch
     (lambda (e) (display-exception e))
     (lambda () (anf-display p)))
    (newline)))

(def anf-test
  (test-suite "anf-test"
    (test-case "testing example sexp files"
      (test-files (examples.sexp)))))

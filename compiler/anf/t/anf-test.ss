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

;; anf-prog-display : [Listof Stmt] -> Void
(def (anf-prog-display prog)
  (defvalues (unused-table env prog2) (alpha-convert-prog prog))
  (defvalues (prog3) (anf-prog unused-table prog2))
  (print-representation env) (newline)
  (for ((stmt prog3))
    (printf "~y" (syntax->datum stmt))))

;; test-files : (Listof Path) -> Void
(def (test-files files)
  (def progs (map read-syntax-from-file files))
  (for ((f files) (p progs))
    (displayln f)
    (with-catch
     (lambda (e) (display-exception e))
     (lambda () (anf-prog-display p)))
    (newline)))

(def anf-test
  (test-suite "anf-test"
    (test-case "testing example sexp files"
      (test-files (examples.sexp)))))

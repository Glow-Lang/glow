(export #t)

(import
  <expander-runtime>
  :gerbil/gambit/exceptions
  :gerbil/gambit/continuations
  :std/format
  :std/iter
  :std/misc/repr
  :std/misc/string
  :std/sugar
  :std/test
  :clan/utils/exception
  :glow/compiler/common
  :glow/t/common
  :glow/compiler/alpha-convert/alpha-convert
  :glow/compiler/desugar/desugar
  :glow/compiler/anf/anf)

;; test-file : Path -> Void
(def (test-file file)
  (test-case (format "testing anf for ~a" file)
    (with-catch/cont
     (lambda (e k)
       (display-exception-in-context e k)
       (raise e))
     (lambda ()
       (def prog (read-sexp-module file))
       (defvalues (acprog unused-table alpha-env) (alpha-convert prog))
       (defvalues (desugprog) (desugar acprog unused-table))
       (defvalues (anfprog) (anf desugprog unused-table))
       (def expected-file (format "~a.anf.sexp" (string-trim-suffix ".sexp" file)))
       (ppd anfprog) (newline)
       (when (file-exists? expected-file)
         (let ((expected (read-sexp-module expected-file)))
           (assert! (stx-sexpr=? anfprog expected))))))))

(def anf-test
  (test-suite "test suite for glow/compiler/anf"
    (for-each test-file (examples.sexp))))

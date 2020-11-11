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
  :clan/exception
  :mukn/glow/compiler/common
  :mukn/glow/t/common
  :mukn/glow/compiler/alpha-convert/alpha-convert
  :mukn/glow/compiler/desugar/desugar
  :mukn/glow/compiler/typecheck/typecheck
  :mukn/glow/compiler/method-resolve/method-resolve
  :mukn/glow/compiler/anf/anf)

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
       (defvalues (tcenv tinfo) (typecheck desugprog unused-table))
       (defvalues (mereprog typetable tymetable) (method-resolve desugprog unused-table))
       (defvalues (anfprog) (anf mereprog unused-table typetable))
       (def expected-file (format "~a.anf.sexp" (string-trim-suffix ".sexp" file)))
       (ppd anfprog) (newline)
       (when (file-exists? expected-file)
         (let ((expected (read-sexp-module expected-file)))
           (assert! (stx-sexpr=? anfprog expected))))))))

(def anf-test
  (test-suite "test suite for glow/compiler/anf"
    (for-each test-file (examples.sexp))))

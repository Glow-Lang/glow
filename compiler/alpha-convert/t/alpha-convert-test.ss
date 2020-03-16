#!/usr/bin/env gxi

;; Runs alpha-convert on the `.sexp` files in `../../../examples`.

(export #t)

(import :gerbil/gambit/exceptions
        :std/format
        :std/iter
        :std/misc/repr
        :std/srfi/13
        :std/test
        :clan/utils/filesystem
        :clan/pure/dict
        <expander-runtime>
        :glow/compiler/common
        :glow/config/path
        :glow/compiler/alpha-convert/alpha-convert)

;; alpha-convert-prog-display : [Listof Stmt] -> Void
(def (alpha-convert-prog-display prog)
  (defvalues (_unused-table env prog2) (alpha-convert-prog prog))
  (print-representation env) (newline)
  (for ((stmt prog2))
    (printf "~y" (syntax->datum stmt)))
  (cond
    ((stx-deep-source=? prog prog2)
     (printf ";; ✓ source locations preserved exactly\n"))
    (else
     (printf ";; ✗ source locations not preserved\n"))))

;; examples.sexp : -> [Listof Path]
(def (examples.sexp)
  (find-files (path-normalize (path-expand "examples" (glow-src)) 'shortest)
              (cut string-suffix? ".sexp" <>)))

;; test-file : Path -> Void
(def (test-file file)
  (def prog (read-syntax-from-file file))
  (displayln file)
  (alpha-convert-prog-display prog))

(def (test-files files)
  (def progs (map read-syntax-from-file files))
  (for ((f files) (p progs))
    (displayln f)
    (with-catch
     (lambda (e) (display-exception e))
     (lambda () (alpha-convert-prog-display p)))
    (newline)))

(def alpha-convert-test
  (test-suite "test suite for glow/compiler/alpha-convert"
    (test-case "testing example sexp files"
      (test-files (examples.sexp)))))

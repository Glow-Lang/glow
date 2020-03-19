#!/usr/bin/env gxi

;; Runs alpha-convert on the `.sexp` files in `../../../examples`.

(export #t)

(import
  :std/test
  :glow/compiler/t/common
  :gerbil/gambit/exceptions
  :gerbil/gambit/continuations
  :std/format
  :std/iter
  :std/misc/list
  :std/misc/repr
  :std/misc/string
  :clan/pure/dict
  :clan/utils/exception
  <expander-runtime>
  :glow/compiler/common
  :glow/compiler/alpha-convert/alpha-convert)

;; Path -> Path
(def (sexp-alpha-version file)
  (string-append (string-trim-suffix ".sexp" file) ".alpha.sexp"))

;; alpha-convert-display : [Listof Stmt] (Or [Listof Stmt] '#f) -> Void
(def (alpha-convert-display prog maybe-alpha)
  (defvalues (prog2 _unused-table env) (alpha-convert prog))
  (prn env)
  (for ((stmt prog2))
    (printf "~y" (syntax->datum stmt)))
  (cond
    ((stx-deep-source=? prog prog2)
     (printf ";; ✓ source locations preserved exactly\n"))
    (else
     (printf ";; ✗ source locations not preserved\n")))
  (defvalues (prog3 _unused-table3 env3) (alpha-convert prog2))
  (cond
    ((and (stx-deep-source=? prog2 prog3) (stx-sexpr=? prog2 prog3))
     (printf ";; ✓ idempotent, alpha-twice = alpha-once\n"))
    (else
     (printf ";; ✗ not idempotent, alpha-twice is different\n")))
  (when maybe-alpha
    (cond ((stx-sexpr=? prog2 maybe-alpha)
           (printf ";; ✓ matches expected alpha output\n"))
          (else
           (printf ";; ✗ different from expected alpha output\n")))))

(def (try-alpha-convert-files files)
  (def files-alpha (map sexp-alpha-version files))
  (def progs (map read-sexp-file files))
  (def progs-alpha
    (map (lambda (f) (and (file-exists? f) (read-sexp-file f))) files-alpha))
  (let ((failed '()))
    (for ((f files) (p progs) (pa progs-alpha))
      (displayln f)
      (with-catch/cont
       (lambda (e k) (display-exception-in-context e k) (push! f failed))
       (lambda () (alpha-convert-display p pa)))
      (newline))
    (unless (null? failed)
      (error 'alpha-convert-failed failed))))

(def (try-alpha-convert-all)
  (try-alpha-convert-files (examples.sexp)))

(def alpha-convert-test
  (test-suite "test suite for glow/compiler/alpha-convert"
    (test-case "testing example sexp files"
      (try-alpha-convert-all))))

#!/usr/bin/env gxi

;; Runs alpha-convert on the `.sexp` files in `../../examples`.

(import :gerbil/gambit/exceptions
        :std/iter
        :std/format
        :std/misc/repr
        :clan/pure/dict
        <expander-runtime>
        "../common.ss"
        "alpha-convert.ss")

;; only-sexp-files : [Listof Path] -> [Listof Path]
(def (only-sexp-files ps)
  (filter (lambda (p) (string=? ".sexp" (path-extension p))) ps))

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

;; main
(def (main . args)
  (match args
    ([] (main "all"))
    (["all"]
     (def names (only-sexp-files (directory-files "../../examples")))
     (def files (map (lambda (p) (path-expand p "../../examples")) names))
     (if (null? files)
         (displayln "nothing to build")
         (apply main files)))
    ([file]
     (def prog (read-syntax-from-file file))
     (displayln file)
     (alpha-convert-prog-display prog))
    (files
     (def progs (map read-syntax-from-file files))
     (for ((f files) (p progs))
       (displayln f)
       (with-catch
        (lambda (e) (display-exception e))
        (lambda () (alpha-convert-prog-display p)))
       (newline)))))


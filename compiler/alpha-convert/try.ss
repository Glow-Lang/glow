#!/usr/bin/env gxi

;; Runs alpha-convert on the `.sexp` files in `../../examples`.

(import :gerbil/gambit/exceptions
        :std/iter
        :std/format
        :std/misc/repr
        :std/srfi/13
        :clan/pure/dict
        <expander-runtime>
        "../common.ss"
        "alpha-convert.ss")

;; only-sexp-files : [Listof Path] -> [Listof Path]
(def (only-sexp-files ps) (filter source-sexp-file? ps))

;; source-sexp-file? : Path -> Bool
(def (source-sexp-file? p)
  (and (string=? ".sexp" (path-extension p)) (not (alpha-sexp-file? p))))

;; alpha-sexp-file? : Path -> Bool
(def (alpha-sexp-file? p) (string-suffix? ".alpha.sexp" p))

;; sexp-alpha-version : Path -> Bool
(def (sexp-alpha-version p)
  (def n (string-length p))
  (unless (and (<= 5 n) (string=? ".sexp" (substring p (- n 5) n)))
    (error 'sexp-alpha-version "expected a `.sexp` path"))
  (string-append (substring p 0 (- n 5)) ".alpha.sexp"))

;; alpha-convert-prog-display : [Listof Stmt] (U #f [Listof Stmt]) -> Void
(def (alpha-convert-prog-display prog maybe-alpha)
  (defvalues (_unused-table env prog2) (alpha-convert-prog prog))
  (print-representation env) (newline)
  (for ((stmt prog2))
    (printf "~y" (syntax->datum stmt)))
  (cond
    ((stx-deep-source=? prog prog2)
     (printf ";; ✓ source locations preserved exactly\n"))
    (else
     (printf ";; ✗ source locations not preserved\n")))
  (when maybe-alpha
    (cond ((stx-sexpr=? prog2 maybe-alpha)
           (printf ";; ✓ matches expected alpha output\n"))
          (else
           (printf ";; ✗ different from expected alpha output\n")))))

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
     (def file-alpha (sexp-alpha-version file))
     (def prog (read-syntax-from-file file))
     (def prog-alpha (and (file-exists? file-alpha) (read-syntax-from-file file-alpha)))
     (displayln file)
     (alpha-convert-prog-display prog prog-alpha))
    (files
     (def files-alpha (map sexp-alpha-version files))
     (def progs (map read-syntax-from-file files))
     (def progs-alpha
       (map (lambda (f) (and (file-exists? f) (read-syntax-from-file f))) files-alpha))
     (for ((f files) (p progs) (pa progs-alpha))
       (displayln f)
       (with-catch
        (lambda (e) (display-exception e))
        (lambda () (alpha-convert-prog-display p pa)))
       (newline)))))


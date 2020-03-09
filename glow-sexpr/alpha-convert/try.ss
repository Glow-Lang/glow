#!/usr/bin/env gxi

;; Runs alpha-convert on the `.sexp` files in `../../examples`.

(import :gerbil/gambit/exceptions
        :std/iter
        :std/format
        :std/misc/repr
        :clan/pure/dict
        <expander-runtime>
        "alpha-convert.ss")

;; only-sexp-files : [Listof Path] -> [Listof Path]
(def (only-sexp-files ps)
  (filter (lambda (p) (string=? ".sexp" (path-extension p))) ps))

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
     (defvalues (env prog2) (alpha-convert-prog prog))
     (print-representation env) (newline)
     (for ((stmt prog2))
       (printf "~y" (syntax->datum stmt)))
     (newline))
    (files
     (def progs (map read-syntax-from-file files))
     (for ((f files) (p progs))
       (displayln f)
       (with-catch
        (lambda (e) (display-exception e))
        (lambda ()
          (defvalues (env prog2) (alpha-convert-prog p))
          (print-representation env) (newline)
          (for ((stmt prog2))
            (printf "~y" (syntax->datum stmt)))
          (newline)))
       (newline)))))


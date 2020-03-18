#!/usr/bin/env gxi

;; Runs alpha-convert on the `.sexp` files in `../../examples`.
(import
  :glow/compiler/alpha-convert/t/alpha-convert-test)

;; main
(def (main . args)
  (match args
    ([] (try-alpha-convert-all))
    (["all"] (try-alpha-convert-all))
    (files (try-alpha-convert-files files))))

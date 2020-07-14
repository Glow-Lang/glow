(export #t)

(import
  :gerbil/gambit/misc
  :gerbil/expander
  :utils/base :utils/filesystem :utils/path
  :glow/config/path :glow/compiler/multipass)

;; examples-dir
(def (examples-dir)
  (shorten-path (path-expand "examples" (glow-src))))

;; examples.sexp : -> [Listof Path]
(def (examples.sexp)
  (find-files (examples-dir) (λ (x) (equal? (identify-layer x) 'sexp))))

(def (ppd x) (pretty-print (syntax->datum x)))
(def (ppdc x) (ppd (car x)))

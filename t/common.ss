(export #t)

(import
  :gerbil/gambit/misc
  :gerbil/expander
  :std/iter :std/misc/repr :std/sort :std/srfi/1 :std/srfi/13 :std/sugar :std/test
  :clan/utils/base :clan/utils/filesystem :clan/utils/path
  :glow/config/path :glow/compiler/multipass :glow/compiler/passes)

;; examples-dir
(def (examples-dir)
  (shorten-path (path-expand "examples" (glow-src))))

;; examples.sexp : -> [Listof Path]
(def (examples.sexp)
  (find-files (examples-dir) (λ (x) (equal? (identify-layer x) 'sexp))))

(def (ppd x) (pretty-print (syntax->datum x)))
(def (ppdc x) (ppd (car x)))

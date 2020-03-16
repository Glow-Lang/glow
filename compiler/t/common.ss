(export #t)

(import
  :std/srfi/13
  :clan/utils/filesystem
  :glow/config/path)

;; examples.sexp : -> [Listof Path]
(def (examples.sexp)
  (find-files (path-normalize (path-expand "examples" (glow-src)) 'shortest)
              (cut string-suffix? ".sexp" <>)))

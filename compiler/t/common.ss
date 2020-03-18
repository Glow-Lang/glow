(export #t)

(import
  :std/srfi/13
  :clan/utils/filesystem
  :glow/config/path)

;; examples.sexp : -> [Listof Path]
(def (examples.sexp)
  (find-files (path-normalize (path-expand "examples" (glow-src)) 'shortest)
              (cut string-suffix? ".sexp" <>)))



;;; To test whether the reader did the right thing with the λ's
(export head-lam?)

(import <expander-runtime>
        (for-template :gerbil/core))

(def (head-lam? stx)
  (syntax-case stx (λ)
    ((λ . _) #t)
    (_ #f)))

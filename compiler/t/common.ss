(export #t)

(import
  :std/srfi/13
  :clan/utils/base :clan/utils/filesystem
  :glow/config/path :glow/compiler/multipass :glow/compiler/passes)

;; examples-dir
(def (examples-dir)
  (path-normalize (path-expand "examples" (glow-src)) 'shortest))

;; examples.sexp : -> [Listof Path]
(def (examples.sexp)
  (find-files (examples-dir) (λ (x) (equal? (identify-language x) ".sexp"))))

;;; To test whether the reader did the right thing with the λ's
(export head-lam?)

(import <expander-runtime>
        (for-template :gerbil/core))

(def (head-lam? stx)
  (syntax-case stx (λ)
    ((λ . _) #t)
    (_ #f)))

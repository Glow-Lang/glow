#!/usr/bin/env gxi

;; Runs project on the `.sexp` files in `../../../dapps`.

(export #t)

(import
  :std/test
  :mukn/glow/t/common
  :gerbil/gambit/exceptions
  :gerbil/gambit/continuations
  :std/format
  :std/iter
  :std/pregexp
  :std/misc/list
  :std/misc/repr
  :std/misc/string
  :clan/pure/dict
  :clan/exception
  <expander-runtime>
  :mukn/glow/compiler/syntax-context
  :mukn/glow/compiler/common
  :mukn/glow/compiler/multipass
  :mukn/glow/compiler/passes
  :mukn/glow/compiler/project/project)

;; Path -> Path
(def (sexp-project-version file)
  (string-append (string-trim-suffix ".sexp" file) ".project.sexp"))

;; Give the name of a DApp, return #t on success, can return #f or raise an exception on failure
;; project-display : DappString -> Bool
(def (project-display dapp-name)
  ;; state : [Hashof LayerName Any]
  (def state (run-passes (string-append dapp-name ".glow") pass: 'checkpoint-liveness show?: #f))
  (def modstx (hash-ref state 'checkpointify.sexp))
  (def unused (hash-ref state 'Unused))
  (def cpit (hash-ref state 'cpitable2.sexp))
  (def scmstx (project modstx unused cpit))
  (printf "~y" (syntax->datum scmstx))
  #t)

;; try-project-files : [Listof PathString] -> Void
(def (try-project-files dapps)
  ;; MUTABLE var failed
  (let ((failed '()))
    (for ((a dapps))
      (displayln a)
      (with-catch/cont
       (lambda (e k) (display-exception-in-context e k) (push! a failed))
       (lambda () (unless (project-display a) (push! a failed))))
      (newline))
    (unless (null? failed)
      (error 'project-failed failed))))

;; try-project-all : -> Void
(def (try-project-all)
  (try-project-files (dapps.glow)))

(def project-test
  (test-suite "test suite for glow/compiler/project/project"
    (test-case "testing example glow files"
      ;(try-project-all)
      (try-project-files ["buy_sig"])
      (void))))

(def (main . args)
  (run-test-suite! project-test))

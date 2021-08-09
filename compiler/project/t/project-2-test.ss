#!/usr/bin/env gxi

;; Runs project-2 on the `.sexp` files in `../../../dapps`.

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
  :mukn/glow/compiler/project/project
  :mukn/glow/compiler/project/project-2)

;; Path -> Path
(def (sexp-project-2-version file)
  (string-append (string-trim-suffix ".sexp" file) ".project-2.sexp"))

;; project-2-display : PathString -> Bool
;; Produces #t on success, can return #f or raise an exception on failure
(def (project-2-display dapp-name)
  ;; state : [Hashof LayerName Any]
  (def state (run-passes (string-append dapp-name ".glow") pass: 'checkpoint-liveness show?: #f))
  (def modstx (hash-ref state 'checkpointify.sexp))
  (def unused (hash-ref state 'Unused))
  (def tytbl (hash-ref state 'typetable.sexp))
  (def tymetbl (hash-ref state 'tymetable.sexp))
  (def mebatbl (hash-ref state 'mebatable.sexp))
  (def cpit (hash-ref state 'cpitable2.sexp))
  (def projs (project modstx unused cpit))
  (def scmstx (project-2 projs unused tytbl tymetbl mebatbl cpit))
  (for ((s scmstx))
    (printf "~y" (syntax->datum s)))
  #t)

;; try-project-2-files : [Listof PathString] -> Void
(def (try-project-2-files files)
  ;; MUTABLE var failed
  (let ((failed '()))
    (for ((f files))
      (displayln f)
      (with-catch/cont
       (lambda (e k) (display-exception-in-context e k) (push! f failed))
       (lambda () (unless (project-2-display f) (push! f failed))))
      (newline))
    (unless (null? failed)
      (error 'project-2-failed failed))))

;; try-project-2-all : -> Void
(def (try-project-2-all)
  (try-project-2-files (dapps.glow)))

(def project-2-test
  (test-suite "test suite for glow/compiler/project"
    (test-case "testing example glow files"
      (try-project-2-all)
      ; (try-project-2-files ["closing"])
      (void))))

(def (main . args)
  (run-test-suite! project-2-test))

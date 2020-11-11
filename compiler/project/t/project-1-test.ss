#!/usr/bin/env gxi

;; Runs project-1 on the `.sexp` files in `../../../examples`.

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
  :mukn/glow/compiler/project/project-1)

;; Path -> Path
(def (sexp-project-1-version file)
  (string-append (string-trim-suffix ".sexp" file) ".project-1.sexp"))

;; project-1-display : PathString -> Bool
;; Produces #t on success, can return #f or raise an exception on failure
(def (project-1-display file)
  ;; state : [Hashof LayerName Any]
  (def state (run-passes file pass: 'checkpoint-liveness show?: #f))
  (def modstx (hash-ref state 'checkpointify.sexp))
  (def unused (hash-ref state 'Unused))
  (def tytbl (hash-ref state 'typetable.sexp))
  (def tymetbl (hash-ref state 'tymetable.sexp))
  (def cpit (hash-ref state 'cpitable2.sexp))
  (def projs (project modstx unused cpit))
  (def scmstx (project-1 projs unused tytbl tymetbl cpit))
  (for ((s scmstx))
    (printf "~y" (syntax->datum s)))
  #t)

;; try-project-1-files : [Listof PathString] -> Void
(def (try-project-1-files files)
  ;; MUTABLE var failed
  (let ((failed '()))
    (for ((f files))
      (displayln f)
      (with-catch/cont
       (lambda (e k) (display-exception-in-context e k) (push! f failed))
       (lambda () (unless (project-1-display f) (push! f failed))))
      (newline))
    (unless (null? failed)
      (error 'project-1-failed failed))))

;; try-project-1-all : -> Void
(def (try-project-1-all)
  (try-project-1-files (examples.glow)))

(def project-1-test
  (test-suite "test suite for glow/compiler/project"
    (test-case "testing example glow files"
      ;(try-project-1-all)
      (try-project-1-files
        (for/collect
          ((s (examples.glow)
              when (or (pregexp-match "buy_sig.glow" s))))
          s))
      (void))))

(def (main . args)
  (run-test-suite! project-1-test))

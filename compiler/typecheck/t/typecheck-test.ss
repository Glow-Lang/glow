#!/usr/bin/env gxi

;; Runs the typchecker on the `.sexp` files in `../../examples`.

(export #t)

(import :std/test
        :gerbil/gambit/exceptions
        :gerbil/gambit/continuations
        :std/iter
        :std/format
        :std/misc/list
        :std/misc/repr
        :clan/pure/dict
        :clan/utils/exception
        :glow/compiler/t/common
        :glow/compiler/syntax-context
        (for-template :glow/compiler/syntax-context)
        (except-in :glow/compiler/alpha-convert/alpha-convert env-put/env not-bound-as-ctor? bound-as-ctor?)
        :glow/compiler/typecheck/typecheck)

;; tc-prog/list : [Listof StmtStx] -> [Assqof Symbol EnvEntry]
(def (tc-prog/list path)
  (symdict->list (tc-prog path)))

;; only-sexp-files : [Listof Path] -> [Listof Path]
(def (only-sexp-files ps)
  (filter (lambda (p) (string=? ".sexp" (path-extension p))) ps))

;; print-env : Env -> Void
(def (print-env env)
  (for ((x (symdict-keys env)))
    (def e (symdict-ref env x))
    (match e
      ((entry:type #f [] b)
       (printf "type ~s = ~y" x (type->sexpr b)))
      ((entry:type #f as b)
       (printf "type ~s~s = ~y" x as (type->sexpr b)))
      ((entry:unknown #f)
       (printf "unknown ~s\n" x))
      ((entry:known #f (typing-scheme me t))
       (unless (symdict-empty? me)
         (printf "constraints ")
         (print-representation me))
       (printf "val ~s : ~y" x (type->sexpr t)))
      ((entry:ctor #f (typing-scheme me t))
       (unless (symdict-empty? me)
         (printf "constraints ")
         (print-representation me))
       (printf "constructor ~s : ~y" x (type->sexpr t))))))

;; typecheck-display : [Listof Stmt] -> Bool
;; Produces #t on success, can return #f or raise an exception on failure
;; TODO: check against expected types of top-level defined identifiers
(def (typecheck-display prog)
  (defvalues (prog2 unused-table alenv) (alpha-convert prog))
  (defvalues (prog3 unused-table3 alenv3 tyenv) (typecheck prog2 unused-table alenv))
  (print-env tyenv)
  #t)

;; try-typecheck-files : [Listof PathString] -> Void
(def (try-typecheck-files files)
  (def progs (map read-sexp-file files))
  ;; TODO: read expected types of type-level defined identifiers from a file,
  ;;       and check that the actual types match
  ;; MUTABLE var failed
  (let ((failed '()))
    (for ((f files) (p progs))
      (displayln f)
      (with-catch/cont
       (lambda (e k) (display-exception-in-context e k) (push! f failed))
       (lambda () (unless (typecheck-display p) (push! f failed))))
      (newline))
    (unless (null? failed)
      (error 'alpha-convert-failed failed))))

;; try-typecheck-all : -> Void
(def (try-typecheck-all)
  (try-typecheck-files (examples.sexp)))

;; typecheck-test : TestSuite
(def typecheck-test
  (test-suite "test suite for glow/compiler/typecheck"
    (test-case "testing example sexp files"
      (try-typecheck-all))))

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
        :std/misc/string
        :clan/pure/dict
        :clan/utils/exception
        :glow/compiler/t/common
        :glow/compiler/syntax-context
        (for-template :glow/compiler/syntax-context)
        (except-in :glow/compiler/alpha-convert/alpha-convert env-put/env not-bound-as-ctor? bound-as-ctor?)
        :glow/compiler/typecheck/typecheck)

;; Path -> Path
(def (sexp-typedecl-version file)
  (string-append (string-trim-suffix ".sexp" file) ".typedecl.sexp"))

;; env->sexpr : Env -> Sexpr
(def (env->sexpr env)
  (for/collect ((x (symdict-keys env)))
    (def e (symdict-ref env x))
    (match e
      ((entry:type #f [] b)
       `(type ,x = ,(type->sexpr b)))
      ((entry:type #f as b)
       `(type (,x ,@(map (lambda (a) `',a) as)) = ,(type->sexpr b)))
      ((entry:unknown #f)
       `(unknown ,x))
      ((entry:known #f (typing-scheme me t))
       (unless (symdict-empty? me)
         (printf "constraints ")
         (print-representation me))
       `(val ,x : ,(type->sexpr t)))
      ((entry:ctor #f (typing-scheme me t))
       (unless (symdict-empty? me)
         (printf "constraints ")
         (print-representation me))
       `(constructor ,x : ,(type->sexpr t))))))

;; print-env-sexpr : Sexpr -> Void
(def (print-env-sexpr s)
  (for ((e s)) (printf "~y" e)))

;; typecheck-display : [Listof Stmt] (Or Sexpr '#f) -> Bool
;; Produces #t on success, can return #f or raise an exception on failure
;; TODO: check against expected types of top-level defined identifiers
(def (typecheck-display prog expected-env)
  (defvalues (prog2 unused-table alenv) (alpha-convert prog))
  (defvalues (prog3 unused-table3 alenv3 tyenv) (typecheck prog2 unused-table alenv))
  (def env-sexpr (env->sexpr tyenv))
  (def expected-env-sexpr (and expected-env (syntax->datum expected-env)))
  (print-env-sexpr env-sexpr)
  (cond
    ((not expected-env) #t)
    ((equal? env-sexpr expected-env-sexpr)
     (printf ";; ✓ matches expected types\n")
     #t)
    (else
     (printf ";; ✗ different from expected types\n")
     (printf "expected: ~y" expected-env-sexpr)
     #f)))

;; try-typecheck-files : [Listof PathString] -> Void
(def (try-typecheck-files files)
  (def decl-files (map sexp-typedecl-version files))
  (def progs (map read-sexp-file files))
  (def decls
    (map (lambda (f) (and (file-exists? f) (read-sexp-file f))) decl-files))
  ;; MUTABLE var failed
  (let ((failed '()))
    (for ((f files) (p progs) (d decls))
      (displayln f)
      (with-catch/cont
       (lambda (e k) (display-exception-in-context e k) (push! f failed))
       (lambda () (unless (typecheck-display p d) (push! f failed))))
      (newline))
    (unless (null? failed)
      (error 'typecheck-failed failed))))

;; try-typecheck-all : -> Void
(def (try-typecheck-all)
  (try-typecheck-files (examples.sexp)))

;; typecheck-test : TestSuite
(def typecheck-test
  (test-suite "test suite for glow/compiler/typecheck"
    (test-case "testing example sexp files"
      (try-typecheck-all))))

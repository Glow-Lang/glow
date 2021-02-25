#!/usr/bin/env gxi

;; Runs the typchecker on the `.sexp` files in `../../dapps`.

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
        :clan/exception
        :mukn/glow/t/common
        :mukn/glow/compiler/syntax-context
        :mukn/glow/compiler/multipass
        (for-template :mukn/glow/compiler/syntax-context)
        :mukn/glow/compiler/alpha-convert/alpha-convert
        :mukn/glow/compiler/desugar/desugar
        :mukn/glow/compiler/typecheck/typecheck)

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

;; typecheck-display : [Listof Stmt] (Or TypeEnv '#f) -> Bool
;; Produces #t on success, can return #f or raise an exception on failure
;; TODO: check against expected types of top-level defined identifiers
(def (typecheck-display prog expected-env expect-failure?)
  (defvalues (prog2 unused-table albatable dlbtbl alenv) (alpha-convert prog))
  (def prog3 (desugar prog2 unused-table))
  (defvalues (tyenv tyinfotbl) (typecheck prog3 unused-table))
  (parameterize ((current-type-info-table tyinfotbl))
    (write-type-env tyenv)
    (cond
      ((not expected-env) #t)
      ((type-env=? tyenv expected-env)
       (printf ";; ✓ matches expected types\n")
       (if expect-failure?
         (begin
           (printf ";; ... but expected failure!\n")
           #f)
         #t))
      (else
       (printf ";; ✗ different from expected types\n")
       (if expect-failure?
         (begin
           (printf ";; ... but expected failure!\n")
           #t)
         (begin
           (printf "expected:\n")
           (write-type-env expected-env)
           #f))))))

;; try-typecheck-files : [Listof PathString] -> Void
(def (try-typecheck-files files)
  (def decl-files (map sexp-typedecl-version files))
  (def progs (map read-sexp-module files))
  (def decls
    (map (lambda (f) (and (file-exists? f) (read-type-env-file f))) decl-files))
  (def known-fails (map known-failure? decl-files))
  ;; MUTABLE var failed
  (let ((failed '()))
    (for ((f files) (p progs) (d decls) (kf known-fails))
      (displayln f)
      (with-catch/cont
       (lambda (e k) (display-exception-in-context e k) (push! f failed))
       (lambda () (unless (typecheck-display p d kf) (push! f failed))))
      (newline))
    (unless (null? failed)
      (error 'typecheck-failed failed))))

;; try-typecheck-all : -> Void
(def (try-typecheck-all)
  (try-typecheck-files (dapps.sexp)))

;; typecheck-test : TestSuite
(def typecheck-test
  (test-suite "test suite for glow/compiler/typecheck"
    (test-case "testing example sexp files"
      (try-typecheck-all))))

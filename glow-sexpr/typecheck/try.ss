#!/usr/bin/env gxi

;; Runs the typchecker on the `.sexp` files in `../examples`.

(import :std/iter
        :std/misc/repr
        :clan/pure/dict
        "typecheck.ss")

;; init-env : Env
(def init-env
  (list->symdict
   [(cons 'int (entry:type [] type:int))
    (cons 'bool (entry:type [] type:bool))
    (cons 'bytes (entry:type [] type:bytes))
    (cons 'not (entry:known (typing-scheme empty-symdict (type:arrow [type:bool] type:bool))))
    (cons '< (entry:known (typing-scheme empty-symdict (type:arrow [type:int type:int] type:bool))))
    (cons '+ (entry:known (typing-scheme empty-symdict (type:arrow [type:int type:int] type:int))))
    (cons 'sqr (entry:known (typing-scheme empty-symdict (type:arrow [type:int] type:int))))
    (cons 'sqrt (entry:known (typing-scheme empty-symdict (type:arrow [type:int] type:int))))
    ;; TODO: make polymorphic
    (cons 'member (entry:known (typing-scheme empty-symdict (type:arrow [type:int (type:listof type:int)] type:bool))))]))

;; tc-prog : [Listof StmtStx] -> Env
(def (tc-prog stmts)
  (for/fold (env init-env) (stmt stmts)
    (tc-stmt env stmt)))

;; tc-prog/list : [Listof StmtStx] -> [Assqof Symbol EnvEntry]
(def (tc-prog/list path)
  (symdict->list (tc-prog path)))

;; only-sexp-files : [Listof Path] -> [Listof Path]
(def (only-sexp-files ps)
  (filter (lambda (p) (string=? ".sexp" (path-extension p))) ps))

;; main
(def (main . args)
  (match args
    ([] (main "all"))
    (["all"]
     (def names (only-sexp-files (directory-files "../../examples")))
     (def files (map (lambda (p) (path-expand p "../../examples")) names))
     (if (null? files)
         (displayln "nothing to build")
         (apply main files)))
    ([file]
     (def prog (read-syntax-from-file file))
     (displayln file)
     (print-representation (tc-prog/list prog))
     (newline))
    (files
     (def progs (map read-syntax-from-file files))
     (for ((f files) (p progs))
       (displayln f)
       (with-catch
        (lambda (e) (display-exception e))
        (lambda ()
          (print-representation (tc-prog/list p))
          (newline)))
       (newline)))))


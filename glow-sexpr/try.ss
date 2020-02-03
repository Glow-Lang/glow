#!/usr/bin/env gxi

;; Runs the typchecker on the `.sexp` files in `../examples`.

(import :std/iter
        :clan/pure/dict
        "typecheck.ss")

;; init-env : Env
(def init-env
  (list->symdict
   [(cons 'int (entry:type [] type:int))
    (cons 'bool (entry:type [] type:bool))
    (cons 'bytestr (entry:type [] type:bytestr))
    (cons 'not (entry:fun [] [type:bool] type:bool))
    (cons '< (entry:fun [] [type:int type:int] type:bool))
    (cons '+ (entry:fun [] [type:int type:int] type:int))
    (cons 'sqr (entry:fun [] [type:int] type:int))
    (cons 'sqrt (entry:fun [] [type:int] type:int))
    ;; TODO: make polymorphic
    (cons 'member (entry:fun [] [type:int (type:listof type:int)] type:bool))]))

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
(def files
  (map (lambda (p) (path-expand p "../examples"))
       (only-sexp-files (directory-files "../examples"))))
(def progs
  (map read-syntax-from-file files))
(map tc-prog/list progs)

(export #t)

(import :clan/debug
        :std/sugar
        (for-template :mukn/glow/compiler/syntax-context)
        :mukn/glow/compiler/common)

;; Recursively accumulate schemata from a statement.
(def (schema-stmt stx)
  (syntax-case stx (def input @make-interaction @record @list participants assets)
    ;; Named user input.
    ((def var (input ty tag))
     (list (hash (var (syntax->datum #'var))
                 (type (syntax->datum #'ty))
                 (tag (syntax->datum #'tag)))))

    ;; The main interaction function.
    ((def interaction (@make-interaction
                       ((@record (participants (@list p ...))
                                 (assets (@list a ...))))
                       params
                       (from to)
                       . body))
     (cons* (hash (interaction (syntax->datum #'interaction))
                  (participants (syntax->datum #'(p ...)))
                  (assets (syntax->datum #'(a ...)))
                  (params (syntax->datum #'params)))
            (schema-stmt #'body)))

    ;; Descend into non-trivial sub-statements.
    (_ (if (trivial-expr? stx)
           []
           (stx-foldr append [] (stx-map schema-stmt stx))))))

;; Accumulate schemata from a list of statements.
(def (schema-stmts stmts)
  (match stmts
    ([] [])
    ([x] (schema-stmt x))
    ([x . xs] (append (schema-stmt x)
                      (schema-stmts xs)))))

;; Accumulate and return schemata from a module.
(def (schema module)
  (syntax-case module (@module)
    ((@module stmts ...)
     (schema-stmts (syntax->list #'(stmts ...))))))

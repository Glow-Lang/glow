(export #t)

(import :clan/debug
        :std/sugar
        (for-template :mukn/glow/compiler/syntax-context)
        :mukn/glow/compiler/common)

;; Accumulate (in reverse order) schemata from an expression.
(def (schema-expr stx)
  (if (trivial-expr? stx)
      []
      (stx-foldl append [] (stx-map schema-stmt stx))))

;; Accumulate (in reverse order) schemata from a single statement.
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
            (schema-expr #'body)))

    ;; Default: descend into expression.
    (expr (schema-expr stx))))

;; Accumulate (in reverse order) schemata from a list of statements.
(def (schema-stmts stmts)
  (match stmts
    ([] [])
    ([x] (schema-stmt x))
    ([x . xs] (append (schema-stmt x)
                      (schema-stmts xs)))))

;; Accumulate and return (in forward order) schemata from a module.
(def (schema module)
  (syntax-case module (@module)
    ((@module stmts ...)
     (reverse (schema-stmts (syntax->list #'(stmts ...)))))))

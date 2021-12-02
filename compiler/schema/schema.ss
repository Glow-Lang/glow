(export #t)

(import :clan/debug
        (for-template :mukn/glow/compiler/syntax-context)
        :mukn/glow/compiler/common)

;; Accumulate (in reverse order) schemata from an expression.
(def (schema-expr stx)
  (if (trivial-expr? stx)
      []
      (stx-foldl append [] (stx-map schema-stmt stx))))

;; Accumulate (in reverse order) schemata from a single statement.
(def (schema-stmt stx)
  (syntax-case stx (def input)
    ((def var (input ty prompt)) (list #'(var ty prompt)))
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

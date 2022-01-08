(export #t)

(import :clan/debug
        :std/sugar
        (for-template :mukn/glow/compiler/syntax-context)
        :mukn/glow/compiler/common)

;; Recursively accumulate schemata from a statement.
(def (schema-stmt stx albatable)
  (syntax-case stx (def input @make-interaction @record @list participants assets)
    ;; Named user input.
    ((def var (input ty tag))
     (list (hash (var (hash-get albatable (syntax->datum #'var)))
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
                  (params (map (cut hash-get albatable <>)
                               (syntax->datum #'params))))
            (schema-stmt #'body albatable)))

    ;; Descend into non-trivial sub-statements.
    (_ (if (trivial-expr? stx)
           []
           (stx-foldr append [] (stx-map (cut schema-stmt <> albatable) stx))))))

;; Accumulate schemata from a list of statements.
(def (schema-stmts stmts albatable)
  (match stmts
    ([] [])
    ([x] (schema-stmt x albatable))
    ([x . xs] (append (schema-stmt x albatable)
                      (schema-stmts xs albatable)))))

;; Accumulate and return schemata from a module.
;; Also take the alpha-back-table, which lets us
;; un-α-rename parameters.
(def (schema module albatable)
  (syntax-case module (@module)
    ((@module stmts ...)
     (schema-stmts (syntax->list #'(stmts ...)) albatable))))

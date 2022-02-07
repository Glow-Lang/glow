(export #t)

(import :clan/debug
        :std/sugar
        (for-template :mukn/glow/compiler/syntax-context)
        ../common
        ../typecheck/type)

(def current-participant (make-parameter #f))

;; TODO: merge with lookup-type in program.ss
(def (lookup-type typetable variable-name)
  (def (type-methods t)
    (match t
      ((type:name 'Bool) 'Bool)
      ((type:name 'Digest) 'Digest)
      ((type:name 'Participant) 'Address)
      ((type:name 'Int) 'UInt256)
      ((type:name-subtype 'Nat _) 'UInt256)
      ((type:name sym) sym)
      ((type:name-subtype sym _) sym)
      ((type:tuple ts) (map type-methods ts))))
  (def t (hash-get typetable variable-name))
  (type-methods t))

;; Recursively accumulate schemata from a statement.
(def (schema-stmt stx albatable typetable)
  (syntax-case stx (def input @make-interaction @record @list participants assets)
    ;; Interaction function.
    ((def interaction (@make-interaction
                       ((@record (participants (@list p ...))
                                 (assets (@list a ...))))
                       params
                       (from to)
                       . body))
     (cons* (hash (interaction (syntax->datum #'interaction))
                  (participants (syntax->datum #'(p ...)))
                  (assets (syntax->datum #'(a ...)))
                  (params (map (lambda (param)
                                 (hash (name (hash-get albatable param))
                                       (type (lookup-type typetable param))))
                               (syntax->datum #'params))))
            (schema-stmt #'body albatable typetable)))

    ;; Named user input.
    ((def var (input ty tag))
     (list (hash (var (hash-get albatable (syntax->datum #'var)))
                 (type (lookup-type typetable (syntax->datum #'var)))
                 (tag (syntax->datum #'tag))
                 (participant (current-participant)))))

    ;; Change of participant.
    ((set-participant p)
     (current-participant (syntax->datum #'p))
     [])

    ;; Descend into non-trivial sub-statements.
    (_ (if (trivial-expr? stx)
           []
           (stx-foldr append [] (stx-map (cut schema-stmt <> albatable typetable) stx))))))

;; Accumulate schemata from a list of statements.
(def (schema-stmts stmts albatable typetable)
  (match stmts
    ([] [])
    ([x] (schema-stmt x albatable typetable))
    ([x . xs] (append (schema-stmt x albatable typetable)
                      (schema-stmts xs albatable typetable)))))

;; Accumulate and return schemata from a module.
;; Takes the the alpha-back-table, which lets us un-α-rename variables,
;; and the type-table, which gives us derived types for variables.
(def (schema module albatable typetable)
  (parameterize ((current-participant current-participant))
    (syntax-case module (@module)
      ((@module stmts ...)
       (schema-stmts (syntax->list #'(stmts ...)) albatable typetable)))))

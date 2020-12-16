(export #t)

(import
  <expander-runtime>
  ../common.ss
  ./symbolnat)

;; Manage the generation of fresh symbols when alpha-converting some code
;; and/or further expanding macros or introducing new temporary bindings in further compiler passes

;; An UnusedTable is a [Hashof Symbol UnusedList]
;; Keys are symbols that do not end in numbers.
;; Values are lists where unused nats can be appended with
;; the key to make an unused symbol.
;; make-unused-table : -> UnusedTable
(def (make-unused-table) (make-hash-table-eq))

;; current-unused-table : [Parameterof UnusedTable]
(def current-unused-table (make-parameter (make-unused-table)))

;; copy-current-unused-table : -> UnusedTable
(define (copy-current-unused-table) (hash-copy (current-unused-table)))

;; use/check-unused : Symbol -> Void
(def (use/check-unused sym)
  (unless (symbol? sym)
    (error 'use/check-unused "expected symbol"))
  (let-values (((s n) (symbol-split sym)))
    (def ut (current-unused-table))
    (def ul (hash-ref ut s []))
    (unless (unusedlist-unused? ul n)
      (error 'use/check-unused "already used" sym))
    (hash-put! ut s (unusedlist-remove ul n))))

;; symbol-fresh : Symbol -> Symbol
;; finds an symbol not used so far, marks it used, and returns it
(def (symbol-fresh sym)
  (unless (symbol? sym)
    (error 'symbol-fresh "expected symbol"))
  (let-values (((s n) (symbol-split sym)))
    (def ut (current-unused-table))
    (def ul (hash-ref ut s []))
    (cond ((unusedlist-unused? ul n)
           (hash-put! ut s (unusedlist-remove ul n))
           (symbolnat s n))
          (else
           (hash-put! ut s (unusedlist-rest ul))
           (symbolnat s (unusedlist-first ul))))))

;; identifier-fresh : Identifer -> Identifier
;; wraps the freshened symbol in the same marks and source location
(def (identifier-fresh id)
  (unless (identifier? id)
    (error 'identifier-fresh "expected identifier"))
  (restx id (symbol-fresh (stx-e id))))

;; --------------------------------------------------------

;; An AlphaBackTable is a [Hashof Symbol Symbol]
;; From alpha-converted symbols to surface symbols.
;; Temps that don't coincide with any surface symbols,
;; should not be in this table.

;; make-alpha-back-table : -> AlphaBackTable
(def (make-alpha-back-table) (make-hash-table-eq))

;; current-alpha-back-table : [Parameterof AlphaBackTable]
(def current-alpha-back-table (make-parameter (make-alpha-back-table)))

;; copy-current-alpha-back-table : -> AlphaBackTable
(def (copy-current-alpha-back-table) (hash-copy (current-alpha-back-table)))

;; add-alpha-back! : Identifier Identifier -> Void
;; From alpha x to surface y
(def (add-alpha-back! x y)
  (hash-put! (current-alpha-back-table)
             (syntax->datum x)
             (syntax->datum y)))

;; symbol-fresh* : Symbol -> Symbol
;; Fresh symbol with an entry in the alpha-back-table
(def (symbol-fresh* x)
  (def y (symbol-fresh x))
  (add-alpha-back! y x)
  y)

;; identifier-fresh* : Identifier -> Identifier
;; Fresh identifier with an entry in the alpha-back-table
(def (identifier-fresh* x)
  (def y (identifier-fresh x))
  (add-alpha-back! y x)
  y)

;; use/check-unused* : Symbol -> Void
;; Unused-so-far symbol with an entry in the alpha-back-table
(def (use/check-unused* x)
  (use/check-unused x)
  (add-alpha-back! x x))

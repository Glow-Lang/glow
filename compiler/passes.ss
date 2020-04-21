;;;;; Compiler passes

(export #t)

(import
  :glow/compiler/multipass :glow/compiler/common
  :glow/compiler/alpha-convert/alpha-convert
  :glow/compiler/desugar/desugar
  (only-in :glow/compiler/typecheck/typecheck
    typecheck read-type-env-file write-type-env type-env=?)
  :glow/compiler/anf/anf)

;;; Layers, passes and strategies

;;; Layers

;; TODO: Our source layer, with its JavaScript-like, ReasonML-like syntax
;;(define-layer ".glow" parse-glow pretty-print-glow)

;; SEXP notation for Glow programs
;; TODO: also represent source location?
(define-layer sexp read-sexp-file write-sexps stx-sexpr=?)

;; Alpha-converted Glow programs
;; TODO: also represent source location and unused-table?
(define-layer alpha.sexp read-sexp-file write-sexps stx-sexpr=?)

;; Desugared Glow programs
;; TODO: also represent source location and unused-table?
(define-layer desugar.sexp read-sexp-file write-sexps stx-sexpr=?)

;; Typed Glow programs
;; TODO: also represent source location, unused-table and type annotations?
(define-layer typedecl.sexp read-type-env-file write-type-env type-env=?)

;; (Typed) Glow programs in A-Normal form
;; where all function call arguments are trivial (reference to constant or variable).
;; TODO: also represent source location, unused-table and type annotations?
(define-layer anf.sexp read-sexp-file write-sexps stx-sexpr=?)


;;; Passes

;; TODO: *Parsing*: transform the original source code into syntax objects.
;; Port → (Listof Stmt)
;;(define-pass identity (glow) (sexp))

;; TODO: *Deriving-expansion*: macro-expand the deriving forms

;; *Alpha-Conversion*: ensure that each identifier only appears once in the entire program.
;; TODO: the user-visible identifiers should stay the same (i.e. (export #f) by default?)
;; TODO: in some future, intersperse alpha-conversion, macro-expansion and type-inference passes?
;; (Listof Stmt) → (values (Listof Stmt) Unused AlphaEnv)
(define-pass alpha-convert (sexp) (alpha.sexp Unused AlphaEnv))

;; *Desugaring*: expand away some more complex syntax into simpler one.
;; NB: Unused is used as a side-effect instead of passed in a pure monadic style
;; (Listof Stmt) Unused AlphaEnv → (Listof Stmt)
(define-pass desugar (alpha.sexp Unused) (desugar.sexp))

;; *Typechecking*: annotate every binding and every sub-expression
;; with an inferred (or explicitly specified) type
;; NB: the Unused table is modified in this pass
;; (Listof Stmt) Unused → TypeEnv
(define-pass typecheck (desugar.sexp Unused) (typedecl.sexp))

;; *A-normalization*: ensure all call arguments are trivial,
;; hence a well-defined sequence for all side-effects.
;; (Listof Stmt) Unused → (Listof Stmt)
(define-pass anf (desugar.sexp Unused) (anf.sexp))

;; *Transaction-grouping*: in an interaction, group all actions into transactions.
;;(define-pass txgroup ".anf.sexp" ".txgroup.sexp")

;; *Message-extraction*: for each choice of transactions, extract a message type for that choice.
;; TODO: maybe merge with previous pass?
;; NB: how choosing the right-size type for each data object is blockchain-dependent
;;(define-pass message-extraction ".txgroup.sexp" ".message.sexp")

;; *Liveness properties*:

;; *Escrow insertion*:

;; *Timeout insertion*:

;; *Safety properties*:

;; *Contract Projection*: extract a contract for every interaction
;;(define-pass contract-projection ".message.sexp" ".contract.sexp")

;; *EVM Extraction*: extract EVM bytecode for the contract
;;(define-pass evm-extraction ".message.sexp" ".contract.sexp")
;; This pass may be further subdivided...

;; *BCH Extraction*: extract BCH bytecode for the contract
;;(define-pass bch-extraction ".message.sexp" ".contract.sexp")
;; This pass may be further subdivided...

;; *Client Projection*: extract a contract for every interaction
;;(define-pass client-projection ".contract.sexp" ".evm.sexp")

;; *Scheme Client*: extract JavaScript code for client
;;(define-pass scheme-extraction ".client.sexp" ".ss")

;; *JavaScript Client*: extract JavaScript code for client
;;(define-pass javascript-extraction ".client.sexp" ".js")

(define-strategy ethereum-direct-style
  alpha-convert desugar typecheck anf) ;; ...

;; Different layers and passes for State-Channel style:
;; the previous contract is virtualized, so that
;; (?before timeout and) immediately after end-point projection,
;; sending/receiving/receiving-as-contract are instrumented, to attempt consensual signing first,
;; and fall back to using the contract in case of timeout,
;; at which point convergence to exit state happens the hard way.

;;(define-strategy ethereum-state-channel-style alpha-convert typecheck anf) ;; ...

(set! default-strategy 'ethereum-direct-style)

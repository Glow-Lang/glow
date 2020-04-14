;;;;; Compiler passes

(export #t)

(import
  :glow/compiler/multipass :glow/compiler/common
  :glow/compiler/alpha-convert/alpha-convert
  (only-in :glow/compiler/typecheck/typecheck typecheck)
  :glow/compiler/anf/anf)

;;; Languages, passes and strategies

;;; Languages

;; TODO: Our source language, with its JavaScript-like, ReasonML-like syntax
;;(define-language ".glow" parse-glow pretty-print-glow)

;; SEXP notation for Glow programs
;; TODO: also represent source location?
(define-language ".sexp" read-sexp-file write-sexps)

;; Alpha-converted Glow programs
;; TODO: also represent source location and unused-table?
(define-language ".alpha.sexp" read-sexp-file write-sexps)

;; Typed Glow programs
;; TODO: also represent source location, unused-table and type annotations?
(define-language ".typed.sexp" read-sexp-file write-sexps)
(define-language ".typedecl.sexp" read-sexp-file write-sexps)

;; (Typed) Glow programs in A-Normal form
;; where all function call arguments are trivial (reference to constant or variable).
;; TODO: also represent source location, unused-table and type annotations?
(define-language ".anf.sexp" read-sexp-file write-sexps)


;;; Passes

;; TODO: *Parsing*: transform the original source code into syntax objects.
;; Port → (Listof Stmt)
;;(define-pass identity ".glow" ".glow.sexp")

;; TODO: *Deriving-expansion*: macro-expand the deriving forms

;; *Alpha-Conversion*: ensure that each identifier only appears once in the entire program.
;; TODO: the user-visible identifiers should stay the same (i.e. (export #f) by default?)
;; TODO: in some future, intersperse alpha-conversion, macro-expansion and type-inference passes?
;; (Listof Stmt) → (values (Listof Stmt) Unused AlphaEnv)
(define-pass alpha-convert ".sexp" ".alpha.sexp")

;; TODO: *Typecheck*: annotate every binding and every sub-expression
;; with an inferred (or explicitly specified) type
;; NB: the Unused table is unused in this pass, but passed along for further passes
;; (Listof Stmt) Unused AlphaEnv → (values (Listof Stmt) Unused AlphaEnv TypeEnv)
(define-pass typecheck ".alpha.sexp" ".typed.sexp")

;; *A-normalization*: ensure all call arguments are trivial,
;; hence a well-defined sequence for all side-effects.
;; (Listof Stmt) Unused AlphaEnv TypeEnv → (values (Listof Stmt) Unused AlphaEnv)
(define-pass anf ".typed.sexp" ".anf.sexp")

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
  alpha-convert typecheck anf) ;; ...


;; Different languages and passes for State-Channel style:
;; the previous contract is virtualized, so that
;; (?before timeout and) immediately after end-point projection,
;; sending/receiving/receiving-as-contract are instrumented, to attempt consensual signing first,
;; and fall back to using the contract in case of timeout,
;; at which point convergence to exit state happens the hard way.

;;(define-strategy ethereum-state-channel-style alpha-convert typecheck anf) ;; ...

(set! default-strategy 'ethereum-direct-style)

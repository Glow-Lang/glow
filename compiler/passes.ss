;;;;; Compiler passes

(export #t)

(import
  :mukn/glow/compiler/multipass :mukn/glow/compiler/common
  :mukn/glow/compiler/parse/parse
  :mukn/glow/compiler/alpha-convert/alpha-convert
  :mukn/glow/compiler/desugar/desugar
  (only-in :mukn/glow/compiler/typecheck/typecheck
    typecheck read-type-env-file write-type-env type-env=?)
  (only-in :mukn/glow/compiler/method-resolve/method-resolve
    method-resolve read-type-table-file write-type-table type-table=?)
  :mukn/glow/compiler/anf/anf
  :mukn/glow/compiler/checkpointify/checkpointify
  :mukn/glow/compiler/checkpointify/checkpoint-info-table
  :mukn/glow/compiler/liveness/checkpoint-liveness
  :mukn/glow/compiler/project/project
  )

;;; Layers, passes and strategies

;;; Layers

;; TODO: at the top-level, have a @module form or structure, NOT a list of statements,
;; so we can attach meta-data to it.

;; TODO: also represent source location somehow? and unused-table?

;; TODO: Our source layer, with its JavaScript-like, ReasonML-like syntax
;; The "read" is open-input-file, since the `parse` pass takes an input port
(define-layer glow open-input-file error error)

;; SEXP notation for Glow programs
(define-layer sexp read-sexp-module write-sexp-module stx-sexpr=?)

;; Alpha-converted Glow programs, and the Unused table
(define-layer alpha.sexp read-sexp-module write-sexp-module stx-sexpr=?)
(define-layer Unused #f #f #f)

;; Desugared Glow programs
;; TODO: also represent source location and unused-table?
(define-layer desugar.sexp read-sexp-module write-sexp-module stx-sexpr=?)

;; Typed Glow programs
;; TODO: also represent source location, unused-table and type annotations?
(define-layer typedecl.sexp read-type-env-file write-type-env type-env=?)

;; Method-resolved Glow programs
(define-layer mere.sexp read-sexp-module write-sexp-module stx-sexpr=?)
(define-layer typetable.sexp read-type-table-file write-type-table type-table=?)

;; (Typed) Glow programs in A-Normal form
;; where all function call arguments are trivial (reference to constant or variable).
;; TODO: also represent source location, unused-table and type annotations?
(define-layer anf.sexp read-sexp-module write-sexp-module stx-sexpr=?)

;; (Typed) Glow programs in A-Normal form with safe-points between participant changes.
;; where all function call arguments are trivial (reference to constant or variable).
(define-layer checkpointify.sexp read-sexp-module write-sexp-module stx-sexpr=?)

;; CheckpointInfoTable
(define-layer cpitable.sexp read-checkpoint-info-table write-checkpoint-info-table checkpoint-info-table=?)
; 2 for after liveness
(define-layer cpitable2.sexp read-checkpoint-info-table write-checkpoint-info-table checkpoint-info-table=?)

;; CheckpointLivenessTable
(define-layer cpltable.sexp read-checkpoint-liveness-table write-checkpoint-liveness-table checkpoint-liveness-table=?)

;; Projection
(define-layer project.sexp read-sexp-module write-sexp-module stx-sexpr=?)

(define-layer safepointify.sexp read-sexp-module write-sexp-module stx-sexpr=?) ;; after safepoints added
(define-layer bbepp.sexp read-sexp-module write-sexp-module stx-sexpr=?) ;; right Before BEPP
(define-layer contract.sexp read-sexp-module write-sexp-module stx-sexpr=?) ;; BEPP for contracts
(define-layer client.sexp read-sexp-module write-sexp-module stx-sexpr=?) ;; BEPP for clients


;;; Passes

;; TODO: *Parsing*: transform the original source code into syntax objects.
;; Port → ModuleStx
(define-pass parse (glow) (sexp))

;; TODO: *Deriving-expansion*: macro-expand the deriving forms

;; *Alpha-Conversion*: ensure that each identifier only appears once in the entire program.
;; TODO: the user-visible identifiers should stay the same (i.e. (export #f) by default?)
;; TODO: in some future, intersperse alpha-conversion, macro-expansion and type-inference passes?
;; ModuleStx → (values ModuleStx Unused AlphaEnv)
(define-pass alpha-convert (sexp) (alpha.sexp Unused AlphaEnv))

;; *Desugaring*: expand away some more complex syntax into simpler one.
;; NB: Unused is used as a side-effect instead of passed in a pure monadic style
;; ModuleStx Unused AlphaEnv → ModuleStx
(define-pass desugar (alpha.sexp Unused) (desugar.sexp))

;; *Typechecking*: annotate every binding and every sub-expression
;; with an inferred (or explicitly specified) type
;; NB: the Unused table is modified in this pass
;; ModuleStx Unused → TypeEnv
(define-pass typecheck (desugar.sexp Unused) (typedecl.sexp TypeInfoTable))

;; *Method-resolve*: handle type methods, attached in `defdata with:` and accessed in `type.method`
(define-pass method-resolve (desugar.sexp Unused) (mere.sexp typetable.sexp))

;; *A-normalization*: ensure all call arguments are trivial,
;; hence a well-defined sequence for all side-effects.
;; ModuleStx Unused → ModuleStx
(define-pass anf (mere.sexp Unused) (anf.sexp))

;; *Transaction-ification*: introduce suitable safe points between changes in participants
(define-pass checkpointify (anf.sexp Unused) (checkpointify.sexp cpitable.sexp))

;; *Checkpoint-Liveness*: determine which variables are publicly live at every checkpoint
(define-pass checkpoint-liveness (cpitable.sexp) (cpitable2.sexp cpltable.sexp))

;; *Message-extraction*: for each choice of transactions, extract a message type for that choice.
;; TODO: maybe merge with previous pass?
;; NB: how choosing the right-size type for each data object is blockchain-dependent
;;(define-pass message-extraction ".txgroup.sexp" ".message.sexp")

;; *Liveness properties*:

;; *Escrow insertion*:
;;(define-pass escrowify (participantity.sexp Unused) (escrowify.sexp))

;; *Timeout insertion*:

;; *Safety properties*:

;; *Projection*: contract and participants in a single file
(define-pass project (checkpointify.sexp Unused cpitable2.sexp) (project.sexp))

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
  parse alpha-convert desugar typecheck method-resolve anf checkpointify checkpoint-liveness project) ;; ...

;; Different layers and passes for State-Channel style:
;; the previous contract is virtualized, so that
;; (?before timeout and) immediately after end-point projection,
;; sending/receiving/receiving-as-contract are instrumented, to attempt consensual signing first,
;; and fall back to using the contract in case of timeout,
;; at which point convergence to exit state happens the hard way.

;;(define-strategy ethereum-state-channel-style alpha-convert typecheck anf) ;; ...

(set! default-strategy 'ethereum-direct-style)

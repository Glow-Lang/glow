(export #t)

(import :std/format :std/iter
        :std/misc/list :std/srfi/1
        <expander-runtime>
        (for-template :mukn/glow/compiler/syntax-context)
        :mukn/glow/compiler/syntax-context
        :mukn/glow/compiler/alpha-convert/fresh
        :mukn/glow/compiler/common
        :clan/base)
(import :clan/debug :std/misc/repr) ;; DEBUG

;; Forward-analysis of *static* flow through variables within each scope of the program.
;; Insert checkpoints in the program from the beginning onward, and for each checkpoint, establish:
;; - What is the set of variables in scope so far at this checkpoint.
;; - What is the set of potential next / previous checkpoints from / to a given one,
;;   are for each pair, what are the relevant side-effects between them?
;;   Effects:
;;   - setting the active user to some variable (which causes a new checkpoint unless idempotent)
;;   - choice between multiple things
;;   - product/sum of publish, deposit or withdrawal statements
;;   - potential failure (maybe)
;;   - non-atomicity (maybe)
;;   - declaring a new variable
;;
;; We have to deal with multiple possible checkpoints in the past, because,
;; in the continuation of a switch wherein some cases contain checkpoints,
;; the previous checkpoint may be one of many things.
;; OR, do we then want to then always insert a checkpoint at the continuation point
;; that joins back after the switch (if non-tail, non-linear-survivor; can be inlined back later if linear)?
;; This prevents exponential explosion from iterated switches (checkpoint|no-checkpoint).
;; If we also insert a named checkpoint at the point of every switch, we might have a strict
;; separation between sum nodes and product nodes.
;; In either case, to each / to the only past checkpoint, the forward analysis information will associate:
;;
;; Question: when calling subroutines, should we have transition
;; to the called routine or to the return point, or both?
;; Or should we have two different kinds of transitions, "static" and "dynamic"?

;; type CheckpointInfoTable = (HashTable CheckpointSymbol -> CI)
;; a CheckpointInfoTable is mutable at this level, but
;; is notionally a monotonic information table:
;; more information about checkpoints is added during this pass, and during the next, backward, pass.
(def current-checkpoint-info-table (make-parameter (make-hash-table)))
(def (get-ci checkpoint) (hash-get (current-checkpoint-info-table) checkpoint))
(def (new-ci checkpoint)
  (def ci (create-ci checkpoint))
  (hash-put! (current-checkpoint-info-table) checkpoint ci)
  ci)

;; type of the current participant in the analysis state
;; #f if all consensual and/or participant-agnostic so far.
;; Participant is a *symbol* (alpha-converted), not an identifier
;; type ParticipantX = Maybe Participant

;; type TI transition-info
;; Information associated to a transition between two points within an atomic block
(defstruct ti
  (from ;; Symbol latest checkpoint before this statement (or #f if unreachable?)
   to ;; (Maybe Symbol) checkpoint reached immediately after this statement, if defined and unique,
   ;; and/or checkpoint for the end of the current scope.
   ;; source ;; ListofStmtStx source code for this transition
   participant ;; (Maybe Participant) identified participant for this block so far
   effects ;; (List StmtStx) list of effects to the interaction: @A deposit! publish! withdraw!, in reverse order
   ;;fallible? ;; Bool can this fail due to a require, or arithmetic?
   ;;reachable? ;; Bool can this point be reached at all? (based just on syntactic criteria, no escape before)
   ;;atomic? ;; Bool is this atomic? Maybe all our blocks always are for now?
   variables-introduced ;; (Listof (Pair Symbol MPart)) variables introduced so far as tis point in the block
   ;                    ;;                              publicly introduced if MPart is false
   variables-used ;; (Listof (Pair Symbol MPart)) variables used in this block, filled by latter backward pass (what if premature exit?)
   ;              ;;                              publicly used if MPart is false
   variables-eliminated) ;; (Listof (Pair Symbol MPart)) variables eliminated so far as tis point in the block, from the end, filled during the latter backward pass (what if premature exit?), used for register/stack allocation and liveness analysis
  ;                      ;;                              publicly eleminated if MPart is false
  transparent: #t)

;; ti-variables-publicly-introduced : TI -> (Listof Symbol)
;; ti-variables-publicly-used : TI -> (Listof Symbol)
(def (ti-variables-publicly-introduced t) (map car (filter (compose not cdr) (ti-variables-introduced t))))
(def (ti-variables-publicly-used t) (map car (filter (compose not cdr) (ti-variables-used t))))

;; TI at the start of an atomic block, just after a checkpoint
;; create-ti : Symbol MaybeParticipant -> TI
(def (create-ti from to participant)
  (make-ti from to participant [] [] [] []))

;; checkpoint-info: represents all the possible atomic paths from or to a given checkpoint
(defstruct ci
  (checkpoint ;; Symbol -- name of the checkpoint
   ;;TODO? sis for the statements within, in reverse order
   ;;TODO? link to CI for upper scope?
   ;;TODO? variables-visible ;; set of all variables introduced in any preceding block
   ;; in this scope (or a surrounding scope?). (NB: they are all bound and immutable)
   variables-live    ;; (Listof Symbol) of the variables visible so far, variables used at this or any future point
   incoming-transitions ;; (Listof TI) transitions going backward (the ti's to field is this checkpoint)
   outgoing-transitions) ;; (Listof TI) transitions going forward (the ti's from field is this checkpoint)
  transparent: #f)

(def (create-ci sym)
  (make-ci sym [] [] []))

;; put-transition! : TI -> TI
(def (put-transition! ti)
  (def from (ti-from ti))
  (when from
    (let* ((to (ti-to ti))
           (from-ci (get-ci from))
           (to-ci (get-ci to)))
      (push! ti (ci-incoming-transitions to-ci))
      (push! ti (ci-outgoing-transitions from-ci))))
  ti)

;; checkpointify: Introduce checkpoints everywhere that might
;; require a change in participant, ending the immediate duties of one
;; and starting the immediate duties of another.
;;
;;   * We add a participant-checkpoint any time there is an actual or potential
;;     change of active participant.
;;
;;   * Note that the interaction *could* start with consensual computations
;;     with require!ments, that are the onus of the caller, who is not determined
;;     by the called interaction, but by the calling interaction.
;;
;; The participant-checkpoints determine the game-theoretic and game-semantic
;; properties of the interaction.
;; They matter when extracting the logical model of the interaction.

;; checkpointify : AnfModuleStx UnusedTable -> CpModuleStx CheckpointInfoTable
(def (checkpointify module unused-table)
  (parameterize ((current-unused-table unused-table)
                 (current-checkpoint-info-table (make-hash-table)))
    (syntax-case module (@module)
      ((@module stmts ...)
       (let-values (((body from to) (checkpointify-scope (syntax->list #'(stmts ...)))))
         (values (retail-stx module [[from to] body ...])
                 (current-checkpoint-info-table)))))))

(def (checkpointify-scope body)
  (def ti (make-scope-ti))
  (def from (ti-from ti))
  (def to (ti-to ti))
  (def revstmts (nth-value 0 (checkpointify-stmts body ti [[#'@label from]])))
  (values (reverse (cons [#'@label to] revstmts))
          from
          to))

;; make-checkpoint : Symbol -> StmtStx
(def (make-checkpoint (label 'pc))
  (def sym (symbol-fresh label))
  (def ci (new-ci sym))
  sym)

;; should only be used for lambda, @make-interaction, and the overall prog
;; should not be used for switch
(def (make-scope-ti)
  (def initial-checkpoint (make-checkpoint 'begin))
  (def final-checkpoint (make-checkpoint 'end))
  (create-ti initial-checkpoint final-checkpoint #f))

;; make-intermediate-checkpoint : [NEListof TI] Symbol -> (values StmtStx TI)
;; ASSERT: the `ti-to` fields of all given `tis` should be the same.
;; Names a new checkpoint cp which comes after all effects in the givin `tis`, but before the common `to`.
;; Puts transition from each original `ti-from` to the new cp, containing information
;; from the original `ti`.
;; Produces 2 values:
;;  * A statement `(@label cp)`, which should be placed after the merging of the statements from `tis`
;;  * A TI which has cp as the `from`, and keeps the common `to` of the given `tis`
(def (make-intermediate-checkpoint tis (label 'cp))
  (def common-to (and (pair? tis) (ti-to (first tis))))
  (for ((ti tis))
    (unless (eq? common-to (ti-to ti))
      (error 'make-intermediate-checkpoint "expected as tis to go to the same checkpoint")))
  (def cp (make-checkpoint label))
  (for ((ti tis))
    (def ti2
      (make-ti (ti-from ti) cp
               (ti-participant ti) (ti-effects ti)
               (ti-variables-introduced ti) (ti-variables-used ti) []))
    (put-transition! ti2))
  (values [#'@label cp]
          (make-ti cp common-to #f [] [] [] [])))

;; introduce-checkpoint : TI [Listof CpStmtStx] -> [Listof CpStmtStx] TI
(def (introduce-checkpoint ti acc)
  ;; Ensure the participant-checkpoint is executed *BEFORE* the statement with a new participant,
  ;; which means cons it after since we accumulate in reverse order.
  (def new-checkpoint (make-checkpoint))
  (values (cons [#'@label new-checkpoint] acc) new-checkpoint))

;; statement-after-checkpoint : CpStmtStx TI [Listof CpStmtStx] SSI -> [Listof CpStmtStx] TI
(def (statement-after-checkpoint stx ti acc ssi)
  ;; Ensure the participant-checkpoint is executed *BEFORE* the statement with a new participant,
  ;; which means cons it after since we accumulate in reverse order.
  (defvalues (cpstx2 ti2) (make-intermediate-checkpoint [ti]))
  (values (cons* stx cpstx2 acc) (merge-ssi ti2 ssi)))

;; For use in dead code, after a return (should not be syntactically possible)
;; unreachable-ti : -> TI
(def unreachable-ti (create-ti #f #f #f))

;; Accumulate statements *in reverse order*, adding checkpoints as needed
;; Return the *reversed* list of statements AND the TI
;;
;; checkpointify-stmts : [Listof AnfStmtStx] TI [Listof CpStmtStx] -> [Listof CpStmtStx] TI
(def (checkpointify-stmts stmts ti acc)
  (match stmts
    ([] (values acc ti))
    ([stmt . more]
     (defvalues (acc2 ti2) (checkpointify-stmt stmt ti acc))
     (checkpointify-stmts more ti2 acc2))))

;; Given the participant of the current transition so far and the participant of the next simple statement,
;; determine the participant of extended transition if the statement is compatible with it.
;; #f means "any participant" (top), #t means "many different participants" (bottom),
;; and will trigger a new checkpoint. TODO: Maybe reverse those booleans?
;;
;; TODO: do we want to represent a *set* of participants that it could be,
;; due to switches, calls, symbolic participant extraction, etc.?
;; Or introduce new participant variables?
;; Do we want a set of participants and/or Top (bottom being the empty set)?
;; [#f Alice] == it can either remain undetermined or change to Alice.
;; [Alice Bob] == it can either stay the same or change to Alice (not undetermined anymore)
;; may matter after a switch (?)
;; unify-participants : ParticipantOrBool * -> ParticipantOrBool
(def unify-participants
  (case-lambda
   ((p1 p2) (cond ((equal? p1 p2) p1) ;; NB: this includes the case where both are #f, i.e. any
                  ((not p1) p2)
                  ((not p2) p1)
                  (else #t)))
   (() #f) ((p) p) (ps (unify-participants/list ps))))
(def (unify-participants/list p) (reduce unify-participants #f p))
(def (participant-ambiguous? p) (eq? p #t))

;; simple-statement-info: transition info contribution from a simple statement
(defstruct ssi
  (participant ;; participant for this statement
   effects ;; effect of this statement
   variables-introduced ;; variables introduced by this statement
   variables-used) ;; variables used by this statement
  transparent: #t)

;; merge-ssi : TI SSI -> TI
(def (merge-ssi ti ssi)
  (make-ti (ti-from ti) (ti-to ti)
           (unify-participants (ti-participant ti) (ssi-participant ssi))
           ;; TODO: use symdict merges instead of list appends to avoid duplicate elements?
           (append (ssi-effects ssi) (ti-effects ti))
           (append (ssi-variables-introduced ssi) (ti-variables-introduced ti))
           (append (ssi-variables-used ssi) (ti-variables-used ti))
           []))

(def (merge-ti ti ti2)
  (assert! (eq? (ti-from ti) (ti-from ti2)))
  (assert! (eq? (ti-to ti) (ti-to ti2)))
  (make-ti (ti-from ti) (ti-to ti)
           (unify-participants (ti-participant ti) (ti-participant ti2))
           ;; TODO: use symdict merges instead of list appends to avoid duplicate elements?
           (append (ti-effects ti2) (ti-effects ti))
           (append (ti-variables-introduced ti2) (ti-variables-introduced ti))
           (append (ti-variables-used ti2) (ti-variables-used ti))
           []))

;; simple : AnfStmtStx TI [Listof CpStmtStx] [Listof EffectStx] [Listof Symbol] -> [Listof CpStmtStx] TI
(def (checkpointify-simple-stmt stx ti acc ssi)
  (values (cons stx acc) (merge-ssi ti ssi)))

;; checkpointify-single-stmt : [Listof CpStmtStx] TI (Args ... -> CpStmtStx SSI) Args ... -> [Listof CpStmtStx] TI
(def (checkpointify-single-stmt acc ti f . args)
  (defvalues (stx ssi) (apply f args))
  (def participant-before (ti-participant ti))
  (assert! (not (participant-ambiguous? participant-before)))
  (def participant-after (unify-participants participant-before (ssi-participant ssi)))
  ;;(DBG css: acc ti f args stx ssi participant-before participant-after)
  (if (equal? participant-before participant-after)
    (checkpointify-simple-stmt stx ti acc ssi)
    (statement-after-checkpoint stx ti acc ssi)))

;; Accumulate checkpoint statements for the current
;; participant-checkpoint-less statement,
;; in reverse order at the beginning of the accumulator acc.
;; checkpointify-stmt : AnfStmtStx TI [Listof CpStmtStx] -> [Listof CpStmtStx] TI
(def (checkpointify-stmt stx ti acc)

  ;; simple : [Listof EffectStx] [Listof [Pair Symbol MPart]] -> [Listof CpStmtStx] TI
  (def (simple e u) (checkpointify-simple-stmt stx ti acc (make-ssi #f e [] u)))

  ;; continue : (Args ... -> CpStmtStx SSI) Args ... -> [Listof CpStmtStx] TI
  (def (continue f . args) (apply checkpointify-single-stmt acc ti f stx args))

  (syntax-case stx (@ @debug-label @record deftype defdata publish! def return ignore! switch require! assert! deposit! withdraw!)
    ((@debug-label . _) (simple [] []))
    ((@ p s) (identifier? #'p)
     ;; NOTE: publish! deposit! withdraw! effects are *NOT* allowed within @p,
     ;; so we can just cons stx at the head, after a checkpoint.
     ;; TODO: properly recurse into function and interaction definitions, and switches, in s ?
     ;; or are they irrelevant to contract extraction analysis?
     (checkpointify-single-stmt acc ti (cut values stx (make-ssi (stx-e #'p) [] [] []))))
    ((publish! . _) (continue checkpointify-publish))
    ((deposit! . _) (continue checkpointify-deposit))
    ((def v e) (checkpointify-prefixed-expr stx ti acc #'(def v) #f (used<-arg #'v) #'e))
    ((ignore! e) (checkpointify-prefixed-expr stx ti acc #'(ignore!) #f [] #'e))
    ((return e) (checkpointify-prefixed-expr stx ti acc #'(return) #t [] #'e))
    ((switch c cases ...) (checkpointify-switch stx ti acc))
    ((defdata . _) (simple [] []))
    ((deftype . _) (simple [] []))
    ;; NB: after ANF, p e v below are guaranteed identifiers
    ((withdraw! lbl p (@record (_ e) ...)) (simple [stx] (used<-args #'(p e ...))))
    ((require! v) (simple [] (used<-arg #'v)))
    ((assert! v) (simple [] (used<-arg #'v)))))

(def (checkpointify-publish stx)
  (syntax-case stx ()
    ((_ p v)
     (values stx (make-ssi (stx-e #'p) [stx] (used<-arg #'v) (cons (syntax->datum #'(v . p)) (used<-arg #'p)))))))

(def (checkpointify-deposit stx)
  (syntax-case stx (@record)
    ((_ _ p (@record (_ v) ...))
     (values stx (make-ssi (stx-e #'p) [stx] [] (used<-args #'(p v ...)))))))

(def (checkpointify-body stx prefix body ti acc)
  (defvalues (stmts2 ti2) (checkpointify-stmts (syntax->list body) ti acc))
  (with-syntax (((prefix ...) prefix)
                ((stmts3 ...) (reverse stmts2)))
    (values (restx1 stx #'(prefix ... stmts3 ...)) ti2)))

;; used<-arg : TrivialExprStx -> [Listof [Pair Symbol MPart]]
(def (used<-arg e)
  (if (identifier? e) [[(stx-e e) . #f]] []))
;; used<-args : [StxListof TrivialExprStx] -> [Listof [Pair Symbol MPart]]
(def (used<-args a)
  (map (cut cons <> #f) (filter symbol? (syntax->datum a))))

;; After ANF, expressions are all simple, except for those containing bodies of statements,
;; like conditionals and lambdas.
;; checkpointify-prefixed-expr : AnfStmtStx TI [Listof CpStmtStx] PrefixStx Bool [ListOf Symbol] ExprStx -> [Listof CpStmtStx] TI
(def (checkpointify-prefixed-expr stx ti acc prefix tail? introduced expr)

  (def (continue-expr e ssi)
    (def stx2 (with-syntax ((e e) ((p ...) prefix)) (restx1 stx #'(p ... e))))
    (def ti2 (merge-ssi ti ssi))
    (if tail?
      (begin
        (put-transition! ti2)
        (values (cons stx2 acc) unreachable-ti))
      (values (cons stx2 acc) ti2)))

  (def (rewrite f)
    (defvalues (re used) (f expr))
    (continue-expr re (make-ssi #f [] introduced used)))

  (def (simple used)
    (continue-expr expr (make-ssi #f [] introduced used)))

  (def (simple-arg arg) (simple (used<-arg arg)))
  (def (simple-args args) (simple (used<-args args)))

  ;;(trace! simple-arg simple-args)

  (syntax-case expr (@dot @tuple @list @record input digest sign == @app @call-interaction λ @make-interaction)
    (_ (trivial-expr? expr) (simple-arg expr))
    ((@dot v _) (simple-arg #'v))
    ((@tuple . a) (simple-args #'a))
    ((@list . a) (simple-args #'a))
    ((@record (x e) ...) (simple-args #'(e ...)))
    ((input _ tag) (simple-arg #'tag))
    ((digest . a) (simple-args #'a))
    ((sign e) (simple-arg #'e))
    ((== x y) (simple-args #'(x y)))
    ;; TODO: support functions that are not simple, based on effect typing?
    ;; TODO: distinguish primitives already.
    ((@app . a) (simple-args #'a)) ;; assume all functions are simple, for now.
    ;; TODO: introduce checkpoint after return, unless it's a tail call.
    ((@call-interaction . a)
     (let* ((used (used<-args #'a))
            (ssi (make-ssi #t [#'expr] introduced used)))
       (if tail?
         (continue-expr #'expr ssi)
         (let-values (((cpstx ti2)
                       (make-intermediate-checkpoint [(merge-ssi ti ssi)] 'return)))
           (values (cons* cpstx stx acc) ti2)))))
    ;; TODO: mark closure variables as used that are visible in this scope.
    ((λ . _) (rewrite checkpointify-lambda))
    ((@make-interaction . _) (rewrite checkpointify-make-interaction))))

(def (checkpointify-lambda stx)
  (syntax-case stx ()
    ((l params body ...)
     (let-values (((body2 from to) (checkpointify-scope #'(body ...))))
       (with-syntax (((body2 ...) body2) (from from) (to to))
         (values (restx1 stx #'(l params (from to) body2 ...))
                 [])))))) ;; TODO: extract the used variables from the last label of the function?

(def (checkpointify-make-interaction stx)
  (syntax-case stx ()
    ((@make-interaction iparams lparams body ...)
     (let-values (((body2 from to) (checkpointify-scope #'(body ...))))
       (with-syntax (((body2 ...) body2) (from from) (to to))
         (values (restx1 stx #'(@make-interaction iparams lparams (from to) body2 ...))
                 [])))))) ;; TODO: extract the used variables from the last label of the function?

(def (checkpointify-switch stx ti acc)
  (syntax-case stx ()
    ((switch e cases ...)
     (let-values (((label-begin ti2) (make-intermediate-checkpoint [ti] 'begin-switch)))
      (let* ((ti3 (merge-ssi ti2 (make-ssi #f [] [] (used<-arg #'e))))
             (cts (map (cut checkpointify-swcase <> ti3) (syntax->list #'(cases ...))))
            ; cs : [Listof SwcaseStx]
             (cs (map first cts))
            ; ts : [Listof TI]
            ; all have the same `ti-to` field, gotten from `ti2`, `(ti-to ti2)`
             (ts (map second cts)))
        (defvalues (label-end ti4) (make-intermediate-checkpoint ts 'end-switch)) ; already a list
        (with-syntax (((scases ...) cs))
          (values
           (cons* label-end
                  (restx1 stx #'(switch e scases ...))
                  label-begin
                  acc)
           ti4)))))))

;; checkpointify-swcase : SwcaseStx TI -> [List SwcaseStx TI]
(def (checkpointify-swcase stx ti)
  (syntax-case stx ()
    ((pattern body ...)
     (let-values (((s2 ti2) (checkpointify-stmts (syntax->list #'(body ...)) ti [])))
       (with-syntax (((stmts2 ...) (reverse s2)))
         [(restx1 stx #'(pattern stmts2 ...)) ti2])))))

;;(trace! trivial-expr? checkpointify checkpointify-stmts checkpointify-stmt checkpointify-prefixed-expr checkpointify-body checkpointify-make-interaction checkpointify-scope)

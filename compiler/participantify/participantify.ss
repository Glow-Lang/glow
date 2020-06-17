(export #t)

(import :std/format :std/iter
        :std/misc/list :std/srfi/1
        :std/misc/repr :clan/utils/debug ;; DEBUG
        <expander-runtime>
        (for-template :glow/compiler/syntax-context)
        :glow/compiler/syntax-context
        :glow/compiler/alpha-convert/fresh
        :glow/compiler/common
        :clan/utils/base)
(import :clan/utils/debug :std/misc/repr)

;; participantify: Introduce participant-checkpoints everywhere that might
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


;; participantify : ModuleStx UnusedTable -> ModuleStx
(def (participantify module unused-table)
  (parameterize ((current-unused-table unused-table))
    (syntax-case module (@module)
      ((@module stmts ...)
       (let-values (((stmts2 part2) (participantify-stmts (syntax->list #'(stmts ...)) #f [])))
         (retail-stx module (reverse stmts2)))))))

;; I don't know for sure what we should return, so it's a ParticipantX for now
;; #f if all consensual so far. -- or should it be variable, introduced if need be?
;; #t if we don't know? A set/list of participants? WTF?
;; Participant is a *symbol* (alpha-converted), not an identifier
;; type ParticipantX = ParticipantOrBool

;; Accumulate statements *in reverse order*, adding participant-checkpoints.
;; Return the *reversed* list of statements AND the last ParticipantX
;;
;; TODO: do we want to represent a *set* of participants that it could be, due to switches?
;; Do we want to desugar if to switch already so we don't have to duplicate code?
;; [#f Alice] == it can either remain undetermined or change to Alice.
;; [Alice Bob] == it can either stay the same or change to Alice (not undetermined anymore)
;; may matter after a switch (?)
;;
;; participantify-stmts : [Listof StmtStx] ParticipantX -> [Listof TxStx] ParticipantX
(def (participantify-stmts stmts participant acc)
  (match stmts
    ([] (values acc participant))
    ([stmt . more]
     (defvalues (acc2 participant2) (participantify-stmt stmt participant acc))
     (participantify-stmts more participant2 acc2))))

(def (participant-checkpoint stx before after)
  (restx1 stx [#'participant-checkpoint (identifier-fresh 'pc) before after]))

(def unify-participants
  (case-lambda
   ((p1 p2) (cond ((equal? p1 p2) p1) ;; NB: this includes the case where both are #f, i.e. any
                  ((not p1) p2)
                  ((not p2) p1)
                  (else #t))) ;; #t means many ;; TODO: DO WE NEED THAT??? OR A SET??? OR NOT AT ALL???
   (() #f) ((p) p) (ps (unify-participants/list ps))))
(def (unify-participants/list p) (reduce unify-participants #f p))

;; Accumulate participant-checkpoint statements for the current
;; participant-checkpoint-less statement,
;; in reverse order at the beginning of the accumulator acc.
;; participantify-stmt : StmtStx ParticipantX [Listof StmtStx] -> [Listof StmtStx] ParticipantX
(def (participantify-stmt stx participant acc)
  (def (simple) (values (cons stx acc) participant))
  (def (continue f . args)
    (defvalues (s p) (apply f args))
    (values
     (if (equal? p participant)
       (cons s acc)
       ;; Ensure the participant-checkpoint is executed *BEFORE* the statement with a new participant,
       ;; which means cons it after since we accumulate in reverse order.
       (cons* s (participant-checkpoint stx participant p) acc))
     p))
  (syntax-case stx (@ deftype defdata publish! def ann return ignore! switch require! assert! deposit! withdraw!)
    ((@ p s) (identifier? #'p)
     (let ((ps (syntax-e #'p)))
       (if (eq? ps participant)
         (simple)
         ;; NOTE: change of participant is *NOT* allowed within @p,
         ;; so we can just cons stx at the head, after a checkpoint.
         (values (cons* stx (participant-checkpoint stx participant ps) acc) ps))))
    ((switch c cases ...)  (continue participantify-switch stx participant))
    ((defdata . _) (continue participantify-defdata stx participant))
    ((def . _) (continue participantify-prefixed-expr stx participant))
    ((ignore! . _) (continue participantify-prefixed-expr stx participant))
    ((return . _) (continue participantify-prefixed-expr stx participant))
    ((publish! . _) (continue participantify-publish-deposit stx participant))
    ((deposit! . _) (continue participantify-publish-deposit stx participant))
    ((deftype . _) (simple)) ;; TODO: support future variant with: rtvalue?
    ((ann . _) (simple))
    ((withdraw! . _) (simple))
    ((require! _) (simple))
    ((assert! _) (simple))))

(def (participantify-defdata stx participant)
  (syntax-case stx ()
    ((prefix ...)
     (values stx participant))))

(def (participantify-body stx prefix body participant acc)
  (defvalues (s p) (participantify-stmts (syntax->list body) participant acc))
  (with-syntax (((prefix ...) prefix)
                ((stmts2 ...) (reverse s)))
    (values (restx1 stx #'(prefix ... stmts2 ...)) p)))

(def (participantify-switch stx participant)
  (syntax-case stx ()
    ((switch e cases ...)
     (let (cp (map (cut participantify-swcase <> participant) (syntax->list #'(cases ...))))
       (with-syntax (((scases ...) (map first cp)))
         (values (restx1 stx #'(switch e scases ...)) (unify-participants/list (map second cp))))))))

(def (participantify-swcase stx participant)
  (syntax-case stx ()
    ((pattern body ...)
     (let-values (((s p) (participantify-stmts (syntax->list #'(body ...)) participant [])))
       (with-syntax (((stmts2 ...) (reverse s)))
         [(restx1 stx #'(pattern stmts2 ...)) p])))))

(def (participantify-publish-deposit stx participant)
  (syntax-case stx ()
    ((_ x _ ...)
     (values stx (stx-e #'x)))))

;; After ANF, expressions are all simple, except for those containing bodies of statements,
;; like conditionals and lambdas.
;; participantify-prefixed-expr : Stx ParticipantX -> Stx ParticipantX
(def (participantify-prefixed-expr stx participant)
  (syntax-case stx ()
    ((prefix ... e) ; NB: prefix is (def v) (ignore!) (return)
     (let ()
       (def (rewrite f) (values (with-syntax ((re (f #'e))) (restx1 stx #'(prefix ... re))) participant))
       (def (simple) (values stx participant))
       (syntax-case #'e (@dot @tuple @record @list λ input digest sign @app @make-interaction @call-interaction ==)
         (_ (trivial-expr? #'e) (simple))
         ((@dot . _) (simple))
         ((@tuple . _) (simple))
         ((@list . _) (simple))
         ((@record . _) (simple))
         ((input . _) (simple))
         ((digest . _) (simple))
         ((sign . _) (simple))
         ((== . _) (simple))
         ((@app a ...) (simple)) ;; assume all functions are simple, for now. Maybe distinguish primitives already
         ((@call-interaction . _) (simple)) ;; TODO: support calling interactions
         ((λ . _) (rewrite participantify-lambda))
         ((@make-interaction . _) (rewrite participantify-make-interaction)))))))

(def (participantify-lambda stx)
  (syntax-case stx ()
    ((l params ?out-type body ...)
     (nth-value 0 (participantify-body stx (restx1 stx #'(l params ?out-type)) #'(body ...) #f [])))))

(def (participantify-make-interaction stx)
  (syntax-case stx ()
    ((@make-interaction iparams lparams ?out-type body ...)
     (nth-value 0 (participantify-body stx #'(@make-interaction iparams lparams ?out-type)
                                       #'(body ...) #f [])))))

;;(import :clan/utils/debug) (trace! participantify participantify-stmts participantify-stmt participantify-prefixed-expr participantify-body participantify-make-interaction)

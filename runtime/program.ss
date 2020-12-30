(export #t)

(import
  :std/iter :std/misc/list :std/srfi/1 :clan/base
  <expander-runtime>
  :mukn/ethereum/types
  :mukn/ethereum/ethereum
  ../compiler/typecheck/type)

;; (deftype Interaction (Table CodeBlock <- Symbol))

(defclass Program
  (name ;; : String
   argument-names ;; : (List Symbol)
   interactions ;; : (Table Interaction <- (OrFalse Symbol))
   compiler-output) ;; : (Table Sexp <- Symbol) ;; S-expression returned by the project pass.
  constructor: :init!
  transparent: #t)

;; Takes the S-expression from the project pass and creates a Program
;; by finding the code-blocks that are transaction boundaries
;; <- Sexp
(def (parse-compiler-output output)
  (def program (extract-program (hash-ref output 'project.sexp)))
  (set! (@ program compiler-output) output)
  program)

(defmethod {:init! Program}
  (λ (self (n "") (an []) (i (make-hash-table)))
    (set! (@ self name) n)
    (set! (@ self argument-names) an)
    (set! (@ self interactions) i)))

;; Interaction <- Program Symbol
(defmethod {get-interaction Program}
  (λ (self participant)
    (hash-get (@ self interactions) participant)))

;; TODO: use typemethods table for custom data types
;; Runtime type descriptor from alpha-converted symbol
;; : Type <- Program Symbol
(defmethod {lookup-type Program}
  (λ (self variable-name)
    (def type-table (hash-ref (@ self compiler-output) 'typetable.sexp))
    (def (type-methods t)
      (match t
        ((type:name 'Bool) Bool)
        ((type:name 'Digest) Digest)
        ((type:name-subtype 'Nat _) UInt256)
        ((type:name sym) (eval sym))
        ((type:name-subtype sym _) (eval sym))
        ((type:tuple ts) (apply Tuple (map type-methods ts)))))
    (type-methods (hash-get type-table variable-name))))

;; context to parse compiler output and locate labels
(defclass ParseContext
  (current-participant ;; : (OrFalse Symbol)
   current-label ;; : (OrFalse Symbol)
   code) ;; : Interaction
  constructor: :init!
  transparent: #t)

(defmethod {:init! ParseContext}
  (λ (self (current-participant #f) ;; : (OrFalse Symbol)
           (current-label 'begin0) ;; : (OrFalse Symbol) ;; TODO: do NOT hardwire begin0
           (code (make-hash-table))) ;; Interaction
    (set! (@ self current-participant) current-participant)
    (set! (@ self current-label) current-label)
    (set! (@ self code) code)))

;; ParseContext <- ParseContext Sexp
(defmethod {add-statement ParseContext}
  (λ (self statement)
    (match (hash-get (@ self code) (@ self current-label))
      ((code-block current-participant statements exits)
        (let ((cb-statements (append statements [statement])))
          (hash-put! (@ self code) (@ self current-label) (make-code-block current-participant cb-statements exits))
          self))
      (#f
        self))))

;; TODO: rename to CodeBlock ?
(defstruct code-block
  (participant ;; : (OrFalse Symbol)
   statements ;; : (List Sexp)
   exit) ;; (OrFalse Symbol)
  transparent: #t)

;; <- ParseContext (OrFalse Symbol)
(defmethod {set-participant ParseContext}
  (λ (self new-participant)
    (unless (and (@ self current-participant) (equal? new-participant (@ self current-participant)))
      (let (contract (@ self code))
        (match (hash-get contract (@ self current-label))
          ((code-block current-participant statements exits)
            (begin
              (match (last statements)
                (['@label last-label]
                 ;;TODO: replace the two statements below by (hash-put! contract (@ self current-label) (make-code-block current-participant statements last-label)) then update all call sites of {get-current-code-block Runtime}
                 (def init-statements (take statements (- (length statements) 1)))
                 (hash-put! contract (@ self current-label) (make-code-block current-participant init-statements last-label))
                 (hash-put! contract last-label (make-code-block new-participant [['set-participant new-participant]] #f))
                 (set! (@ self current-participant) new-participant)
                 (set! (@ self current-label) last-label))
                (else
                 (error "Change of participant with no preceding label")))))
          (#f
            (begin
              (set! (@ self current-participant) new-participant)
              (hash-put! contract (@ self current-label) (make-code-block new-participant [] #f)))))))))

;; Program <- Sexp
(def (extract-program statements)
  (def program (make-Program))
  (for ((statement (syntax->datum statements)))
    (match statement
      (['def name ['@make-interaction [['@list participants ...]] argument-names labels interactions ...]]
        (set! (@ program name) name)
        (set! (@ program argument-names) argument-names)
        (def interactions-table (make-hash-table))
        (for ((values name body) (list->hash-table interactions))
          (hash-put! interactions-table name (process-program name body)))
        (set! (@ program interactions) interactions-table))
      ('@module
        (void))
      (['begin 'end]
        (void))
      (['@label 'begin]
        (void))
      (['return ['@tuple]]
        (void))
      (['@label 'end]
        (void))
      (else
        (error "Unrecognized program statement: " statement))))
  program)

;; : Interaction <- Symbol (List Sexp)
(def (process-program name body)
  (def parse-context (make-ParseContext))
  (for-each! body (λ (statement)
    (match statement
      (['participant:set-participant new-participant]
        {set-participant parse-context new-participant})
      (['consensus:set-participant new-participant]
        {set-participant parse-context new-participant})
      (else
        {add-statement parse-context statement}))))
  (@ parse-context code))
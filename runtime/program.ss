(export #t)

(import
  :std/iter :std/sugar :std/misc/list :std/srfi/1 :clan/base :clan/pure/dict/symdict :clan/poo/object
  :clan/poo/debug
  <expander-runtime>
  :mukn/ethereum/types
  :mukn/ethereum/ethereum
  :mukn/ethereum/signing
  (only-in ../compiler/alpha-convert/alpha-convert init-syms)
  ../compiler/typecheck/type
  ../compiler/checkpointify/checkpointify)

;; (deftype Interaction (Table CodeBlock <- Symbol))

(defclass Program
  (name ;; : String
   parameter-names ;; : (List Symbol)
   interactions ;; : (Table Interaction <- (OrFalse Symbol))
   compiler-output ;; : (Table Sexp <- Symbol) ;; S-expression returned by the project pass.
   initial-label ;; Symbol ;; First label in program, as defined by module header.
   initial-code-block-label ;; Symbol ;; Label preceding first participant code block.
   small-functions) ;; (Table SmallFunction <- Symbol)
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
    (set! (@ self parameter-names) an)
    (set! (@ self interactions) i)
    (set! (@ self small-functions) (make-hash-table))))

;; Interaction <- Program Symbol
(def (get-interaction self participant)
  (hash-get (@ self interactions) participant))

;; TODO: use typemethods table for custom data types
;; Runtime type descriptor from alpha-converted symbol
;; : Type <- Program Symbol
(def (lookup-type self variable-name)
  (def type-table (hash-ref (@ self compiler-output) 'typetable.sexp))
  (def (type-methods t)
    (match t
      ((type:name 'Bool) Bool)
      ((type:name 'Digest) Digest)
      ((type:name 'Participant) Address)
      ((type:name 'Int) UInt256)
      ((type:name-subtype 'Nat _) UInt256)
      ((type:name sym) (eval sym))
      ((type:name-subtype sym _) (eval sym))
      ((type:tuple ts) (apply Tuple (map type-methods ts)))))
  (def t (hash-get type-table variable-name))
  (type-methods t))

;; A conservative predicate that only returns true when the variable-name
;; is definitely a constant.
;; Currently, it is a constant if it is in the `init-syms` or in the values in AlphaEnv,
;; in the future, this may include variables that are defined in the first transaction and
;; precompiled into the contract as first transaction deploys it.
(defmethod {definitely-constant? Program}
  (λ (self variable-name)
    (or (and (memq variable-name init-syms) #t)
        (let* ((alba (hash-ref (@ self compiler-output) 'albatable.sexp))
               (alenv (hash-ref (@ self compiler-output) 'AlphaEnv)))
          (symdict-has-key? alenv (hash-ref alba variable-name))))))

;; : ListOf Symbol <- Program Symbol
(def (lookup-live-variables self code-block-label)
  (def live-variable-table (hash-ref (@ self compiler-output) 'cpitable2.sexp))
  ;; TODO: Store participants in fixed addresses.
  (def participants (filter (λ (x) x) (hash-keys (@ self interactions))))
  (unique (append participants
    (filter (λ (x) (not {definitely-constant? self x}))
          (ci-variables-live (hash-get live-variable-table code-block-label))))))

;; context to parse compiler output and locate labels
(defclass ParseContext
  (current-participant ;; : (OrFalse Symbol)
   current-label ;; : (OrFalse Symbol)
   code) ;; : Interaction
  constructor: :init!
  transparent: #t)

(defmethod {:init! ParseContext}
  (λ (self (current-label #f) ;; : (OrFalse Symbol)
           (code (make-hash-table))) ;; Interaction
    (set! (@ self current-participant) #f)
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

(define-type SmallFunction
  (Record
   arguments: [(List Symbol)]
   start-label: [Symbol]
   end-label: [Symbol]
   body: [(List Any)]))

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
(def (extract-program module)
  (match (syntax->datum module)
    (['@module [initial-label final-label] . statements]
      (def program (make-Program))
      (set! (@ program initial-label) initial-label)
      (for ((statement (syntax->datum statements)))
        (match statement
          (['def name ['λ arguments-value [start-label-value end-label-value] . body-value]]
            (def small-functions (@ program small-functions))
            (hash-put! small-functions name
              (.o arguments: arguments-value
                  start-label: start-label-value
                  end-label: end-label-value
                  body: body-value)))
          (['def name ['@make-interaction [['@list participants ...]] parameter-names labels interactions ...]]
            (set! (@ program name) name)
            (set! (@ program parameter-names) parameter-names)
            (def initial-code-block-label (car labels))
            (set! (@ program initial-code-block-label) initial-code-block-label)
            (def interactions-table (make-hash-table))
            (for ((values name body) (list->hash-table interactions))
              (hash-put! interactions-table name (process-program initial-code-block-label name body)))
            (set! (@ program interactions) interactions-table))
          (['@label label]
            (void))
          (['return ['@tuple]]
            (void))
          (else
            (error "Unrecognized program statement: " statement))))
      program)
    (else
      (error "Unrecognized module format"))))

;; : Interaction <- Symbol (List Sexp)
(def (process-program initial-code-block-label name body)
  (def parse-context (make-ParseContext initial-code-block-label))
  (for-each! body (λ (statement)
    (match statement
      (['participant:set-participant new-participant]
        {set-participant parse-context new-participant})
      (['consensus:set-participant new-participant]
        {set-participant parse-context new-participant})
      (else
        {add-statement parse-context statement}))))
  (@ parse-context code))

(def (get-last-code-block-label self)
  (def consensus-interaction (hash-get (@ self interactions) #f))
  (let/cc return
    (for ((values label code-block) (in-hash consensus-interaction))
      (when (equal? (code-block-exit code-block) #f)
        (return label)))))

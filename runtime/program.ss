(export #t)

(import
  :std/iter :std/sugar :std/misc/list :std/srfi/1 :clan/base :clan/pure/dict/symdict :clan/poo/object
  :clan/poo/io :clan/poo/object :clan/poo/brace :clan/poo/debug
  <expander-runtime>
  :mukn/ethereum/types
  :mukn/ethereum/ethereum
  (only-in ../compiler/alpha-convert/alpha-convert init-syms)
  ../compiler/typecheck/type
  ../compiler/checkpointify/checkpointify)

(def Interaction (Map CodeBlock <- Symbol))

(define-type InteractionInfo
  (.+
   (Record
    name: [Symbol]
    participant-names: [(List Symbol)]
    asset-names: [(List Symbol)]
    parameter-names: [(List Symbol)]
    specific-interactions: [(Map Interaction <- (OrFalse Symbol))]
    initial-code-block-label: [Symbol])
   {.make: (lambda (name participant-names asset-names parameter-names specific-interactions initial-code-block-label)
             { name
               participant-names
               asset-names
               parameter-names
               specific-interactions
               initial-code-block-label})
   }))

(def SExp Any)

(define-type Program
  (.+
    (Record
      interactions: [(Map InteractionInfo <- Symbol)]
      compiler-output: [(Map SExp <- Symbol)]
      initial-label: [Symbol]
      small-functions: [(Map SmallFunction <- Symbol)])
    {.make:
      (lambda (some-compiler-output some-initial-label)
        {interactions: (make-hash-table)
         compiler-output: some-compiler-output
         initial-label: some-initial-label
         small-functions: (make-hash-table)})
      ;; A conservative predicate that only returns true when the variable-name
      ;; is definitely a constant.
      ;; Currently, it is a constant if it is in the `init-syms` or in the values in AlphaEnv,
      ;; in the future, this may include variables that are defined in the first transaction and
      ;; precompiled into the contract as first transaction deploys it.
     .definitely-constant?:
      (lambda (self variable-name)
        (or (and (memq variable-name init-syms) #t)
              (let* ((alba (hash-ref (.@ self compiler-output) 'albatable.sexp))
                     (alenv (hash-ref (.@ self compiler-output) 'AlphaEnv)))
                (symdict-has-key? alenv (hash-ref alba variable-name)))))
    }))

;; Interaction <- Program Symbol Symbol
(def (get-interaction self name participant)
  (def specific-interactions
        (.@ (hash-get (.@ self interactions) name) specific-interactions))
  (hash-get specific-interactions participant))

;; TODO: use typemethods table for custom data types
;; Runtime type descriptor from alpha-converted symbol
;; : Type <- Program Symbol
(def (lookup-type self variable-name)
  (def type-table (hash-ref (.@ self compiler-output) 'typetable.sexp))
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

;; : Symbol <- Program Symbol
(def (lookup-surface-name self variable-name)
  (def alba (hash-ref (.@ self compiler-output) 'albatable.sexp))
  (hash-ref alba variable-name))

;; : ListOf Symbol <- Program Symbol Symbol
(def (lookup-live-variables self name code-block-label)
  (def live-variable-table (hash-ref (.@ self compiler-output) 'cpitable2.sexp))
  (def specific-interactions
    (.@ (hash-get (.@ self interactions) name) specific-interactions))
  ;; TODO: Store participants in fixed addresses.
  (def participants (filter (λ (x) x) (hash-keys specific-interactions)))
  (unique (append participants
    (filter (λ (x) (not (.call Program .definitely-constant? self x)))
          (ci-variables-live (hash-get live-variable-table code-block-label))))))

(define-type ParseContext
  (.+
   (Record
    current-participant: [(OrFalse Symbol)]
    current-label: [(OrFalse Symbol)]
    code: [Interaction])
   {.make: (lambda ((current-label #f) (code (make-hash-table)))
             { current-label
               code
               current-participant: #f })}))

;; ParseContext <- ParseContext Sexp
(def (add-statement self statement)
  (let (code-block (hash-get (.@ self code) (.@ self current-label)))
    (cond
     ((element? CodeBlock code-block)
      (with-slots (participant statements exit) code-block
       (let ((cb-statements (append statements [statement])))
         (hash-put! (.@ self code) (.@ self current-label) (.call CodeBlock .make participant cb-statements exit))
         self)))
     (else
      self))))

(define-type CodeBlock
  (.+
   (Record
    participant: [(OrFalse Symbol)]
    statements: [(List SExp)]
    exit: [(OrFalse Symbol)])
   {.make: (lambda (participant statements exit)
             { participant
               statements
               exit })}))

(define-type SmallFunction
  (Record
   arguments: [(List Symbol)]
   start-label: [Symbol]
   end-label: [Symbol]
   body: [(List Any)]))

;; <- ParseContext (OrFalse Symbol)
(def (set-participant self new-participant)
 (unless (and (.@ self current-participant) (equal? new-participant (.@ self current-participant)))
    (let (contract (.@ self code))
      (let (code-block (hash-get contract (.@ self current-label)))
        (cond
         ((element? CodeBlock code-block)
          (with-slots (participant statements exit) code-block
            (begin
              (match (last statements)
                (['@label last-label]
                  ;;TODO: replace the two statements below by (hash-put! contract (.@ self current-label) (make-code-block current-participant statements last-label)) then update all call sites of {get-current-code-block Runtime}
                  (def init-statements (take statements (- (length statements) 1)))
                  (hash-put! contract (.@ self current-label) (.call CodeBlock .make participant init-statements last-label))
                  (hash-put! contract last-label (.call CodeBlock .make new-participant [['set-participant new-participant]] #f))
                  (set! (.@ self current-participant) new-participant)
                  (set! (.@ self current-label) last-label))
                (else
                 (error "Change of participant with no preceding label"))))))
         (else
          (begin
            (set! (.@ self current-participant) new-participant)
            (hash-put! contract (.@ self current-label) (.call CodeBlock .make new-participant [] #f)))))))))

;; Takes the S-expression from the project pass and creates a Program
;; by finding the code-blocks that are transaction boundaries
;; Program <- Sexp
(def (parse-compiler-output compiler-output)
  (def module (hash-ref compiler-output 'project.sexp))
  (match (syntax->datum module)
    (['@module [initial-label final-label] . statements]
      (def program (.call Program .make compiler-output initial-label))
      (for ((statement (syntax->datum statements)))
        (match statement
          (['def name ['λ arguments-value [start-label-value end-label-value] . body-value]]
            (def small-functions (.@ program small-functions))
            (hash-put! small-functions name
              (.o arguments: arguments-value
                  start-label: start-label-value
                  end-label: end-label-value
                  body: body-value)))
          (['def name
             ['@make-interaction
               [['@record ['participants ['@list participant-names ...]]
                          ['assets ['@list asset-names ...]]]]
               parameter-names
               labels
               bodys ...]]
            (def interactions (.@ program interactions))
            (def initial-code-block-label (car labels))
            (def specific-table (make-hash-table))
            (for ((values p body) (list->hash-table bodys))
              (hash-put! specific-table p (process-program initial-code-block-label p body)))
            (hash-put! interactions name
              (.call InteractionInfo .make name participant-names asset-names parameter-names specific-table initial-code-block-label)))
          (['@label label]
            (void))
          (['@debug-label label]
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
  (def parse-context (.call ParseContext .make initial-code-block-label))
  (for-each! body (λ (statement)
    (match statement
      (['participant:set-participant new-participant]
        (set-participant parse-context new-participant))
      (['consensus:set-participant new-participant]
        (set-participant parse-context new-participant))
      (else
        (add-statement parse-context statement)))))
  (.@ parse-context code))

(def (get-last-code-block-label self name)
  (def specific-interactions
    (.@ (hash-get (.@ self interactions) name) specific-interactions))
  (def consensus-interaction (hash-get specific-interactions #f))
  (let/cc return
    (for ((values label code-block) (in-hash consensus-interaction))
      (when (equal? (.@ code-block exit) #f)
        (return label)))))

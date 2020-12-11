(export #t)

(import
  :std/iter :std/misc/list :std/srfi/1 :clan/base
  <expander-runtime>
  ../compiler/typecheck/type)

(defclass Program (name argument-names interactions compiler-output)
  constructor: :init!
  transparent: #t)

(def (parse-compiler-output output)
  (def program (extract-program (hash-ref output 'project.sexp)))
  (set! (@ program compiler-output) output)
  program)

(defmethod {:init! Program}
  (λ (self (n "") (an []) (i (make-hash-table)))
    (set! (@ self name) n)
    (set! (@ self argument-names) an)
    (set! (@ self interactions) i)))

(defmethod {get-interaction Program}
  (λ (self participant)
    (hash-get (@ self interactions) participant)))

;; TODO: use typemethods table for custom data types
(defmethod {lookup-type Program}
  (λ (self variable-name)
    (def type-table (hash-ref (@ self compiler-output) 'typetable.sexp))
    (match (hash-get type-table variable-name)
      ((type:name sym) (eval sym))
      ((type:name-subtype sym _) (eval sym)))))

(defclass ParseContext (current-participant current-label code)
  constructor: :init!
  transparent: #t)

(defmethod {:init! ParseContext}
  (λ (self (cp #f) (cl 'begin0) (c (make-hash-table)))
    (set! (@ self current-participant) cp)
    (set! (@ self current-label) cl)
    (set! (@ self code) c)))

(defmethod {add-statement ParseContext}
  (λ (self statement)
    (match (hash-get (@ self code) (@ self current-label))
      ((code-block current-participant statements exits)
        (let ((cb-statements (append statements [statement])))
          (hash-put! (@ self code) (@ self current-label) (make-code-block current-participant cb-statements exits))
          self))
      (#f
        self))))

(defstruct code-block (participant statements exit) transparent: #t)

(defmethod {set-participant ParseContext}
  (λ (self new-participant)
    (unless (and (@ self current-participant) (equal? new-participant (@ self current-participant)))
      (let (contract (@ self code))
        (match (hash-get contract (@ self current-label))
          ((code-block current-participant statements exits)
            (begin
              (match (last statements)
                (['@label last-label]
                  (def init-statements (take statements (- (length statements) 1)))
                  (hash-put! contract (@ self current-label) (make-code-block current-participant init-statements last-label))
                  (hash-put! contract last-label (make-code-block new-participant [] #f))
                  (set! (@ self current-participant) new-participant)
                  (set! (@ self current-label) last-label))
                (else
                  (error "Change of participant with no preceding label")))))
          (#f
            (begin
              (set! (@ self current-participant) new-participant)
              (hash-put! contract (@ self current-label) (make-code-block new-participant [] #f))
              self)))))))


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
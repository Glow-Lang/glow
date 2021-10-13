(export scan-for-labels verification)

(import :std/iter
        :std/format
        :std/misc/list
        :std/misc/repr
        :std/misc/hash
        :std/misc/process
        :std/misc/ports
        :gerbil/gambit/misc
        <expander-runtime>
        :clan/base
        :clan/pure/dict/symdict
        :mukn/glow/compiler/common
        :mukn/glow/compiler/checkpointify/checkpointify
        :mukn/glow/compiler/typecheck/typecheck
        :mukn/glow/compiler/typecheck/variance
        :mukn/glow/compiler/typecheck/type
        :mukn/glow/compiler/typecheck/stx-prop)

;; TODO : error on duplicated label
(def (scan-for-labels src)
     (letrec
         ((acc [])
          (f (lambda (x)
               (if (eq? (car x) 'withdraw!)
                   (let (l (car (cdr x)))
                     (if l (set! acc (cons l acc))))
                   )))
          (step (lambda (x)                   
                   (if (list? x)
                        (if (not (null? x))
                            (begin (f x) (step (car x)) (step (cdr x))))))))
    (step src)
   (values acc)
  ))


(def (module-interactions module)
       (match (syntax->datum module)
    (['@module [initial-label final-label] . statements]
     (list->hash-table (filter identity (map (lambda (x)
               (match (syntax->datum x)
                      (['def iName (cons* '@make-interaction iBody)]
                         (cons (syntax->datum iName) (syntax->datum iBody)))

                      (else #f))
               
               ) statements))
     ))
    (else
      (error "Unrecognized module format"))))
     


(def (string->expr s) (read (open-input-string s)))

(def (run-z3 z3-input)
     (let ((z3-formula
              (string-join (map (lambda (x) (call-with-output-string (lambda (out) (write x out)))) z3-input) #\newline)))
           
       (write-file-string "/tmp/z3.glow.tmp" z3-formula)
       ;; (pretty-print z3-input)
       (let ((z3-output (run-process ["z3" "/tmp/z3.glow.tmp"]  check-status: #f)))
         ;; (pretty-print z3-output)
         (string->expr (string-join ["(" z3-output ")"] #\newline))
         )
       ))


(def example-z3-input
     ;; activate model generation
  '(
     (set-option :produce-models true)

     (declare-fun x () Int)
     (declare-fun y1 () Int)
     (declare-fun y2 () Int)
     (declare-fun z () Int)

     (assert (= x y1))
     (assert (not (= y1 z)))
     (assert (= x y2))
     (assert (and (< y2 0) (< 2 1)))

     (check-sat)

     ;; ask for the model value of some of the constants
     (get-value (x z y1 y2))

     (exit)
  )
)

(def (verification proj labels typetable.sexp)
     ;; (pretty-print (hash-get typetable.sexp 'x))

     ;; (pretty-print (hash->list (module-interactions proj)))
     (pretty-print (run-z3 example-z3-input))

     
  (values))




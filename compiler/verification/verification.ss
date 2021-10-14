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
         (write z3-output)
         (string->expr (string-join ["(" z3-output ")"] #\newline))
         )
       ))


(def (to-z3-type ty)
     (cond
      ((equal? ty type:Int) 'Int)
      ((equal? ty type:Nat) 'Int) ;;TODO handle this in some sane way!!
      ((equal? ty type:Bool) 'Bool)
      (else #f))
     )


(def (verify-assertion typetable.sexp i-name i-parameters stmnts participant assertion-formula)
     ;; (pretty-print assertion-formula)
     (pretty-print [i-name i-parameters (length stmnts) participant assertion-formula])
     (letrec
         (wrap-into-quantifiers
          (λ (stmnts e)
            (match stmnts
                   ((cons s ss)
                    (match (syntax->list s)
                           (['def sym v]
                            (match (syntax->list v)
                                   (['input ty _] ['exists (list [sym ty]) (wrap-into-quantifiers ss e)])
                                   (else
                                    (let (ty (to-z3-type  (hash-get typetable.sexp sym)))
                                      (cond (ty ['forall (list [sym ty]) (wrap-into-quantifiers ss e)] )
                                            (else (wrap-into-quantifiers ss e) ))) ;; TODO : hande this somehow!!!
                                   )
                            
                            ))
                           (else (wrap-into-quantifiers ss e))
                           ))
                   (else e)
                   )

             )
          )
       (letrec
           ((formula-for-z3
              [ ['assert (wrap-into-quantifiers stmnts ['true])]
                ['check-sat-using ['then 'qe 'smt]]
                ['get-model]
                ])
             (z3-result (run-z3 formula-for-z3)) 
            )
           (pretty-print z3-result)
         )

       )
     
  ;; ()
)

(def (verify-interaction typetable.sexp i-name i-body)
     (let ((i-parameters (car (cdr i-body)))
           (proj-stmnts (list->hash-table (cdr (cdr (cdr i-body)))))
           )
       (hash-map
        (λ (participant stmnts)
          (map (λ (assertion-formula)
                 ;; (pretty-print stmnts)
                 (verify-assertion typetable.sexp i-name i-parameters stmnts participant assertion-formula))
               (extract-assertions-from-stmnts stmnts)))
        proj-stmnts)
       )
     )


;; generatl strucutre of model of z3 for particular assertion:
;; quantifiers ( assertions infered from program flow  /\ assertion generated from formula ) 



;; TODO : fix it! it sohuld work also with assertions outside the root branch of interaction!!
(def (extract-assertions-from-stmnts stmnts) 
     (map cdr (filter (lambda (x) (eq? (car x) 'assert!))  (syntax->list stmnts))))

(def (extract-imputs-from-stmnts stmnts) 
     (map cdr (filter (lambda (x) (eq? (car x) 'assert!))  (syntax->list stmnts))))

(def (verification proj labels typetable.sexp)
     ;; (pretty-print (to-z3-type  (hash-get typetable.sexp 'x)))
     ;; (pretty-print (hash-get typetable.sexp 'flag))
     (let ((intrctns (module-interactions proj)))
         (hash-map (lambda (k x) (verify-interaction typetable.sexp k x)) intrctns))
     
     ;; (pretty-print (run-z3 example-z3-input))

     
  (values))

;;TODO : ask someone aobut:
;;                          (eq? (car '(a b)) 'a)


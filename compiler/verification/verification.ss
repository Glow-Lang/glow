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
         ;; (write z3-output)
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

(def (to-z3-type-sym ty)
     (cond
      ((equal? ty 'Int) 'Int)
      ((equal? ty 'Nat) 'Int) ;;TODO handle this in some sane way!!
      ((equal? ty 'Bool) 'Bool)
      (else #f))
     )

(def (cons-truthfull x xs)
     (if x (cons x xs) xs))


;; (def (andZ3l l)
;;     (cons 'and (cons 'true l))
;;      )

(def (andZ3l l)
     (match l
            ([] 'true)
            ([x] x)
            (_ (cons 'and l))))

(def (verify-assertion typetable.sexp labels i-name i-parameters stmnts participant hypotesis-formula)
     ;; (pretty-print hypotesis-formula)
     ;; (pretty-print [i-name i-parameters (length stmnts) participant hypotesis-formula])
     (letrec
         ((wrap-into-parameters-quantifiers
          (λ (params e)
            (match params
                   ((cons param-sym ss)
                                 (let (ty (to-z3-type  (hash-get typetable.sexp param-sym)))
                                      (cond (ty ['forall (list [param-sym ty]) (wrap-into-parameters-quantifiers ss e)] )
                                            (else (wrap-into-parameters-quantifiers ss e) )))
                    )
                   (else e))))
          (wrap-into-labels-quantifiers
            (λ (labels e)
              (match labels
                     ((cons label-sym ss) ['forall (list [label-sym 'Bool]) (wrap-into-labels-quantifiers ss e)]
                      )
                     (else e))))
          (wrap-into-quantifiers
            (λ (stmnts e)
              (match stmnts
                     ((cons s ss)
                      (match (syntax->list s)
                             (['def sym v]
                              (match (syntax->list v)
                                     (['input ty _] ['exists (list [sym (to-z3-type-sym ty)]) (wrap-into-quantifiers ss e)])
                                     (else
                                      (let (ty (to-z3-type  (hash-get typetable.sexp sym)))
                                        (cond (ty ['forall (list [sym ty]) (wrap-into-quantifiers ss e)] )
                                              (else (wrap-into-quantifiers ss e) ))))  ;; TODO : hande this somehow!!!

                                     ))
                             ;; ((or ['switch sym (cons #t branchT)  (cons #f branchF)]
                             ;;              ['switch sym (cons #f branchF)  (cons #t branchT)])
                             ;;          (wrap-into-quantifiers
                             ;;           (append (syntax->list branchT) (syntax->list branchF) (syntax->list ss))
                             ;;           e)
                             ;;          )
                             ((cons* 'switch (cons* sym branches))
                              (wrap-into-quantifiers
                               (apply append (append
                                              (map
                                               (λ (x) (syntax->list (cdr x)))
                                               branches)
                                              [(syntax->list ss)] ))
                                       ;; (append (syntax->list branchT) (syntax->list branchF) )
                                       e)
                                  )
                             (else (wrap-into-quantifiers ss e))
                             ))
                     (else e))))
          (gen-exec-flow-formula-stmnt
           (λ (stmnt)
             (match (syntax->list stmnt)
                    (['@label _] #f)
                    (['@debug-label _] #f)
                    (['participant:set-participant _] #f)
                    (['consensus:set-participant _] #f)
                    (['consensus:withdraw _ _ _] #f)
                    (['expect-deposited _] #f)
                    (['add-to-publish _ _] #f)
                    (['add-to-deposit _] #f)
                    (['assert! _] #f)
                    (['participant:withdraw _ _ _] #f)
                    (['return _] #f)

                    
                    (['require! _] #f) ;; those are treated seperatly!
                    ;; (as preassumptions for reachability of labels,
                    ;; in <label-reach-formulas> function)
                    ;; TODO : investigate if we can put those assumption here (to centralize tratment of 'require construct)
                    ;; THIS ISSUE IS TRICKY!! ,
                    ;;  previous version, where those assumption were addded here resulted in dificult error
                    ;;  and its interpretation is not as straitforward at it may apear at first glance.
                    

                    
                    (['def _ ['input ty _]] #f)
                    (['def _ ['expect-published _]] #f)
                    (['def sy #f] ['= sy 'false])
                    (['def sy #t] ['= sy 'true])
                    
                    (['def sy (cons '== args)] ['= sy (cons '= args)])
                    (['def sy (cons '@app (cons f args))] ['= sy (cons f args)])
                    (['def sy v] ['= sy v]) ;; TODO : check if v is symbol or atom

                    ;; TODO : reconsider if it is worthwile to handle this case sepearately
                    ((or ['switch sym (cons #t branchT)  (cons #f branchF)]
                         ['switch sym (cons #f branchF)  (cons #t branchT)])
                     ['and
                       ['=> sym (gen-exec-flow-formula-stmnts branchT)]
                       ['=> ['not sym] (gen-exec-flow-formula-stmnts branchF)]
                      ])

                    ((cons* 'switch (cons* sym branches))
                     (cons 'and
                           (map (λ (x)
                                  (match x
                                         ((cons pat branch)
                                           ['=> ['= pat sym] (gen-exec-flow-formula-stmnts branch)]    
                                          )))
                                branches)))
                    
                    (else (begin (pretty-print stmnt) (error "not implemented!!!")))
                    )
             
              )
           )
          (gen-exec-flow-formula-stmnts
           (λ (stmnts) (cons 'and (filter identity (map gen-exec-flow-formula-stmnt stmnts ))  ))
           )
          (exec-flow-formula-full (gen-exec-flow-formula-stmnts stmnts) )
          (label-reach-formulas
           (letrec ((acc ['true])                    
                    (add-label-formula
                     (λ (label-symbol assumptions)
                       (set! acc (cons ['= label-symbol (andZ3l assumptions)] acc))
                       #f
                       ))
                    (step ;; this function returns assumptions resulting from executing particular statement,
                          ;; it also accumulates results in acc lists
                     (λ (stmnt assumptions)
                         (match (syntax->list stmnt)
                                        (['consensus:withdraw l _ _]
                                          (add-label-formula l assumptions))
                                        (['participant:withdraw l _ _]
                                         (add-label-formula l assumptions))

                                        (['require! y] ;;TODO check if y is identifier or atom!
                                           y 
                                           )

                                        
                                        ;; TODO : reconsider if it is worthwile to handle this case sepearately
                                        ((or ['switch sym (cons #t branchT)  (cons #f branchF)]
                                             ['switch sym (cons #f branchF)  (cons #t branchT)])
                                          ['and
                                           ['=> sym (andZ3l (stmnts-fold branchT (cons sym assumptions)))]
                                           ['=> ['not sym] (andZ3l (stmnts-fold branchF (cons ['not sym] assumptions)))]
                                          ]
                                           
                                         )
                                        
                                        ((cons* 'switch (cons* sym branches))
                                           
                                             (andZ3l (map (λ (x)
                                                        (match x
                                                               ((cons pat branch)
                                                              ['=> ['= pat sym]
                                                                   (andZ3l
                                                                       (stmnts-fold branch (cons ['= pat sym] assumptions)))
                                                                   ]
                                                                )))
                                                branches))
                                           
                                           )
                                        
                                        (else #f) ;; TODO: handle each case explicitly! to ensure that nothing is ingored
                                                  
                                )))
                    (stmnts-fold ;; this function return list of assumption implicated by fact that statements were executed
                     (λ (stmnts assumptions)
                            (match stmnts
                                   ((cons stmnt ss)
                                     (let ((step-result (step stmnt assumptions)))
                                      (cons-truthfull step-result (stmnts-fold ss (cons-truthfull step-result assumptions)))))
                              (else [])))) 
                    )
             (stmnts-fold stmnts [])
             ;; (pretty-print acc) 
             (cons 'and acc))
           )
          (hy-fo-ev
           (λ (e)
             (match (syntax->list e)
                    (['@app 'canReach l] l)
                    (['@app 'propOr x y] ['or (hy-fo-ev x) (hy-fo-ev y) ])
                    (['@app 'propAnd x y] ['and (hy-fo-ev x) (hy-fo-ev y) ])
                    (['@app 'propImpl x y] ['=> (hy-fo-ev x) (hy-fo-ev y) ])
                    (['@app 'propNot x] ['not (hy-fo-ev x)])
                    ))
           )
           (hypotesis-formula-z3 (hy-fo-ev hypotesis-formula))
          )
       (letrec
           ((formula-for-z3
             [ ['assert
                 (wrap-into-quantifiers stmnts
                   (wrap-into-parameters-quantifiers i-parameters
                          (wrap-into-labels-quantifiers labels
                              ['=> ['and exec-flow-formula-full label-reach-formulas]
                               hypotesis-formula-z3 ])))]
               ;;['check-sat-using ['then 'qe 'smt]]
                ['check-sat]
                ['get-model]
                ])
             (z3-result (run-z3 formula-for-z3)) 
             )
         ;; (pretty-print label-reach-formulas)
         ;; (pretty-print exec-flow-formula-full)
         ;; (pretty-print hypotesis-formula-z3)
           ;; (pretty-print (wrap-into-quantifiers stmnts ()))
           ;; (pretty-print formula-for-z3)
         ;; (pretty-print z3-result)
         (match (syntax->list z3-result)
                ((cons* ['error e] _) (error (string-append "Z3 error: " e)))
                ((cons* 'sat _) ())
                ((cons* 'unsat _)
                 (begin
                   (display "\n--- disproven assumption! ----------\n")
                   (pretty-print hypotesis-formula)
                   (pretty-print hypotesis-formula-z3)
                   (display "\n------------------------------------\n")
                 )
                )
         )

       ))
     
  ;; ()
)

(def (verify-interaction typetable.sexp labels i-name i-body)
     (let ((i-parameters (car (cdr i-body)))
           (proj-stmnts (list->hash-table (cdr (cdr (cdr i-body)))))
           )
       (hash-map
        (λ (participant stmnts)
          (map (λ (assertion-formula)
                 ;; (pretty-print stmnts)
                 (verify-assertion typetable.sexp labels i-name i-parameters stmnts participant assertion-formula))
               (extract-assertions-from-stmnts stmnts)))
        proj-stmnts)
       )
     )


;; generatl strucutre of model of z3 for particular assertion:
;; quantifiers ( assertions infered from program flow  => assertion generated from formula ) 



;; TODO : fix it! it sohuld work also with assertions outside the root branch of interaction!!
(def (extract-assertions-from-stmnts stmnts) 
     (map (lambda (x) (car (cdr x))) (filter (lambda (x) (eq? (car x) 'assert!))  (syntax->list stmnts))))

(def (extract-imputs-from-stmnts stmnts) 
     (map cdr (filter (lambda (x) (eq? (car x) 'assert!))  (syntax->list stmnts))))

(def (verification proj labels typetable.sexp)
     ;; (pretty-print (to-z3-type  (hash-get typetable.sexp 'x)))
     ;; (pretty-print (hash-get typetable.sexp 'flag))
     (let ((intrctns (module-interactions proj)))
         (hash-map (lambda (k x) (verify-interaction typetable.sexp labels k x)) intrctns))
     
     ;; (pretty-print (run-z3 example-z3-input))

     
  (values))

;;TODO : ask someone aobut:
;;                          (eq? (car '(a b)) 'a)

;;TODO : check lexical scope of labels!!! 

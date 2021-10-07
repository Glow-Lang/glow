(export scan-for-labels verification)

(import :std/iter
        :std/format
        :std/misc/list
        :std/misc/repr
        :std/misc/hash
        :std/misc/process
        :gerbil/gambit/misc
        <expander-runtime>
        :clan/base
        :mukn/glow/compiler/common
        :mukn/glow/compiler/checkpointify/checkpointify)

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

(def (verification proj labels)
     ;; (pretty-print labels)
  (values))

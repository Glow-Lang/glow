#lang racket/base

(provide hash-lang
         defglowlang
         defid
         glowmod glowmodblock
         glowstm glowstmblock defglowstm
         glowexp glowexpblock defglowexp)

(require racket/match
         racket/string
         scribble/manual
         syntax/parse/define
         (only-in scribble/racket make-variable-id)
         (only-in scribble/decode splice part-tag-decl)
         (only-in scribble/struct element->string make-index-element)
         (only-in scribble/manual-struct make-language-index-desc)
         (for-label glow)
         (for-syntax racket/base
                     racket/list
                     racket/match
                     racket/string
                     "glow-parse.rkt"))

(define (hash-lang)
  (seclink "hash-lang" (racketmodfont "#lang")))

(define-simple-macro (defglowlang name:id)
  (begin
    (declare-exporting name #:packages ())
    (*defglowlang (racketmodname name))))

(define (*defglowlang name)
  (define modpath name)
  (define modname (make-defglowmodname name modpath the-language-index-desc))
  (splice
   (list
    (tabular
     #:style "defmodule"
     (list (list (elem (hash-lang) " " modname))))
    (part-tag-decl
     (intern-taglet
      `(mod-path ,(datum-intern-literal (element->string modpath))))))))

(define (make-defglowmodname mn mp index-desc)
  (let ([name-str (datum-intern-literal (element->string mn))]
        [path-str (datum-intern-literal (element->string mp))])
    (make-index-element #f
                        (list mn)
                        (intern-taglet `(mod-path ,path-str))
                        (list name-str)
                        (list mn)
                        index-desc)))

(define the-language-index-desc (make-language-index-desc))

(define-simple-macro (defid x:id)
  (parameterize ([read-accept-bar-quote #f])
    (defidentifier #'x)))

(begin-for-syntax
  (define (token->elem t s)
    (match t
      ['HashLang `(hash-lang)]
      [`(ModName ,x) `(racketmodname ,(string->symbol x))]
      ['(IdentifierName "true")  `(racketlink true (racketvalfont ,s))]
      ['(IdentifierName "false") `(racketlink false (racketvalfont ,s))]
      [(or `(IntegerLiteral ,_)
           `(DoubleQuoteStringLiteral ,_) `(SingleQuoteStringLiteral ,_)
           `(StringLiteral ,_))
       `(racketvalfont ,s)]
      [(or `(LeftBracePunctuator ,_)   `(RightBracePunctuator ,_)
           `(LeftPrenPunctuator ,_)    `(RightPrenPunctuator ,_)
           `(LeftBracketPunctuator ,_) `(RightBracketPunctuator ,_)
           `(Annotation ,_) `(Marker ,_))
       `(racketparenfont ,s)]
      [`(Comment ,_) `(racketcommentfont ,s)]
      [(or 'WhiteSpace 'LineTerminator) s]
      ;[`(IdentifierName "let") `(racketlink let (racketkeywordfont ,s))]
      [(or '(Opertor "=") '(Opertor "->") '(Opertor "<-"))
       `(racketkeywordfont ,s)]
      [`(ReservedWord ,x) `(racket ,(string->symbol x))]
      [`(IdentifierName ,x) `(racket ,(string->symbol x))]
      [`(Opertor ,x) `(racket ,(string->symbol x))]
      ))

  (define (tokens->elems ts s [i 0] [d #f])
    (match ts
      ['()
       (cond
         [(= i (string-length s)) '()]
         [else (list (substring s i))])]
      [(cons `(token ,a ,_ ,_) rst)
       #:when (< i a)
       (cons (substring s i a) (tokens->elems ts s a d))]
      [(cons `(token ,a ,_ ,_) rst)
       #:when (< a i)
       (tokens->elems rst s i d)]
      [(list* `(token ,a ,_ (Annotation #\@))
              `(token ,_ ,_ WhiteSpace) ...
              `(token ,_ ,b
                      (IdentifierName
                       ,(and c (or "interaction" "verifiably" "publicly"))))
              rst)
       (define x (string->symbol (string-append "@" c)))
       (cond
         [(equal? d x)
          (cons `(defid ,x)
                (tokens->elems rst s b #f))]
         [else
          (cons `(racketlink ,x (racketkeywordfont ,(substring s a b)))
                (tokens->elems rst s b d))])]
      [(list* `(token ,a ,_
                      (IdentifierName
                       ,(and c (or "publish" "verify" "require" "assert"
                                   "deposit" "withdraw"))))
              `(token ,_ ,_ WhiteSpace) ...
              `(token ,_ ,b (Opertor "!"))
              rst)
       (define x (string->symbol (string-append c "!")))
       (cond
         [(equal? d x)
          (cons `(defid ,x)
                (tokens->elems rst s b #f))]
         [else
          (cons `(racketlink ,x (racketkeywordfont ,(substring s a b)))
                (tokens->elems rst s b d))])]
      [(cons `(token ,a ,b ,c) rst)
       (define e (token->elem c (substring s a b)))
       (match e
         [(or `(racket ,(== d)) `(racketlink ,(== d) . ,_))
          (cons `(defid ,d) (tokens->elems rst s b #f))]
         [_
          (cons e (tokens->elems rst s b d))])]))

  (define (modstrs->elems strs)
    (define str
      (format "~a\n" (string-append* (syntax->datum #`#,strs))))
    (define ts (glow-lex str))
    (match-define (list es ... "\n")
      (tokens->elems ts str 0))
    (for/list ([e (in-list es)])
      (datum->syntax (first strs) e)))

  (define (stmstrs->elems strs [d #f])
    (define str
      (format "#lang glow\n~a" (string-append* (syntax->datum #`#,strs))))
    (define ts (glow-lex str))
    (define es (tokens->elems ts str 11 (syntax->datum #`#,d)))
    (for/list ([e (in-list es)])
      (datum->syntax (first strs) e)))

  (define (expstrs->elems strs [d #f])
    (define str
      (format "#lang glow\n~a\n;" (string-append* (syntax->datum #`#,strs))))
    (define ts (glow-lex str))
   (match-define (list es ... "\n" '(racketparenfont ";"))
     (tokens->elems ts str 11 (syntax->datum #`#,d)))
   (for/list ([e (in-list es)])
      (datum->syntax (first strs) e))))

(define-simple-macro (glowmodblock strs:str ...)
  #:with [e ...] (modstrs->elems (attribute strs))
  (parameterize ([read-accept-bar-quote #f])
    (nested #:style 'code-inset (verbatim e ...))))

(define-simple-macro (glowstmblock strs:str ...)
  #:with [e ...] (stmstrs->elems (attribute strs))
  (parameterize ([read-accept-bar-quote #f])
    (nested #:style 'code-inset (verbatim e ...))))

(define-simple-macro (glowexpblock strs:str ...)
  #:with [e ...] (expstrs->elems (attribute strs))
  (parameterize ([read-accept-bar-quote #f])
    (verbatim e ...)))

(define-simple-macro (glowmod strs:str ...)
  #:with [e ...] (modstrs->elems (attribute strs))
  (parameterize ([read-accept-bar-quote #f])
    (verbatim e ...)))

(define-simple-macro (glowstm strs:str ...)
  #:with [e ...] (stmstrs->elems (attribute strs))
  (parameterize ([read-accept-bar-quote #f])
    (elem e ...)))

(define-simple-macro (glowexp strs:str ...)
  #:with [e ...] (expstrs->elems (attribute strs))
  (parameterize ([read-accept-bar-quote #f])
    (elem e ...)))

(define (boxed e)
  (tabular #:style 'boxed (list (list e))))

(define-syntax-parser defglowstm
  [(_ (main:id args:id ...) (strs:str ...) body:expr ...)
   #:with [e ...] (stmstrs->elems (attribute strs) #'main)
   #`(parameterize ([read-accept-bar-quote #f])
       (let-syntax ([args (make-variable-id 'args)] ...)
         (nested
          (boxed (verbatim e ...))
          body
          ...)))])

(define-syntax-parser defglowexp
  [(_ (main:id args:id ...) (strs:str ...) body:expr ...)
   #:with [e ...] (stmstrs->elems (attribute strs) #'main)
   #`(parameterize ([read-accept-bar-quote #f])
       (let-syntax ([args (make-variable-id 'args)] ...)
         (nested
          (boxed (verbatim e ...))
          body
          ...)))])

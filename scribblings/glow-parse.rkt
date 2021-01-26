#lang racket/base

(provide glow-parse glow-lex)

(require racket/match
         racket/system
         racket/port
         racket/string)

(define (which name)
  (string-trim
   (with-output-to-string
     (λ ()
       (unless (system (format "bash --login -c 'which ~v'" name))
         (error 'which "could not find ~v" name))))))

(define gxi
  (with-handlers
    ([exn:fail?
      (λ (e) (build-path (find-system-path 'home-dir) ".nix-profile" "bin" "gxi"))])
    (which "gxi")))

(define (gxi-e . sexprs)
  (define out (open-output-string))
  (for ([sexpr (in-list sexprs)])
    (fprintf out "~s " sexpr))
  (system* gxi "-e" (get-output-string out)))

(define (glow-parse str)
  (define-values [in out] (make-pipe))
  (parameterize ([current-output-port out])
    (gxi-e '(import :mukn/glow/compiler/common
                    :mukn/glow/compiler/parse/parse)
           `(write-sexp-module (parse (open-input-string ,str)))))
  (close-output-port out)
  (define mod (read in))
  (unless (eof-object? (read in)) (error "expected eof"))
  mod)

(define (glow-lex str)
  (unless (string-prefix? str "#lang glow\n")
    (error "expected `#lang glow` on first line"))
  (define start (string-length "#lang glow\n"))
  (define str-after (substring str start))
  (define-values [in out] (make-pipe))
  (parameterize ([current-output-port out])
    (gxi-e '(import :std/misc/repr
                    :mukn/glow/compiler/parse/lexical)
           `(prn (lex-tokens-list (lexify ,str-after)))))
  (close-output-port out)
  (define toks (or (read in) '()))
  (unless (eof-object? (read in)) (error "expected eof"))
  (list*
   '(token 0 5 HashLang)
   '(token 5 6 WhiteSpace)
   '(token 6 10 (ModName "glow"))
   '(token 10 11 LineTerminator)
   (for/list ([t (in-list toks)])
     (match t
       [`(token ,a ,b ,c ,d)
        `(token ,(+ a start)
                ,(+ b start)
                ,(match d
                   [`',e e]
                   [`(',e ',f) `(,e ,f)]
                   [`(',e ,f) `(,e ,f)]))]))))


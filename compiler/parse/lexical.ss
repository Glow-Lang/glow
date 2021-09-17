(export #t)
(import
        :std/generic
        :drewc/smug
        :std/srfi/13)

;; TODO: Upstream to :drewc/smug-gerbil
(def (sat-token-reader? p (reader token-value))
  (.let* (val ((liftP reader) ITEM))
    (sat p (return val))))

(def SourceCharacter (item))

(def (abr code) (.char=? (if (integer? code) (integer->char code) code)))

(def (opt p) (.or p (return #f)))

(def ID_Start (sat char-alphabetic?))
(def ID_Continue (.or (.let* (c ID_Start) c) (sat char-numeric?) #\_))



;; Unicode Format-Control Characters
(def <ZWNJ> (abr #x200C)) (def <ZWJ> (abr #x200D)) (def <ZWNBSP> (abr #xFEFF))

;;  White Space Code Points
(def <TAB> (abr #\tab)) (def <VT> (abr #x000B)) (def <FF> (abr #x000C))
(def <SP> (abr #\space)) (def <NBSP> (abr #x00A0))

;; TODO: | Other category “Zs” | Any other Unicode “Space_Separator” code point
;; | <USP> |

(def %ws (.or <TAB> <VT> <FF> <SP> <NBSP> <ZWNBSP>))
(def WhiteSpace (.begin %ws (skip %ws) (return 'WhiteSpace)))

;; Line Terminator Code Points
(def <LF> (abr #x000A)) (def <CR> (abr #x000D))
(def <LS> (abr #x2028)) (def <PS> (abr #x2029))
(def LineTerminator (.begin (.or <LF> <CR> <LS> <PS>) (return 'LineTerminator)))


(def HashLang (.let* (hl "#lang") `(HashLang ,hl)))

(def RightBracePunctuator (.list (return 'RightBracePunctuator) #\}))
(def LeftBracePunctuator (.list (return 'LeftBracePunctuator) #\{))
(def LeftPrenPunctuator (.list (return 'LeftPrenPunctuator) #\())
(def RightPrenPunctuator (.list (return 'RightPrenPunctuator) #\)))
(def RightBracketPunctuator (.list (return 'RightBracketPunctuator) #\]))
(def LeftBracketPunctuator (.list (return 'LeftBracketPunctuator) #\[))
(def Annotation (.list (return 'Annotation) #\@))


(def IdentifierName
 (.let* ((s (sat char-alphabetic?))
         (ps (many (.or  (.let* (c (sat char-alphabetic?)) c)  (.let* (n (sat char-numeric?)) n) #\_ #\$  #\!))))
  `(IdentifierName ,(list->string (cons s ps)))))

(def NonZeroDigit (sat (cut string-any <> "123456789"))) ;;
(def DigitWithZero (sat (cut string-any <> "0123456789")))
(def DigitsWithZero (many1 DigitWithZero))

(def DigitIntegerLiteral
    (.or
        (.list #\0)
        (.let* ((d NonZeroDigit)
                (ds (.or DigitsWithZero (return []))))
            [d . ds])))

(def IntegerLiteral (.let* (n (.or DigitIntegerLiteral))
                        (return `(IntegerLiteral, (list->string n)))))

(def OperatorToken
    (.let* (op (.or "<<" ">>"
                    "==" "=>" "<=" ">=" "!=" "!" "="  "<-" "<" ">"
                    "++" "+=" "+"
                    "--" "-=" "->" "-"
                    "*=" "**" "*"
                    "%=" "%"
                    "/=" "/"
                    "|||" "||"
                    "&&&" "&&"
                    "^^^" "~~~"  ))
        (return `(OperatorToken ,op))))


(def Marker
    (.let* (m (.or #\. #\; #\, #\_ #\~ #\? #\: #\| #\'))
        `(Marker ,m)))

(def ReservedWord
    (.let* ((rw (.or
                    "data" "type" "let" "while" "publish!" "verify!" "if" "while"
                    "true" "false" "else" "require!" "assert!" "deposit!" "switch"
                    "withdraw!")))
        `(ReservedWord ,rw)))


(def SingleLineCommentChar (.begin (.not LineTerminator) SourceCharacter))
(def SingleLineCommentChars (many1 SingleLineCommentChar))

(def SingleLineComment
    (.let* ((_ "//") (cs (opt SingleLineCommentChars)))
        (return 'single-line-comment)))

(def MultiCommentStringCharacter
    (.begin #t SourceCharacter))

(def MultiCommentStringCharacters (many MultiCommentStringCharacter))

(def MultiComment
    (.let* (cs (bracket "/*" MultiCommentStringCharacters "*/"))
        (return 'multi-line-comment)))

(def Comment
    (.let* (c (.or SingleLineComment MultiComment)) `(Comment , c)))

(def (char-ascii? c)
  (and (char<=? #\null c) (char<=? c #\delete)))

(def (double-string-nonesc-char? c)
  (and (char-ascii? c) (not (memv c '(#\\ #\")))))
(def DoubleStringNonescCharacter (sat double-string-nonesc-char?))

(def (single-string-nonesc-char? c)
  (and (char-ascii? c) (not (memv c '(#\\ #\')))))
(def SingleStringNonescCharacter (sat single-string-nonesc-char?))

(def DoubleStringEscapeSequence
  (.begin #\\
    (.or #\\ #\" (.begin #\n (return #\newline)))))

(def SingleStringEscapeSequence
  (.begin #\\
    (.or #\\ #\' (.begin #\n (return #\newline)))))

(def DoubleStringCharacter
    (.or DoubleStringNonescCharacter DoubleStringEscapeSequence))

(def SingleStringCharacter
    (.or SingleStringNonescCharacter SingleStringEscapeSequence))

(def DoubleStringCharacters (many DoubleStringCharacter))

(def SingleStringCharacters (many SingleStringCharacter))

(def DoubleQuoteStringLiteral
    (.let* (cs (bracket #\" DoubleStringCharacters #\"))
        `(DoubleQuoteStringLiteral ,(list->string cs))))

(def SingleQuoteStringLiteral
    (.let* (cs (bracket #\' SingleStringCharacters #\'))
        `(SingleQuoteStringLiteral ,(list->string cs))))

(def StringLiteral (.or  SingleQuoteStringLiteral  DoubleQuoteStringLiteral))

(def CommonToken
    (.or
        IdentifierName
        ;; Numbers come first because ~.~ is a punctuator
        IntegerLiteral
        StringLiteral
        OperatorToken
        Marker
        ))

(def TokenTemplate
    (.or
        WhiteSpace
        LineTerminator
        Comment
        HashLang
        CommonToken
        RightBracePunctuator
        LeftBracePunctuator
        LeftPrenPunctuator
        RightPrenPunctuator
        RightBracketPunctuator
        LeftBracketPunctuator
        Annotation))


(def (lex-error c . args)
    (.let* (p (point)) (apply error "Invalid Token:" c "at" (1- p) args)))


(def GlowToken
    (.or (.begin #!void (Token TokenTemplate))
        (.let* (v (.or #!eof ITEM)) (if (eof-object? v) FAIL (lex-error v)))))

(def (tokenize str) (run (many1 GlowToken) str))


(def (production? p) (or (symbol? p) (and (pair? p) (symbol? (car p)))))
(def (token-production t) (let (v (token-value t)) (and (production? v) v)))
(def (production-type p) (and (production? p) (if (pair? p) (car p) p)))
(def (production-value p) (and (production? p) (if (pair? p) (cadr p) p)))
(def (token-production-type t)
  (let (v (token-production t)) (and v (production-type v))))
;;get token value
(def (get-token-value (t #f))
  (def (tpv tk)
    (let (v (token-production tk)) (and v (production-value v))))
  (if t (tpv t) (.let* (t (item)) (return (get-token-value t)))))
(def (match-token-type? p) (token-reader? p token-production-type))
(def (match-token-value? p) (token-reader? p get-token-value))
(def (equal-token-value? v) (sat-token-reader? (cut equal? v <>) get-token-value))
(def (member-token-value? l) (sat-token-reader? (cut member <> l) get-token-value))


(defstruct lex-tokens (vector list) transparent: #t)
(defmethod (input-item (ts lex-tokens)) (input-item (String 0 ts)))
(defmethod (input-item-ref (t lex-tokens) (n <t>))
  (input-item-ref (lex-tokens-vector t) n))

(def (lexify thing (rem '(Comment WhiteSpace LineTerminator)))
  (def tokens (if (list? thing) thing (tokenize thing)))
  (let (v (filter (lambda (t) (let (pt (production-type (token-production t)))
                           (not (member pt rem)))) tokens))
    (lex-tokens (list->vector v) tokens)))
(def (.lex-tokens)
  (lambda (i) ((return (if (String? i) (String-thing i) i)) i)))

(def (next-source-tokens)
  (peek (.let* ((p (point))
                (t (.begin (goto-char (1- p)) (item)))
                (lts (.lex-tokens))
                (l (lex-tokens-list lts))
                (m (member t l)))
          (if m (cdr m) #f))))

(def BooleanLiteral
  (.begin
    (peek (match-token-type? 'IdentifierName))
    (peek (.or (match-token-value? "true")
               (match-token-value? "false")))
    (item)))

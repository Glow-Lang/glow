(export #t)
(import 
        :std/generic
        :drewc/smug 
        :std/srfi/13)

;; LeftBrace
;; RightBrace
;; LeftBracket:std/srfi/13 :std/generic
;; data
;; type
;; AtNotation
;; Let
;; =>

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


(def RightBracePunctuator (.list (return 'RightBracePunctuator) #\}))
(def LeftBracePunctuator (.list (return 'LeftBracePunctuator) #\{))
(def LeftPrenPunctuator (.list (return 'LeftPrenPunctuator) #\())
(def RightPrenPunctuator (.list (return 'RightPrenPunctuator) #\)))
(def RightBracketPunctuator (.list (return 'RightBracketunctuator) #\]))
(def LeftBracketPunctuator (.list (return 'LeftBracketunctuator) #\[))


(def IdentifierName 
 (.let* ((s (sat char-alphabetic?))
         (ps (many (.or  (.let* (c (sat char-alphabetic?)) c)  (.let* (n (sat char-numeric?)) n) #\_ #\$ ))))
  `(IdentifierName ,(list->string (cons s ps)))))

(def NonZeroDigit (sat (cut string-any <> "123456789"))) ;; 
(def DigitWithZero (sat (cut string-any <> "0123456789")))
(def DigitsWithZero (many1 DigitWithZero))

(def DigitIntergerLiteral 
    (.or   
        (.list #\0)
        (.let* ((d NonZeroDigit)
                (ds (.or DigitsWithZero (return []))))
            [d . ds])))

(def IntergerLiteral (.let* (n (.or DigitIntergerLiteral))
                        (return `(IntergerLiteral, (list->string n)))))

(def Operator
    (.let* (op (.or ">>" "<<" 
                    "==" "=>" "<=" "!=" "!" "=" "<" ">" 
                    "++" "+=" "+"
                    "--" "-=" "-"
                    "*=" "**" "*"
                    "%=" "%" 
                    "/=" "/"
                    "||"
                    "&&" "&"))
        (return `(Operator ,op))))


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
    (.begin  SourceCharacter))

(def MultiCommentStringCharacters (many MultiCommentStringCharacter))

(def MultiComment
    (.let* (cs (bracket "/*" MultiCommentStringCharacters "*/"))
        (return 'multi-line-comment)))

(def Comment 
    (.let* (c (.or SingleLineComment MultiComment)) `(Comment , c)))


(def DoubleStringCharacter 
    (.or (.begin (.not (.or #\" LineTerminator)) SourceCharacter)))

(def DoubleStringCharacters (many DoubleStringCharacter))

(def DoubleQuoteStringLiteral 
    (.let* (cs (bracket #\" DoubleStringCharacters #\"))
        `(DoubleQuoteStringLiteral ,(list->string cs))))


(def SingleStringCharacter 
    (.or (.begin (.not (.or #\' LineTerminator)) SourceCharacter)))

(def SingleStringCharacters (many SingleStringCharacter))

(def SingleQuoteStringLiteral 
    (.let* (cs (bracket #\' SingleStringCharacters #\'))
        `(SingleQuoteStringLiteral ,(list->string cs))))

(def String (.let* (str (.or  SingleQuoteStringLiteral  DoubleQuoteStringLiteral)) `(String ,str)))

(def CommonToken
    (.or
        IdentifierName 
        ;; Numbers come first because ~.~ is a punctuator
        IntergerLiteral
        String    
        Marker
        Operator))

(def TokenTemplate
    (.or 
        WhiteSpace 
        LineTerminator 
        Comment
        CommonToken 
        RightBracePunctuator
        LeftBracePunctuator
        LeftPrenPunctuator
        RightPrenPunctuator
        RightBracketPunctuator
        LeftBracketPunctuator))


(def (lex-error c . args)
    (.let* (p (point)) (apply error "Invalid Token:" c "at" (1- p) args)))


(def GlowToken
    (.or (.begin #!void (Token TokenTemplate))
        (.let* (v (.or #!eof ITEM)) (if (eof-object? v) FAIL (lex-error v)))))

(def (tokenize str) (run (many1 GlowToken) str))

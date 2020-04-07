;;;;; Compiler passes

(export #t)

;; TODO:
;; - In the near future, a strategy will not be a linear list of consecutive passes,
;;   but an arbitrary DAG with meets and joins and multiple final outputs,
;;   especially due to End-Point Projection. We ought to support that.
;; - In a further future, the "languages" below will be themselves types of the pass functions,
;;   and a "strategy" is a well-typed diagram written by composing these functions,
;;   all in a typed combinator language that still allows enough introspection for debugging,
;;   selection and inspection of the output, plus, hopefully, proof of correctness.
;; - Current "ancillary data" should be made of such "languages" too, using dependent types
;;   to describe how it relates to the "main" data.

(import
  :std/format :std/iter :std/misc/ports :std/misc/string
  :std/srfi/1 :std/srfi/13 :std/sugar
  :clan/utils/base :clan/utils/files
  :glow/compiler/common)

;; A language has a name, a reader and a writer.
;; - The name, a string, is the file extension of corresponding source files (e.g. ".glow").
;; - The reader, a function from Path to some representation type R (typically, type Stx of syntax objects),
;;   reads the file at given Path into a representation of given type.
;; - The writer, a function from R and optional Port to Unit, writes a representation to the given port.
;; Note that the writer *may* lose some semantically secondary information, such as source location.
;; The *languages* table maps the name to a list of the reader and writer.
(def *languages* (hash))
(def (register-language name reader writer) (hash-put! *languages* name [reader writer]))
(defrule (define-language name reader writer) (register-language name reader writer))

;; A pass has a name, a transformation function, and input and an output.
;; - The name, a symbol, identifies the pass.
;; - The input and the output, each strings, are the names of languages defined as above.
;; - The function takes a representation for the input, plus some optional ancillary data,
;;   and returns one or multiple values; the first of which is a representation for the output,
;;   and the rest is further ancillary data to pass to the next pass.
;; The *passes* table maps the name to a list of the function, input and output.
(def *passes* (hash))
(def (register-pass name fun input output) (hash-put! *passes* name [fun input output]))
(defrule (define-pass name input output) (register-pass 'name name input output))

;; A strategy has a name, and a list of passes.
;; - The name, a string, names the strategy
;; - the list of passes, by name (symbol).
;; The *strategy* table maps the name to the list of passes.
(def *strategies* (hash))
(def (register-strategy name passes) (hash-put! *strategies* name passes))
(defrule (define-strategy name pass ...) (register-strategy 'name '(pass ...)))

(def default-strategy (values #f))

;; String Path → LanguageRepresentation
(def (read-file/lang lang filename)
  ((first (hash-get *languages* lang)) filename))

;; LanguageRepresentation Port → Unit
(def (write/lang lang representation (port (current-output-port)))
  ((second (hash-get *languages* lang)) representation port))

;; Picks the longest matching known language suffix for a given filename.
;; Thus, for "foo.sexp", return ".sexp", but for "foo.alpha.sexp" return ".alpha.sexp".
;; Path -> String
(def (identify-language filename)
  (def lang #f) (def len -1)
  (for (k (hash-keys *languages*))
    (def kl (string-length k))
    (when (and (string-suffix? k filename)
               (< len kl))
      (set! lang k)
      (set! len kl)))
  lang)

;; If the element is in the list,
;; return all the list elements until and including its first occurrence
;; otherwise return #f
;; TODO: move that to clan/utils/list ?
;; A (Listof A) -> (Or (Listof A) '#f)
(def (member/take x lis)
  (def rmember (member x (reverse lis)))
  (and rmember (reverse rmember)))

;; Compute relevant passes to run to go from a program representation in the start-lang
;; along the named strategy until the specified last-pass (or if false, all the subsequent passes).
;; This only works if the passes can infer any ancillary data as optional arguments.
;; At this time in practice, that only works with start-lang being the ".sexp" language.
;; String Symbol (Or Symbol '#f) -> (Listof Symbol)
(def (relevant-passes start-lang strategy last-pass)
  (def strategy-passes
    (or (hash-get *strategies* strategy) (error 'no-such-strategy strategy)))
  (def passes-from-lang
    (or (find-tail (λ (pass) (equal? start-lang (second (hash-get *passes* pass))))
                   strategy-passes)
        (error 'no-such-language-in-strategy start-lang strategy)))
  (if last-pass
    (or (member/take last-pass passes-from-lang)
        (error 'no-such-pass-in-strategy last-pass strategy))
    passes-from-lang))

;; Symbol String Representation+AncillaryDataIn -> Representation+AncillaryDataOut
(def (run-pass pass basename state)
  (match (hash-get *passes* pass)
    ([fun _ output]
     (def new-state (values->list (apply fun state)))
     (def new-prog (first new-state))
     (def expected-output-file (string-append basename output))
     (when (file-exists? expected-output-file)
       (let ((new-prog (first new-state)))
         (unless (stx-sexpr=? new-prog (read-file/lang output expected-output-file))
           (eprintf "output for pass ~a doesn't match expectation from ~a:\n"
                    pass expected-output-file)
           (write/lang output new-prog (current-error-port))
           (error 'pass-output-mismatch pass basename))))
     new-state)))

;; Path lang:  String Representation+AncillaryDataIn -> Representation+AncillaryDataOut
(def (run-passes filename
                 lang: (lang (identify-language filename))
                 strategy: (strategy default-strategy)
                 pass: (last-pass #f)
                 show?: (show? #t)
                 save?: (save? #f))
  (def passes (relevant-passes lang strategy last-pass))
  (def in [(read-file/lang lang filename)])
  (def basename (string-trim-suffix lang filename))
  (def out (for/fold (state in) ((pass passes)) (run-pass pass basename state)))
  (def last-lang (third (hash-get *passes* (last passes))))
  (def (write-last port) (write/lang last-lang (first out) port))
  (when show? (write-last (current-output-port)))
  (when save? (clobber-file (string-append basename last-lang) write-last))
  out)

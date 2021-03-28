(export
  answer-questions
  read-environment)
;; Utility module for using Glow's interactive prompts programmatically,
;; for the purposes of integration testing (for "real" development you
;; should instead use programmatic interfaces directly).

(import :std/pregexp)

(def (answer-questions q-and-as)
  ;; Read a series of questions from (current-input-port), and feed
  ;; the corresponding answers to (current-output-port).
  ;;
  ;; The questions should be like:
  ;;
  ;;   Choose your role:
  ;;   1) Buyer
  ;;   2) Seller
  ;;   Enter number:
  ;;
  ;; The argument should be an association list mapping questions
  ;; (in the above example "Choose your role:") to textual answers
  ;; (in the above, "Buyer" or "Seller").
  ;;
  ;; An error is rased if any of the questions are presented out
  ;; of sequence or are missing.
  (map
    (lambda (q-and-a)
      (answer-question
        (expect-question (car q-and-a))
        (cadr q-and-a)))
    q-and-as))

(def (drop-until matches?)
  ;; Skip past any lines in the input that don't match the predicate `matches?`,
  ;; and return the first line that does.
  (def line (read-line))
  (if (matches? line)
    line
    (drop-until matches?)))

;; TODO(isd): this causes an error because Hashof isn't defined in any of our
;; imports figure out where it lives.
#;(define-type Question
  ;; A question that has been parsed from the input.
  (Record
    ;; The prompt for the question, e.g. "Choose your role:":
    prompt: String

    ;; A mapping from the textual answers to the numeric option
    ;; that must be entered to choose that answer:
    options: [Hashof String String]))

(defstruct question
  (prompt  ;; The prompt for the question, e.g. "Choose your role:":
   options ;; A hash table mapping the textual answers to the numeric option
           ;; (as a string) that must be entered to choose that answer:
  ))

(def (read-question)
  ;; Read in a Question object.
  (def prompt
    (drop-until (lambda (line) (string-prefix? "Choose " line))))
  (def options
    (read-options))
  (make-question prompt options))

(def (read-options)
  ;; Helper for read-question; reads in the hash table for the
  ;; options slot.
  (def table (make-hash-table))
  (def (read-all)
    (def line (read-line))
    (unless (string-prefix? "Enter " line)
      (match (pregexp-match "^([0-9]+)\\) (.*)$" line)
        ([_ no value]
         (hash-put! table value no))
        (error "expected option like 1) value, but got " line))
      (read-all)))
  (read-all)
  table)

(def (expect-question expected-prompt)
  ;; Like read-question, but expects a specific prompt, and raises
  ;; an error if the prompt is different.
  (def question (read-question))
  (def actual-prompt (question-prompt question))
  (unless (string=? actual-prompt expected-prompt)
    (error
      "Unexpect question prompt: expected "
      expected-prompt
      " but got "
      actual-prompt))
  question)

(def (answer-question question answer)
  ;; Answer a Question object "question" with the provided answer.
  (def option-num (hash-ref (question-options question) answer))
  (displayln option-num))


(def (read-environment)
  ;; Finds the environment logged at the end of a cli run, parses
  ;; it, and returns it as a hash table.
  (drop-until
    (lambda (line) (string=? line "Final environment:")))
  (def table (make-hash-table))
  (def (read-all)
    (def line (read-environment-line))
    (match line
      ([key value]
       (hash-put! table key value)
       (read-all))
      (_ (void))))
  (read-all)
  table)

(def (read-environment-line)
  ;; helper for read-environment; reads a single line.
  (def key (read))
  (if (equal? key #!eof)
    #!eof
    (begin
      (read) ; skip over the =>
      [key (read)])))

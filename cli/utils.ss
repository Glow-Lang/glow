(export #t)
;; TODO the contents of this file should be upstreamed to gerbil-utils/cli
;; Not upstreamed yet because there is a dependency on
;; :glow/runtime/terminal-codes (which should be upstreamed eventually as well).
(import :clan/base :gerbil/gambit :mukn/glow/runtime/terminal-codes)

(def (display-prompt name)
  (displayln CYAN name)
  (display (string-append "> " END))
  (force-output))

(def (ask-string name)
  (display-prompt name)
  (def result (read-line))
  (displayln)
  result)

;; If option is undefined, request it
(def (get-or-ask options option ask-function)
  (or (hash-get options option)
    (let (input (apply ask-function []))
      (hash-put! options option input)
      input)))

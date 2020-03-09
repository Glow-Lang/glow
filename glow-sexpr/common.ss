(export #t)

(import :gerbil/gambit/bytes
        <expander-runtime>)

(def (stx-atomic-literal? v)
  (def e (stx-e v))
  (or (integer? e) (string? e) (bytes? e) (boolean? e)))

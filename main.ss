(export #t)

(import :mukn/glow/all-glow :clan/multicall)

(def (main . args)
  (apply call-entry-point args))

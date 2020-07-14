(export #t)

(import :glow/all-glow :utils/multicall)

(def (main . args)
  (apply call-entry-point args))

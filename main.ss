(export #t)

(import :glow/all-glow :clan/utils/multicall)

(def (main . args)
  (apply call-entry-point args))

(export #t)

(import
  :gerbil/gambit/system
  :clan/utils/base :clan/utils/source)
(import :std/misc/repr)

(def build-time-glow-src (path-normalize (path-expand ".." (path-directory (vector-ref (this-source-location) 0)))))

(def (glow-src) (getenv "GLOW_SRC" build-time-glow-src))

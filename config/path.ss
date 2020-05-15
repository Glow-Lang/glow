(export #t)

(import
  :gerbil/gambit/system
  :clan/utils/base :clan/utils/source :clan/utils/filesystem
  :clan/utils/path :clan/utils/path-config)

(def build-time-glow-src (path-parent (path-normalized-directory (this-source-file))))
(def (glow-src) (getenv "GLOW_SRC" build-time-glow-src))
(def (glow-home) (getenv "GLOW_HOME" (glow-src)))
(set! source-directory glow-src)
(set! home-directory glow-home)
(set! config-directory (lambda () (path-expand "etc" (home-directory))))

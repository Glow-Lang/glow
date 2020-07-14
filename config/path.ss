(export #t)

(import
  :gerbil/gambit/system
  :utils/base :utils/source :utils/filesystem
  :utils/path :utils/path-config)

(def build-time-glow-src (path-parent (path-normalized-directory (this-source-file))))
(def (glow-src) (getenv "GLOW_SRC" build-time-glow-src))
(def (glow-home) (or (getenv "GLOW_HOME" #f) (glow-src)))
(set! source-directory glow-src)
(set! home-directory glow-home)
(set! config-directory (lambda () (path-expand "etc" (home-directory))))

(def (in-glow-src)
  (current-directory (glow-src))
  ((eval 'add-load-path) (glow-src)))

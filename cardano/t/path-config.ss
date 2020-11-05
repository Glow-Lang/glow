(export #t)

(import
  :gerbil/gambit/system
  :clan/path :clan/path-config :clan/source)

(def gerbil-cardano-src (path-parent (path-normalized-directory (this-source-file))))
(set! source-directory (lambda () gerbil-cardano-src))
(set! home-directory (lambda () gerbil-cardano-src))

(def (in-gerbil-cardano-src)
  (current-directory gerbil-cardano-src)
  ((eval 'add-load-path) gerbil-cardano-src))

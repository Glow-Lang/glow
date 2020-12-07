(export #t)

(import
  :gerbil/gambit/system
  :clan/base :clan/source :clan/filesystem
  :clan/path :clan/path-config)

;; TODO: nix: use ${pkgs.gerbilPackages-unstable.glow-lang.src} ?
;; TODO: no nix: use gxpkg's ${GERBIL_PATH:-$HOME/.gerbil}/pkg/gitlab.com/mukn/glow ?
(def build-time-glow-src (path-simplify-directory (this-source-file)))

(set! application-source-envvar "GLOW_SRC")
(set! application-home-envvar "GLOW_HOME")

(def (glow-src) (getenv "GLOW_SRC" build-time-glow-src))
(def (glow-home) (or (getenv "GLOW_HOME" #f) (glow-src)))
(set! source-directory glow-src)
(set! home-directory glow-home)

(def (in-glow-src)
  (current-directory (glow-src))
  ((eval 'add-load-path) (glow-src)))

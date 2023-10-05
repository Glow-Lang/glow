;; This file doesn't export anything, it required for its side-effects

(import
  ;; gerbil
  (only-in :std/misc/path path-simplify-directory)
  (only-in :std/source this-source-file)
  ;; gerbil-utils
  (only-in :clan/path-config application-name
           default-application-source-directory default-application-home-directory))

;; TODO: nix: use ${pkgs.gerbilPackages-unstable.glow-lang.src} ?
;; TODO: no nix: use gxpkg's ${GERBIL_PATH:-$HOME/.gerbil}/pkg/github.com/Glow-Lang/glow ?
(set! default-application-source-directory (path-simplify-directory (this-source-file)))
(set! default-application-home-directory default-application-source-directory)

(set! application-name (lambda () "glow"))

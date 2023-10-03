(export #t)

(import
  ;; gerbil
  :gerbil/gambit
  :std/misc/path
  :std/source
  ;; gerbil-utils
  :clan/filesystem
  :clan/path-config)

;; TODO: nix: use ${pkgs.gerbilPackages-unstable.glow-lang.src} ?
;; TODO: no nix: use gxpkg's ${GERBIL_PATH:-$HOME/.gerbil}/pkg/github.com/Glow-Lang/glow ?
(set! default-application-source-directory (path-simplify-directory (this-source-file)))
(set! default-application-home-directory default-application-source-directory)

(set! application-name (lambda () "glow"))

(export #t)

(import
  :gerbil/gambit/system
  :clan/base :clan/source :clan/filesystem
  :clan/path :clan/path-config)

;; TODO: nix: use ${pkgs.gerbilPackages-unstable.glow-lang.src} ?
;; TODO: no nix: use gxpkg's ${GERBIL_PATH:-$HOME/.gerbil}/pkg/github.com/Glow-Lang/glow ?
(set! default-application-source-directory (path-simplify-directory (this-source-file)))
(set! default-application-home-directory default-application-source-directory)

(set! application-name (lambda () "glow"))

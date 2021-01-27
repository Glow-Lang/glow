#!/usr/bin/env gxi
;; -*- Gerbil -*-
;;;; Run build and test in gitlab ci -- see glow/.gitlab-ci.ss

(import
  :gerbil/gambit/exceptions :gerbil/gambit/ports
  :std/format :std/getopt :std/misc/ports :std/misc/process :std/srfi/1 :std/sugar
  :clan/base :clan/exit :clan/debug :clan/files :clan/git-fu :clan/json
  :clan/multicall :clan/path :clan/path-config :clan/source :clan/versioning
  :mukn/ethereum/version)

;; Initialize paths from the environment
(def here (path-directory (path-maybe-normalize (this-source-file))))
(def glow-home (path-parent here))
(set! source-directory (lambda () glow-home))
(set! home-directory (lambda () glow-home))
(current-directory glow-home)
(register-software "Glow CI" "0.1")

(def tmpdir (getenv "TMPDIR" "/tmp"))

(def cachix-cache "mukn")
(def nix-store-pre-build (subpath tmpdir "store-path-pre-build"))

(def (enable-cachix)
  ;; NB: For now we trust developers who have keys to be honest.
  ;; When going to production, we will have to be less trusting
  ;; and only let trusted machines build. And/or use a different production cache pool.
  ;; https://app.cachix.org/cache/mukn
  (awhen (token (getenv "CACHIX_AUTH_TOKEN" #f))
    (printf "Using cachix for ~a\n" cachix-cache)
    (run-process/batch ["cachix" "authtoken" token])
    (run-process/batch ["cachix" "use" cachix-cache])))

(def (current-nix-store-paths)
  (run-process ["nix" "path-info" "--all"]
               stdin-redirection: #f stdout-redirection: #t
               coprocess: read-all-as-lines))

(def (show-env-vars tag)
  (def repo (git-origin-repo))
  (def repo-url (git-repo-url repo))
  (def normalized-repo-url (normalize-git-url repo-url))
  (DBG tag
       here glow-home (run-directory)
       repo repo-url normalized-repo-url
       (getenv "HOME" #f) (getenv "USER" #f) (getenv "UID" #f)
       (getenv "PWD" #f) (getenv "PATH" #f) (getenv "TMPDIR" #f)
       (getenv "GERBIL_LOADPATH" #f) (getenv "GERBIL_PATH" #f)))

(define-entry-point (before-build)
  "Prepare a build in Gitlab CI"
  (enable-cachix)
  (show-env-vars 'before-build:)
  (write-file-lines nix-store-pre-build (current-nix-store-paths)))

(define-entry-point (build)
  "Build in Gitlab CI"
  (displayln "build")
  ;; Compared to a plain "nix-build", this extracts nix version information from git.
  ;; NB: Like nix-build, this runs the unit-tests, but not integration tests.
  (run-process/batch ["./build.ss" "nix"])
  ;; Just print the version we got at the end of the build
  (run-process/batch ["nix-shell"
                      "--arg" "thunk" "false"
                      "--arg" "precompile" "true"
                      "--pure"
                      "--command" "./unit-tests.ss version"]))

(define-entry-point (after-build)
  "Build in Gitlab CI"
  (displayln "after-build")
  ;;(def previous-paths (read-file-lines nix-store-pre-build))
  ;;(def current-paths (current-nix-store-paths))
  ;;(def paths (lset-difference equal? current-paths previous-paths))
  (def paths
    (run-process ["nix" "path-info" "-r" "-f" "./pkgs.nix" "glow-lang"]
                 stdin-redirection: #f stdout-redirection: #t
                 coprocess: read-all-as-lines))
  (run-process/batch ["cachix" "push" cachix-cache . paths]))

(def (set-test-environment-variables)
  (let* ((glow-lang.out
          (run-process ["nix" "eval" "--raw" "-f" "pkgs.nix" "glow-lang"]
                       coprocess: read-all-as-string))
         (gerbil-ethereum.src
          (run-process ["nix" "eval" "--raw" "-f" "pkgs.nix"
                        "glow-lang.passthru.pre-pkg.gerbilEthereumSrc"]
                       coprocess: read-all-as-string))
         (loadpath (run-process ["nix" "eval" "--raw" "-f" "pkgs.nix"
                          "glow-lang.passthru.pre-pkg.testGerbilLoadPath"]
                         coprocess: read-all-as-string)))
    (setenv "GERBIL_LOADPATH" loadpath)
    (setenv "PATH" (format "~a/bin:~a" glow-lang.out (getenv "PATH" "")))
    (setenv "GERBIL_APPLICATION_HOME" glow-home)
    (setenv "GLOW_HOME" glow-home)
    (setenv "GLOW_SRC" glow-home)
    (values glow-lang.out gerbil-ethereum.src loadpath)))


(define-entry-point (before-test)
  "Prepare a test in Gitlab CI"
  (displayln "before-test")
  (enable-cachix)
  ;; For integration tests, nix-shell --pure causes us to run in an read-only directory,
  ;; so let' try it a slightly hard way...
  (run-process/batch ["./build.ss" "nix"]) ;; NB: this should use the path from cachix
  (defvalues (glow-lang.out gerbil-ethereum.src loadpath) (set-test-environment-variables))
  (def testnet.ss (subpath gerbil-ethereum.src "scripts/run-ethereum-test-net.ss"))
  #;(DBG before: gerbil-ethereum.src (file-exists? gerbil-ethereum.src) glow-lang.out)
  (unless (file-exists? gerbil-ethereum.src)
    (run-process/batch ["nix-build" "./pkgs.nix" "-A"
                        "gerbilPackages-unstable.gerbil-ethereum.src"]))
  #;(DBG after: (file-exists? gerbil-ethereum.src) testnet.ss (file-exists? testnet.ss))
  (create-directory* (run-directory))
  (show-env-vars "Running test net:")
  (run-process/batch [testnet.ss])
  (void))

(define-entry-point (test)
  "Test in Gitlab CI"
  (displayln "test")
  (set-test-environment-variables)
  ;; NB: Regular unit-tests should already have been taken care of by the build
  ;;;; (run-process/batch ["nix-shell" "--arg" "thunk" "false" "--arg" "precompile" "true" "--pure" "--command" "./unit-tests.ss"])
  ;; For integration tests, could we somehow run in a pure nix-shell?
  (run-process/batch ["./unit-tests.ss" "integration"])
  (run-process/batch ["make" "doc"])
  ;; NB: running the below outside a Nix shell so we can interact with the git repo
  (run-process/batch ["./unit-tests.ss" "check-git-up-to-date"]))

(define-entry-point (after-test)
  "Cleanup tests in Gitlab CI"
  (displayln "after-test")
  (ignore-errors (run-process/batch ["killall" "geth"])))

(define-entry-point (local-all)
  "Do it all locally on your machine"
  ;; Compared to CI: doesn't build from clean filesystem and environment,
  (before-build)
  (build)
  (after-build)
  ;; CI resets to a new docker image between build and test. Obviously we don't
  (before-test)
  (test)
  (after-test))

(def docker-image "mukn/glow:latest")

;; TODO: local build on docker
(define-entry-point (docker-all)
  "Build and test locally in docker containers, emulating CI"
  (docker-build)
  (docker-test))

;; Instead of extracting these and putting them in the too-visible environment,
;; just mount the appropriate configuration directory
;;;;(def (cachix-conf) (subpath (getenv "HOME") ".config/cachix/cachix.dhall"))
;;;;(def (get-cachix-auth-token) (json<-string (cadr (read-file-lines (cachix-conf)))))

(def (docker-run command)
  (run-process/batch ["docker" "run"
                      "-t" "-v" (format "~a:/root/glow-source" glow-home)
                      "-v" (format "~a/.config/cachix:/root/.config/cachix:ro" (getenv "HOME"))
                      ;;"-e" (format "CACHIX_AUTH_TOKEN=~a" (get-cachix-auth-token))
                      docker-image "bash" "-c" command]))

(define-entry-point (docker-build)
  "Build locally in a docker container, emulating CI"
  (docker-run "/root/glow-source/scripts/ci.ss in-docker-build"))

(define-entry-point (docker-test)
  "Build and test locally in docker containers, emulating CI"
  (docker-run "/root/glow-source/scripts/ci.ss in-docker-test"))

(define-entry-point (in-docker-setup)
  "(Internal) Setup local docker container"
  (run-process/batch ["sh" "-c" (string-append
                                 "rm -rf /root/glow ; "
                                 "cp -fax /root/glow-source /root/glow && "
                                 "cd /root/glow && "
                                 "git fetch --depth 50 /root/glow-source")])
  (current-directory "/root/glow"))

(define-entry-point (in-docker-build)
  "(Internal) Emulate CI build inside a local docker container"
  (in-docker-setup)
  (run-process/batch ["./scripts/ci.ss" "before-build"])
  (run-process/batch ["./scripts/ci.ss" "build"])
  (run-process/batch ["./scripts/ci.ss" "after-build"]))

(define-entry-point (in-docker-test)
  "(Internal) Emulate CI test inside a local docker container"
  (in-docker-setup)
  (run-process/batch ["./scripts/ci.ss" "before-test"])
  (run-process/batch ["./scripts/ci.ss" "test"])
  (run-process/batch ["./scripts/ci.ss" "after-test"]))

(set-default-entry-point! docker-all)
(backtrace-on-abort? #f)
(def main call-entry-point)

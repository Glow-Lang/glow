#!/usr/bin/env gxi
;; -*- Gerbil -*-
;; This is the main build file for Glow. Invoke it using
;; ./build.ss [cmd]
;; where [cmd] is typically left empty (same as "deps" if needed then "compile")

(import
  :std/build-script :std/srfi/1
  :std/misc/ports :std/misc/process :std/misc/repr :std/misc/string
  :clan/utils/base :clan/utils/filesystem)

(def glow-src (path-normalize (getenv "GLOW_SRC" (path-directory (this-source-file)))))
(setenv "GLOW_SRC" glow-src)
(current-directory glow-src)

(def (files)
  (append-map
   (λ (dir) (find-files dir
                        (λ (x) (equal? (path-extension x) ".ss"))
                        ;;recurse?: (λ (x) (not (equal? (path-strip-directory x) "t")))
                        ))
   ["config" "compiler" "runtime" "eth"]))

;; Update the version in config/version.ss:
;; If we have access to git (i.e. not from a tarball),
;; then overwrite config/version.ss based on the output of `git describe --tags`
;; unless it's unchanged.
;; If we don't have access to git, then whoever made the tarball better have
;; generated that file and included it in the tarball.
(define (update-version)
  (let* ((version-path "config/version.ss")
         (git-version
          (and (file-exists? ".git")
               (string-trim-eol (run-process '("git" "describe" "--tags")))))
         (version-text
          (and git-version
               (string-append "(import :clan/utils/version)\n"
                              "(register-software \"Glow\" \"" git-version "\")\n")))
         (previous-version-text
          (and version-text ;; no need to compute it if no current version to replace it with
               (file-exists? version-path)
               (read-file-string version-path))))
    (if (and version-text (not (equal? version-text previous-version-text)))
      (call-with-output-file [path: version-path create: 'maybe append: #f truncate: #t]
        (cut display version-text <>)))))

(def (main . args)
  (when (or (null? args)
            (member (first args) '("deps" "compile")))
    (update-version))
  (defbuild-script
    (append
     (files)
     '("all-glow.ss"
       (exe: "main" bin: "glow" static-exe: #t))))
  (apply main args))

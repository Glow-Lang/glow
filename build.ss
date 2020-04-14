#!/usr/bin/env gxi
;; -*- Gerbil -*-
;; This is the main build file for Glow. Invoke it using
;; ./build.ss [cmd]
;; where [cmd] is typically left empty (same as "deps" if needed then "compile")

(import
  :std/build-script
  :std/srfi/1
  :std/misc/ports :std/misc/process :std/misc/repr :std/misc/string
  :clan/utils/base :clan/utils/filesystem :clan/utils/version)

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

(def (main . args)
  (when (or (null? args)
            (member (first args) '("deps" "compile")))
    (update-version-from-git name: "Glow"))
  (defbuild-script
    (append
     (files)
     '("all-glow.ss"
       (exe: "main" bin: "glow" static-exe: #t))))
  (apply main args))

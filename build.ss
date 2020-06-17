#!/usr/bin/env gxi
;; -*- Gerbil -*-
;; This is the main build file for Glow. Invoke it using
;; ./build.ss [cmd]
;; where [cmd] is typically left empty (same as "compile")
;; Note that may you need to first:
;;   gxpkg install github.com/fare/gerbil-utils
;; See HACKING.md for details.

(import
  :std/build-script :std/format :std/srfi/1
  :clan/utils/filesystem :clan/utils/version
  "config/path")

(current-directory (glow-src))

(def (files)
  (append-map
   (cut find-files <>
        (lambda (x) (equal? (path-extension x) ".ss"))
        recurse?: (lambda (x) (not (equal? (path-strip-directory x) "t"))))
   ["config" "compiler" "runtime" "ethereum"]))

(def (build-spec)
  [(files) ...
   "t/common"
   "all-glow"
   [exe: "main" bin: "glow"]])

(def (main . args)
  (when (match args ([] #t) (["compile" . _] #t) (_ #f))
    (update-version-from-git name: "Glow"))
  (defbuild-script ;; defines an inner "main"
    ;;verbose: 9
    (build-spec))
  (apply main args))

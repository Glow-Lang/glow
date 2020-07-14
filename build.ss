#!/usr/bin/env gxi
;; -*- Gerbil -*-
;; This is the main build file for Glow. Invoke it using
;; ./build.ss [cmd]
;; where [cmd] is typically left empty (same as "compile")
;; Note that may you need to first:
;;   gxpkg install github.com/fare/gerbil-utils
;; See HACKING.md for details.

(import
  :std/build-script :std/format :std/misc/list :std/srfi/1
  :utils/filesystem :utils/versioning
  "config/path")

(current-directory (glow-src))

(def (files)
  (append-map
   (cut find-files <>
        (lambda (x) (equal? (path-extension x) ".ss"))
        recurse?: (lambda (x) (not (equal? (path-strip-directory x) "t"))))
   ["config" "compiler" "runtime" "ethereum"]))

(def gsc-options/no-optimize '("-cc-options" "-O0" "-cc-options" "-U___SINGLE_HOST"))
(def gsc-options/tcc '("-cc" "tcc" "-cc-options" "-shared"))

(def (tcc?) (case (getenv "USE_TCC" #f) (("y" "Y" "yes" "YES" "1") #t) (else #f)))
(def (optimize?) (case (getenv "USE_OPT" #f) (("n" "N" "no" "NO" "0") #f) (else #t)))
(def (debug?) (case (getenv "USE_DBG" #f) (("src") 'src) (("env") 'env) (else #f)))

(def gsc-options
  (append (when/list (tcc?) gsc-options/tcc)
          (when/list (not (optimize?)) gsc-options/no-optimize)))

(def (normalize-spec x)
  (match x
    ((? string?) [gxc: x . gsc-options])
    ([(? (cut member <> '(gxc: gsc: exe:))) . _] (append x gsc-options))))

(def (build-spec)
  (map normalize-spec
       [(files) ...
        "t/common" "version" "all-glow"
        [exe: "main" bin: "glow"]]))

(def (main . args)
  (defbuild-script ;; defines an inner "main"
    (build-spec)
    ;;verbose: 9
    debug: (debug?)
    optimize: (optimize?))
  (when (match args ([] #t) (["compile" . _] #t) (_ #f))
    ;; TODO: create a version.nix with git log -1 --pretty=%ad --date=short
    (update-version-from-git name: "Glow" path: "version.ss"))
  (apply main args))

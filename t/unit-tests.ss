(import
  :gerbil/gambit/ports
  :std/format :std/iter :std/misc/process
  :clan/utils/exit :clan/utils/ports
  :clan/t/test-support
  ;;:glow/compiler/syntax-context ;; important for the parsing to work (!)
  :glow/config/path
  :glow/compiler/passes :glow/compiler/multipass
  :glow/t/common)

(set-current-ports-encoding-standard-unix!)

(def (gerbil.pkg)
  (with-catch false (lambda () (call-with-input-file (path-expand "gerbil.pkg" (glow-src)) read))))

(def (git-origin-repo)
  (or (pgetq repo: (gerbil.pkg)) "origin"))

(def (git-origin-branch)
  (or (pgetq branch: (gerbil.pkg)) "master"))

(def (git-merge-base . commitishs)
  (run-process ["git" "merge-base" . commitishs] coprocess: read-line))

(def (main . args)
  (eval-print-exit
   (silent-exit
    (match args
      ([] (run-tests "."))
      (["meta"] (println "meta all test process pass"))
      (["all"] (run-tests "." test-files: (find-test-files ".")))
      (["integrationtests"] (run-tests "." test-files: (find-test-files "." "-integrationtest.ss$")))
      (["test" . files] (run-tests "." test-files: files))
      (["process" . files] (for-each run-passes files) #t)
      (["pass" pass . files]
       ;; Given a pass by name, and for each specified files,
       ;; identify the language in which the file is written, by file extension,
       ;; and run all compiler passes on the file until the named pass.
       ;; Run safety checks on the results of the specified pass,
       ;; and compare these results to pre-recorded results if available;
       ;; unless the pre-recorded results exist and match, print the pass results.
       ;; Either way, print test results.
       (def pass-sym (string->symbol pass))
       (for (file files) (run-passes file pass: pass-sym))
       #t)
      (["check_git_up_to_date"]
       (def branch (git-origin-branch))
       (run-process ["git" "fetch" "--depth" "1" (git-origin-repo) branch])
       (def up-to-date? (equal? (git-merge-base "FETCH_HEAD" "FETCH_HEAD")
                                (with-catch false (cut git-merge-base "HEAD" "FETCH_HEAD"))))
       (printf "Checkout~a up-to-date with branch ~a\n" (if up-to-date? "" " not") branch)
       up-to-date?)))))

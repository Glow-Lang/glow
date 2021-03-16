(export #t)

(import
  :std/getopt :std/iter :std/misc/hash :std/sugar
  :clan/cli :clan/config :clan/filesystem :clan/hash :clan/multicall
  :clan/path :clan/path-config :clan/string
  :clan/poo/cli)

(def glow-path #f)
(def glow-dapps #f)

(def (initialize-glow-path! (user-provided #f))
  (set! glow-path (or user-provided
                      (getenv-absolute-paths "GLOW_PATH")
                      [(source-path "dapps")])))

(def (initialize-glow-dapps!)
  (assert! glow-path "You must initialize-glow-path! before you initialize-glow-dapps!")
  (set! glow-dapps (hash))
  (for (top (reverse glow-path)) ;; reverse so earlier entries override later ones.
    (for (file (find-files top (cut path-extension-is? <> ".glow")))
      (let (name (path-strip-extension (path-enough file top)))
        (hash-put! glow-dapps name file)))))

(def (ensure-glow-dapps)
  (unless glow-dapps (initialize-glow-dapps!))
  glow-dapps)

(def options/glow-path
  (make-options [(option 'glow-path "-G" "--glow-path" help: "search path for Glow DApps"
                         value: (lambda (x) (filter path-absolute? (split-dirs x))))]
                [(lambda (opt) (initialize-glow-path! (hash-removed opt 'glow-path)))]))

;; List all of the applications in the search path.
(define-entry-point (list-applications)
  (help: "List interactions available in search path"
   getopt: options/glow-path)
  (def apps (hash->list/sort (ensure-glow-dapps) string<?))
  (for-each (lambda (n p) (displayln n "  " p)) (co-pad-strings (map car apps)) (map cdr apps)))

(def (find-glow-dapp dapp)
  (def relpath (string-append dapp ".glow"))
  (or (find file-exists? (map (cut subpath <> relpath) glow-path))
      (error "Glow dapp not found: " dapp)))

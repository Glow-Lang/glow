(export #t)

(import
  ;; gerbil
  (only-in :std/assert assert!)
  (only-in :std/getopt option)
  (only-in :std/iter for)
  (only-in :std/misc/hash hash->list/sort)
  (only-in :std/misc/path absolute-path? path-absolute? subpath path-enough)
  (only-in :std/misc/string string-trim-suffix)
  (only-in :std/sort sort)
  (only-in :std/srfi/13 string-suffix?)
  (only-in :std/sugar hash)
  ;; gerbil-utils
  (only-in :clan/filesystem find-files)
  (only-in :clan/hash hash-removed)
  (only-in :clan/multicall define-entry-point)
  (only-in :clan/config getenv-absolute-paths split-dirs xdg-data-home xdg-data-dirs)
  (only-in :clan/path-config source-path)
  (only-in :clan/string co-pad-strings)
  ;; gerbil-poo
  (only-in :clan/poo/cli make-options)
  ../path-config) ;; needed for side-effects

(def glow-install-path (source-path))
(def glow-path #f)
(def glow-dapps #f)

(def (default-glow-path)
  [(map (cut subpath <> "glow/dapps") [(xdg-data-home) (xdg-data-dirs) ...]) ...
   (subpath glow-install-path "dapps")])

(def (initialize-glow-path! (user-provided #f))
  (set! glow-path (or user-provided
                      (getenv-absolute-paths "GLOW_PATH")
                      (default-glow-path))))

(def (for-each-dapp-file f extension: (extension ".glow"))
  (assert! glow-path "You must initialize-glow-path! before you search for dapp files!")
  (for (top (reverse glow-path)) ;; reverse so earlier entries override later ones.
    (for (path (find-files top (cut string-suffix? extension <>)))
      (let (name (string-trim-suffix extension (path-enough path top)))
        (f name path)))))

(def (find-dapp-files extension: (extension ".glow") filter: (filter true))
  (def h (hash))
  (for-each-dapp-file
   extension: extension
   (lambda (name path)
     (when (filter name path)
       (hash-put! h name path))))
  h)

(def (initialize-glow-dapps!)
  (set! glow-dapps (find-dapp-files)))

(def (ensure-glow-dapps)
  (unless glow-dapps (initialize-glow-dapps!))
  glow-dapps)

(def (get-glow-app-names)
  (sort (hash-keys (ensure-glow-dapps)) string<?))

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

(define-entry-point (show-glow-path)
  (help: "Show Glow path"
   getopt: options/glow-path)
  (displayln (string-join glow-path ":")))

(def (find-dapp-path path)
  (if (absolute-path? path)
    (and (file-exists? path) path)
    (find file-exists? (map (cut subpath <> path) [(subpath ".") glow-path ...]))))

(def (find-dapp-file dapp (extension ".glow"))
  (def relpath (string-append dapp extension))
  (or (find-dapp-path relpath) (error "Glow DApp not found: " relpath)))

(define-entry-point ($default-glow-path)
  (help: "Print the default GLOW_PATH"
   getopt: [])
  (displayln (string-join (default-glow-path) ":")))

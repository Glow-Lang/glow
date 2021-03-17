(export #t)

(import
  :gerbil/gambit/misc
  :gerbil/expander
  :std/getopt :std/iter :std/sort
  :clan/base :clan/exit :clan/filesystem :clan/multicall :clan/path :clan/path-config
  :clan/persist/content-addressing
  :mukn/ethereum/hex
  :mukn/glow/path-config
  :mukn/glow/compiler/multipass
  :mukn/glow/compiler/passes
  :mukn/glow/runtime/glow-path)

;; dapps-dir
(def (dapps-dir)
  (source-path "dapps"))

;; dapps.sexp : -> [Listof Path]
(def (dapps.sexp)
  (def h (find-dapp-files
          extension: ".sexp" filter: (lambda (name path) (equal? (identify-layer path) 'sexp))))
  (sort (hash-keys h) string<?))

;; dapps.glow : -> [Listof Path]
(def (dapps.glow)
  (def h (find-dapp-files extension: ".glow"))
  (sort (hash-keys h) string<?))

(def (ppd x) (pretty-print (syntax->datum x)))

(def (ppdc x) (ppd (car x)))

(define-entry-point (process . files)
  (help: "Fully process given files through all compiler passes"
   getopt: [(rest-arguments 'files help: "files to process through the compiler")])
  (for-each run-passes files))

;; Given a pass by name, and for each specified files,
;; identify the language in which the file is written, by file extension,
;; and run all compiler passes on the file until the named pass.
;; Run safety checks on the results of the specified pass,
;; and compare these results to pre-recorded results if available;
;; unless the pre-recorded results exist and match, print the pass results.
;; Either way, print test results.
(define-entry-point (process-to-pass pass . files)
  (name: 'pass
   help: "Given a pass and files, process them until the end of it"
   getopt: [(argument 'pass help: "pass up to which to process the files" value: string->symbol)
            (rest-arguments 'files help: "files to process through the compiler")])
  (for (file files) (run-passes file pass: pass)))

;; TODO: make the choice of the digest algorithm controllable,
;; with the defualt dependent on the current network type, etc.
(define-entry-point (digest-command file)
  (name: 'digest
   help: "compute the cryptographic digest for a given file"
   getopt: [(argument 'file help: "file of which to compute the digest")])
  (displayln (0x<-bytes (digest<-file file))))

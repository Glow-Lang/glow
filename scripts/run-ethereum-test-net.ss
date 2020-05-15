#!/usr/bin/env gxi
;; Run your own local private copy of Ethereum as a node on localhost, for testing purposes

(import
  :gerbil/gambit/exceptions :gerbil/gambit/ports
  :std/format :std/getopt :std/misc/list :std/misc/ports :std/misc/process :std/srfi/13 :std/sugar
  :clan/utils/base :clan/utils/files :clan/utils/maybe :clan/utils/multicall :clan/utils/path-config
  :clan/net/json-rpc
  :glow/config/path)

(def geth-port 30303)
(def geth-rpc-port 8545)

;; We use `--dev.period 1` to prevent the `geth` miner from pausing
;; in the absence of pending transactions to process,
;; e.g. in the case of demo/test suite timeouts.
;;
;; Using the nominal block generation speed design goal one should expect to see
;; on the main net is painfully slow for demos/testing, so here we default to "1",
;; but CI should invoke this script with "12" for correctness' sake.
;;
;; https://blog.ethereum.org/2014/07/11/toward-a-12-second-block-time/
(def geth-dev-period 1)

;; First, kill any previous node.
(ignore-errors
 (run-process ["killall" "geth"]
              stdin-redirection: #t stdout-redirection: #t stderr-redirection: #t))

;; Determine the runtime directory, create it if needed
(def geth-run-directory (path-expand "ethereum" (run-directory)))
(create-directory* geth-run-directory)
(current-directory geth-run-directory)

;; Determine the data directory, clear it, thus resetting the test blockchain to zero
(def geth-data-directory (path-expand "data" geth-run-directory))
(run-process ["rm" "-rf" geth-data-directory]
             stdin-redirection: #t stdout-redirection: #t stderr-redirection: #t show-console: #f)
(create-directory* geth-data-directory)

(def geth-logs-directory (path-expand "logs" geth-run-directory))
(run-process ["rm" "-rf" geth-logs-directory]
             stdin-redirection: #t stdout-redirection: #t stderr-redirection: #t show-console: #f)
(create-directory* geth-logs-directory)

(def (ascii-alphanumeric? x)
  (or (char<=? #\A x #\Z) (char<=? #\a x #\z) (char<=? #\0 x #\9)))

(def (easy-shell-character? x)
  (or (ascii-alphanumeric? x) (string-index "+-_.,%@:/=" x)))

(def (needs-shell-escape? token)
  (not (string-every easy-shell-character? token)))

(def (escape-shell-token token)
  (if (needs-shell-escape? token)
    (call-with-output-string
     (lambda (port)
       (display #\" port)
       (string-for-each
        (lambda (c) (when (string-index "$`\\\"" c) (display #\\ port)) (display c port))
        token)
       (display #\" port)))
    token))

(def (escape-shell-tokens tokens)
  (string-join (map escape-shell-token tokens) " "))

(def geth-arguments
  ["--dev"
   (when/list (and geth-dev-period (< 0 geth-dev-period))
              ["--dev.period" (number->string geth-dev-period)])...
  "--mine"
  "--identity" "GlowEthereumPrivateTestNet"
  "--datadir" geth-data-directory
  "--nodiscover"
  "--maxpeers" "0"
  "--rpc"
  "--rpcapi" "db,eth,net,debug,web3,light,personal,admin"
  "--rpcport" (number->string geth-rpc-port)
  "--rpccorsdomain" "*"
  "--port" (number->string geth-port)
  "--nousb"
  "--networkid" "17"
  "--nat" "any"
  "--ipcpath" ".ethereum/geth.ipc"])

(create-directory* geth-logs-directory)

(def geth-process
  (open-process
   [path: "sh"
    arguments: ["-c"
                (format "~a < /dev/null > ~a/geth.log 2>&1 &"
                        (escape-shell-tokens ["geth" . geth-arguments])
                        geth-logs-directory)]
    stdin-redirection: #f
    stdout-redirection: #f
    stderr-redirection: #f
    show-console: #f]))

(let loop ()
  (cond
   ((with-catch false
                (cut json-rpc (format "http://localhost:~d" geth-rpc-port)
                     "web3_clientVersion" null))
    => (lambda (version) (printf "Connected to geth ~a\n" version)))
   (else
    (printf "Waiting for geth to start...\n")
    (thread-sleep! 1)
    (loop))))

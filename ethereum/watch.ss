;; General-purpose Ethereum Blockchain Watcher
(export #t)

(import
  :gerbil/gambit/threads
  :std/sugar
  :clan/utils/concurrency :clan/utils/exception
  :clan/poo/poo
  (only-in :clan/poo/number Integer)
  (only-in :clan/poo/type Map)
  :clan/runtime/persist
  ./types ./ethereum ./config ./json-rpc)

;; TODO: Handle "query returned more than 1000 results" when too many contracts created in interval!!!
;; TODO: Support watching multiple Ethereum-like networks in one image

;; : Quantity
(define-persistent-variable next-unprocessed-block Nat "ETH.nextUnprocessedBlock" 0)

;; : (Table (Fun <- Quantity) <- String)
(def new-block-hooks (make-hash-table))

;; Process new blocks.
;; : Unit <-
(def (process-new-blocks)
  (def current-block (eth_blockNumber))
  (with-logged-exceptions ()
   (when (>= current-block (next-unprocessed-block))
     (hash-for-each (lambda (_ hook) (hook (next-unprocessed-block) current-block)) new-block-hooks)
     (set! (next-unprocessed-block) (1+ current-block)))))

;; : Unit <-
(def (watchBlockchain)
  (spawn/name/logged
   "watch-ethereum-blockchain"
   (while #t
     (process-new-blocks)
     (thread-sleep! (.@ (current-ethereum-config) blockPollingPeriodInSeconds)))))

;; hook to synchronously watch all events of some kind as the chain keeps getting updated
(def (process-events filter process)
  (lambda (fromBlock toBlock)
    (set! fromBlock (max fromBlock 0))
    (when (or (not (real? toBlock)) (<= fromBlock toBlock))
      (for-each process (eth_getLogs (.cc filter fromBlock: fromBlock toBlock: toBlock))))))

;; Register a confirmed event hook.
;; NB: *IF* some event has hooks for both confirmed and unconfirmed events, then
;; (1) the confirmed event hook must be registered *before*, and
;; (2) the name of the confirmed event hook must be lexicographically strictly less
;; than the name for the corresponding unconfirmed event hook.
(def (register-confirmed-event-hook
      name fromBlock filter process
      (confirmations (.@ (current-ethereum-config) confirmationsWantedInBlocks)))
  (hash-put! new-block-hooks name
             (lambda (first-unprocessed-block last-unprocessed-block)
               ((process-events filter process)
                (- first-unprocessed-block confirmations)
                (- last-unprocessed-block confirmations))))
  ((process-events filter process) fromBlock (1- (next-unprocessed-block))))

(def (register-unconfirmed-event-hook
      name filter process
      (confirmations (.@ (current-ethereum-config) confirmationsWantedInBlocks)))
  (def hook (lambda (first-unprocessed-block last-unprocessed-block)
              ((process-events filter process)
               (max first-unprocessed-block (- last-unprocessed-block confirmations -1))
               'pending)))
  (hash-put! new-block-hooks name hook)
  (def fromBlock (- (next-unprocessed-block) 1 confirmations))
  (hook fromBlock 'pending))

;; The code in the section below might belong to some library to manage multiple interactions.
;; Managing interactions

;; runs up from 0, in the table of interactions
(define-persistent-variable next-interaction-id Integer "ETH.nextInteractionId" 0)
;; runs down from -1, in the same table of interactions
(define-persistent-variable previous-unconfirmed-id Integer "ETH.previousUnconfirmedId" -1)
(define-persistent-variable active-interactions NatSet "ETH.activeInteractions" (.@ NatSet .empty))
(define-type MapNatFromDigest (Map Nat <- Digest))
(define-persistent-variable interactions-by-txhash MapNatFromDigest "ETH.interactionsByTxhash" (.@ MapNatFromDigest .empty))

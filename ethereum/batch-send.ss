(export #t)

(import
  :gerbil/gambit/bytes
  :clan/utils/base :clan/poo/poo (only-in :clan/poo/mop) :clan/poo/io
  ./hex ./types ./ethereum ./known-addresses
  ./assembly ./transaction ./tx-tracker ./contract-config ./contract-support)

;;; EVM Contract for batch transfers.
;;
;;  Instead of sending a series of N transactions, have a single transaction that does it all.
;;  The advantage is that you have only one transaction to nurse to completion,
;;  which is a much less complex thing to do, with fewer and simpler failure scenarios to handle.
;;  This is especially important if your transaction posting code needs to deal with
;;  FOMO3D-style block-buying attacks, or other rapid gas price increase events.
;;
;;  The input data format does not use the Solidity ABI; we use a simpler and cheaper style:
;;  Just a raw vector of 32-byte records each comprised of a 20-byte address and a 12-byte value.
;;  No 4-byte header to identify a "function" to call; there's only one "function".
;;  No 32-byte vector count as first implicit argument; the size is taken from CALLDATASIZE.
;;  We never copy anything to memory; we just extract data to the stack.
;;
;;  Before we execute any transfer, we check that the sender matches the declared owner.
;;  Thus, if any money is sent to the contract or left in it, only the sender can later
;;  transfer that money out of the contract.
;;
;;  If any single transfer in the batch fails (because of lack of either funds or gas),
;;  revert the entire transaction (which cancels all the transfers that previously succeeded).

;; Create the runtime code for a batch contract associated to given owner
;; : Bytes <- Address
(def (batch-contract-runtime owner)
  (assemble
   [;; At instruction 0, so push 0 on stack while it's extra cheap!
    ;; a non-zero contract byte costs 220, a zero one only costs 204, so the GETPC optimization
    ;; is worth it if we actually use that 0 at least twice in the source code.
    GETPC #|0|# ; -- 0
    ;; Abort if the caller isn't the contract's owner
    [&address owner] CALLER EQ [jumpi1 'loop-init]
    DUP1 #|0|# REVERT

    ;; Initialize the loop invariant
    [jumplabel 'loop-init] ;; -- 0
    1 96 DUP2 #|1|# DUP1 #|1|# DUP3 #|96|# SHL SUB CALLDATASIZE DUP5
    ;; -- 0 size 2**96-1 96 1 0
    [jump1 'loop-entry] ;; jump to entry, skipping inter-loop action

    ;; The loop: inter-loop action
    [jumplabel 'loop]
    32 ADD

    ;; The entry point of the loop: check condition
    [jumplabel 'loop-entry] ;; -- cursor size 2**96-1 96 1 0
    ;; If less then continue to loop-body, else return
    DUP2 #|size|# DUP2 #|cursor|# LT [jumpi1 'loop-body] STOP

    ;; Loop body: take the next 256-bit argument.
    ;; Top 160 are address, lower 96 are value in wei.
    ;; Prepare the arguments to a transfer call.
    [jumplabel 'loop-body] ;; -- cursor size 2**96-1 96 1 0
    DUP6 #|0|# DUP1 #|0|# DUP1 #|0|# DUP1 #|0|# DUP5 #|cursor|# CALLDATALOAD
    DUP1 #|data|# DUP9 #|2**96-1|# AND
    ;; -- value data 0 0 0 0 cursor size 2**96-1 96 1 0
    SWAP1 #|data value|# DUP10 #|96|# SHR #|address|# GAS
    ;; Transfer! -- gas address value 0 0 0 0 cursor size 2**96-1 96 1 0
    CALL

    ;; loop if successful, revert everything if failed.
    [jumpi1 'loop]
    ;; -- cursor size 2**96-1 96 1 0
    DUP6 DUP1 REVERT]))

;; Given a constant contract runtime of length below 255,
;; that doesn't need any memory initialization, and doesn't contain functions we call,
;; return a contract initialization string, to be passed as parameter
;; to a CreateContract operation, to register the contract.
;; Beware: the code produced is not relocatable.

;; Create the runtime code for a batch contract associated to given owner
(def batch-contract-init (rcompose batch-contract-runtime constant-stateless-small-contract-init))

;; Ensure that there is a batch transfer contract associated with the owner
;; on the blockchain and saved to the working database, and
;; return the ContractConfig for that contract.
(def (ensure-batch-send-contract owner log: (log #f))
  (def config (ensure-contract-config/db
               (u8vector-append (string->bytes "BATC") owner)
               (create-contract owner (batch-contract-init owner))))
  (when log (log "batch contract for: " (0x<-address owner) " " (nickname<-address owner) " is: "
                  (json-string<- ContractConfig config)))
  config)

;; : <- Address (Listof (List Address UInt96))
(def (batch-send sender transfers log: (log #f))
  (def (fmt address amount)
    (bytes-append (bytes<- Address address)
                  (bytes<- UInt96 amount)))
  (def data (apply bytes-append (map (cut apply fmt <>) transfers)))
  (def value (foldl + 0 (map cadr transfers)))
  (when (< 0 value)
    (when log (log ["batch transfer" "value:" value "data:" (0x<-bytes data)]))
    (let (contract (.@ (ensure-batch-send-contract sender log: log) contract-address))
      (post-transaction (call-function sender contract data value: value)))))

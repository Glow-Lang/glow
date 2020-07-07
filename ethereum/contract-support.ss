(export #t)
(import
  :gerbil/gambit/bytes
  :std/sugar
  ./assembly)

;; Local memory can only be accessed 32-byte (or, for writes, also 1 byte) at a time,
;; and masking / merging is rather expensive, so for often-used stuff, it makes sense
;; to waste memory to save some gas. On the other hand, the cost of local memory is ultimately
;; quadratic in the total size, so for regular data, it pays to be compact.
;; Reading is cheap enough:
(def (&mload n-bytes)
  (&begin MLOAD (- 256 (* 8 n-bytes)) SHR)) ;; [4B, 9G]
(def (&mstore n-bytes)
  (def n-bits (* 8 n-bytes))
  ;;(&begin SWAP1 scratch0@ MSTORE DUP1 n-bytes ADD MLOAD scratch1@ MSTORE (- scratch1@ n-bytes) MLOAD SWAP1 MSTORE) ;; [17B, 39G]
  ;; [16B, 38G] -- note that we could skip the ending POP
  (&begin DUP1 n-bytes ADD MLOAD n-bits SHR DUP3 (- 256 n-bits) SHL OR SWAP1 MSTORE POP))
(def (&mstoreat n-bytes addr)
  (def n-bits (* 8 n-bytes))
  (&begin (- 256 n-bits) SHL (+ addr n-bytes) MLOAD n-bits SHR OR addr MSTORE)) ;; [15B, 36G]
(def &mstore16
  (&begin DUP2 8 SHR DUP2 MSTORE8 1 ADD MSTORE8)) ;; [10B, 24G]
(def (&mstore16at addr)
  (&begin DUP1 8 SHR addr MSTORE8 (1+ addr) MSTORE8)) ;; [12B, 21G]
(def &mstore24
  (&begin DUP2 16 SHR DUP2 MSTORE8 1 ADD &mstore16)) ;; [19B, 45G]
(def (&mstore24at addr)
  (&begin DUP1 16 SHR addr MSTORE8 (&mstore16at (1+ addr)))) ;; [20B, 36G]

;; Generic initialization code for stateless contracts of less than 255 bytes.
;; : Bytes <- Bytes
(def (constant-stateless-small-contract-init contract-runtime)
  (assert! (< (bytes-length contract-runtime) 256))
  (assemble
   [;; Push args for RETURN; doing it in this order saves one byte and some gas
    (bytes-length contract-runtime) 0 ;; memory address for the code
    ;; -- 0 length

    ;; Push args for CODECOPY; the DUP's for length and memory target are where the savings are
    DUP2 #| length |# [pushlabel1 'runtime-start] DUP3 ;; memory target address: 0
    ;; -- 0 start length 0 length

    ;; Initialize the contract by returning the memory array containing the runtime code
    CODECOPY RETURN

    ;; Inline code for the runtime as a code constant in the init code
    [&label 'runtime-start] #| @ 10 |# [&bytes contract-runtime]]))


;; Generic initialization code for stateful contracts of any allowable size (<= 24KiB),
;; where the initial state is a single merklized data point.
;; : Bytes <- Bytes32 Bytes
(def (simple-contract-init state-digest contract-runtime)
  (assemble
   [;; Save the state
    PUSH32 [&bytes state-digest] 0 SSTORE
    ;; Push args for RETURN; doing it in this order saves one byte and some gas
    PUSH2 [&int (bytes-length contract-runtime) 16] 0 ;; memory address for the code
    ;; -- 0 length

    ;; Push args for CODECOPY; the DUP's for length and memory target are where the savings are
    DUP2 #| length |# [pushlabel1 'runtime-start] DUP3 ;; memory target address: 0
    ;; -- 0 start length 0 length

    ;; Initialize the contract by returning the memory array containing the runtime code
    CODECOPY RETURN

    ;; Inline code for the runtime as a code constant in the init code
    [&label 'runtime-start] #| @ 10 |# [&bytes contract-runtime]]))

;; local-memory-layout for solidity:
;; 0x00 - 0x3f (64 bytes): scratch space for hashing methods
;; 0x40 - 0x5f (32 bytes): currently allocated memory size (aka. free memory pointer)
;; 0x60 - 0x7f (32 bytes): zero slot (why does solidity need that at all???)
;;
;; local-memory-layout for glow:
(def brk@ 0) ;; (32 bytes): brk, free memory pointer
(def calldatapointer@ 32) ;; (32 bytes): pointer within CALLDATA to yet unread published information
(def calldatanew@ 64) ;; (32 bytes): pointer to new information within CALLDATA (everything before was seen)
(def deposit@ 96) ;; (32 bytes): required deposit so far
(def frame@ 128) ;; or do we want it variable?
;; 128 - N-1: as many temporary variables as needed in the program, N is a program constant.
;; N - N+M: reserved for frame variables
;; M - end: heap

;; call stack has fixed layout:
;; - 0

;; We log the entire CALLDATA zone in one go. The upside is to save on extra 375 per LOG0 cost
;; and simplify the calling and publish convention, so we don't have to track and log individual messages.
;; The downside is the quadratic cost to memory, 3N+N^2/512.
;; Our strategy pays as long as we keep the memory under 438 words or so.
;; For large contracts with lots of data, it may pay to divide the logging into segments.
;; We'll figure that later (TODO!).
;; One solution would be to run the program first, then at the end,
;; log the CALLDATA starting with the new data only,
;; and decide on block size based on MSIZE.
;;
;; TODO: Don't log old merkleized data (top frame, but also other frames), but check it,
;; and log new data, albeit maybe in many chunks of ~400 words (~14KB) Find the optimal solution in gas.
;;
(def simple-contract-prelude
  [;; Init vs running convention!
   ;; [0] Put some values on stack while they're cheap.
   GETPC GETPC GETPC 240 ;; -- 240 2 1 0 [+5]
   ;; [5] Get state frame size, starting with PC, 16 bit
   DUP4 #|0|# CALLDATALOAD DUP2 #|240|# SHR frame@ ;; -- frame@ sz 240 2 1 0 [+6, assuming frame@<255]
   ;; [11] copy frame to memory
   DUP2 #|sz|# DUP5 #|2|# DUP3 #|frame@|# CALLDATACOPY ;; -- frame@ sz 240 2 1 0 [+4]
   ;; [15] store calldatapointer
   DUP2 #|sz|# DUP5 #|2|# ADD calldatapointer@ MSTORE ;; -- frame@ sz 240 2 1 0 [+6]
   ;; [21] save the brk variable
   DUP2 #|sz|# DUP2 #|frame@|# ADD DUP8 #|brk@,==0|# MSTORE ;; -- frame@ sz 240 2 1 0 [+6]
   ;; [27] compute the digest of the frame just restored
   SHA3 ;; -- digest 240 2 1 0 [+1]
   ;; [28] compare to saved merkleized state, jump to saved label if it matches
   DUP5 #|0|# SLOAD EQ (- frame@ 30) MLOAD JUMPI ;; [+7]
   [jumplabel 'abort-label] ;; [+1]
   ;; [36] Abort. We explicitly PUSH1 0 rather than DUPn because we don't assume stack position in general
   0 DUP1 REVERT]) ;; [+4]

;; Logging the data, simple version, optimal for messages less than 6000 bytes of data.
(def simple-logging
  [[jumplabel 'commit-label 40] ;; [+1]
   calldatanew@ MLOAD CALLDATASIZE DUP2 SUB ;; -- logsz cdn [+6]
   0 DUP2 #|logsz|# DUP4 #|cdn|# DUP3 #|0|# CALLDATACOPY LOG0 STOP])

;; Logging the data
(def variable-size-logging
  [[jumplabel 'commit-label 40] ;; [+1]
   ;; [41] compute available buffer size: max(MSIZE, n*256)
   ;; -- TODO: find out the optimal number to minimize gas, considering the quadratic cost of memory
   ;; versus the affine cost of logging, and the cost of this loop.
   ;; i.e. compute total logging gas depending on buffer size, differentiate, minimize
   ;; The marginal cost C of this loop is ~550 (linear logging costs are not marginal), total C*L/B.
   ;; The marginal memory cost beyond M is 3/32*B+B*B/Q, Q=524288. Minimize for B: C*L/B+B*B/Q+3/32*B
   ;; Let's neglect the quadratic term for now. To minimize, we cancel the derivative.
   ;; The optimal buffer size verifies -C*L/B^2 + 3/32 = 0, or B = sqrt(32*C*L/3) = sqrt(32*C/3)*sqrt(L)
   ;; sqrt(32*C/3) is about 77. Under about 6000B (the usual case?), it's always best to have a single log.
   ;; That's before the quadratic term kicks in.
   ;; Now, for large call data sizes, the quadratic term starts to matter:
   ;; 8M gas limit and about 22 g/byte mean that L < 360000 sqrt(L) < 600.
   ;; The optimal number neglecting the quadratic term goes up to 46200,
   ;; but at that point, the total two memory costs are comparable (about 4000 Gas).
   ;; The formula for optimal L with only the quadratic term is cubrt(Q*C/2)*cubrt(L),
   ;; which also tops at 38000 and grows more slowly.
   ;;
   ;; Instead of having the contract minimize a polynomial, we can just let the user
   ;; specify their buffer size as a parameter:
   ;; if they provide a bad answer, they are the ones who pay the extra gas (or fail).
   ;; This parameter can be a single byte, to be shifted left 7 or 8 times
   ;; -- getting it wrong is only 70-odd gas wrong,
   ;; less than it costs to use a second byte for precision.
   MSIZE 16384 DUP2 DUP2 GT [pushlabel1 'maxm1] JUMPI SWAP1 ;;[+11]
   [jumplabel 'maxm1 52] POP ;; -- bufsz [+2]
   calldatanew@ MLOAD CALLDATASIZE DUP2 SUB ;; -- logsz cdn bufsz [+6]
   ;; [60] Loop:
   [jumplabel 'logbuf 57] ;; -- logsz cdn bufsz [+1]
   ;; [61] If there's no more data, stop.
   DUP1 #|logsz|# [pushlabel1 'logbuf1] JUMPI STOP [jumplabel 'logbuf1 66] ;; -- logsz cdn bufsz [+6]
   ;; [67] compute the message size: msgsz = min(cdsz, bufsz)
   DUP3 #|bufsz|# DUP2 #|logsz|# LT [pushlabel1 'minbl] JUMPI SWAP1 ;; [+7]
   [jumplabel 'minbl 74] POP ;; -- msgsz logsz cdn bufsz [+1]
   ;; [75] Log a message
   DUP1 #|msgsz|# 0 DUP2 #|msgsz|# DUP6 #|cdn|# DUP3 #|0|# CALLDATACOPY LOG0 ;; -- msgsz logsz cdn bufsz [+8]
   ;; [83] Adjust logsz and cdn
   SWAP3 #|cdn logsz msgsz|# DUP3 #|msgsz|# ADD SWAP3 #|msgsz logsz cdn|# SWAP1 SUB ;; -- logsz cdn bufsz [+6]
   ;; [89] loop!
   [pushlabel1 'logbuf] JUMP]) ;; [+3]

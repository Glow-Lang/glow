(export #t)

(import
  :gerbil/gambit/bits :gerbil/gambit/bytes :gerbil/gambit/exact
  :std/misc/bytes :std/sugar
  :clan/utils/number :clan/poo/poo :clan/poo/io
  ./types ./ethereum)

;; In the future, segments can be nested, and
;; offset would be relative to a subsegment of a segment,
;; to be fully resolved when the segment size is finalized.
;; (Also, a non-embedded front-end syntax.)
;;
;; For now, everything is fixed size and we don't compute
;; label offsets much less optimize segment layout:
;; instead we merely *check* that labels are used properly
;; and the formulas match.

;; 24576, limit set by EIP 170 https://github.com/ethereum/EIPs/blob/master/EIPS/eip-170.md
(def max-segment-size #x6000)

;; TODO: use pure functional arrays? Doubly linked list of sub-segments? Balanced tree?
;; A segment is one of:
;; a u8vector, used literally as values
;; a list of a segment descriptor and segments
;; the descriptor contains min-start/max-start, min-length/max-length

;; an relocation expression is one of:
;; an integer, representing an address of offset
;; a symbol, label representing an unresolved address
;; a sum (+ x ...) of relocation expressions
;; a negation or difference (- x ...) of relocation expressions
(defstruct Segment
  (bytes ;; vector of bytes
   fill-pointer) ;; how many of those bytes are filled?
  transparent: #t)

(def (new-segment (size max-segment-size))
  (make-Segment (make-bytes size) 0))

(def (segment-full? s)
  (>= (Segment-fill-pointer s) (bytes-length (Segment-bytes s))))

(def (segment-push! s b)
  (when (segment-full? s) (error "segment full" 'segment-push! s b))
  (bytes-set! (Segment-bytes s) (Segment-fill-pointer s) b)
  (inc! (Segment-fill-pointer s)))

(def (segment-push-bytes! s b)
  (unless (< (+ (bytes-length b) (Segment-fill-pointer s))
             (bytes-length (Segment-bytes s)))
    (error "segment full" 'segment-push-bytes! s b))
  (subu8vector-move! b 0 (bytes-length b) (Segment-bytes s) (Segment-fill-pointer s))
  (inc! (Segment-fill-pointer s) (bytes-length b)))

(def (segment-contents s)
  (subu8vector (Segment-bytes s) 0 (Segment-fill-pointer s)))

;; a Fixup is a pair of an expression and a size in bits specifying how much space there is for the fixup,
;; at a given address which is used as key of the fixup table.
;; (deftype Fixup (Pair FixupExpression UInt24))

;; an Assembler has a Segment and/or buffer, a table from Symbol to LabelInformation,
;; and a table from address to fixup
(defstruct Assembler
  (segment ;; : Segment
   labels  ;; : (Table UInt16 <- Symbol)
   fixups) ;; : (Table Fixup <- UInt16)
  transparent: #t)
(def (new-assembler)
  (make-Assembler (new-segment) (make-hash-table) (make-hash-table)))

;; TODO: have chunks of code of constant or unknown but bounded length,
;; labels, fixups, displacements, etc.; compile-time merging of constant stuff(?)

;; (define-type Directive (Fun Unit <- Assembler))

(def fixup-functions
  (hash (+ +) (- -)))

(def (eval-fixup-expression labels expr)
  (match expr
    ((? number? x) x)
    ((? symbol? s) (hash-get labels s))
    ([f . l]
     (apply (hash-get fixup-functions f) (map (cut eval-fixup-expression labels <>) l)))))

(def (&byte a b)
  (segment-push! (Assembler-segment a) b))
(def (&bytes a b)
  (segment-push-bytes! (Assembler-segment a) b))
(def (&type a type x)
  (&bytes a ((.@ type .bytes<-) x)))
(def (&int a i (n-bytes (n-bytes<-n-bits (integer-length i))))
  (segment-push-bytes! (Assembler-segment a) (bytes<-nat i n-bytes)))
(def (&push a i (n-bytes (max 1 (n-bytes<-n-bits (integer-length i)))))
  (assert! (<= 1 n-bytes 32))
  (&byte a (+ #x5F n-bytes))
  (&int a i n-bytes))

(def (current-offset a)
  (Segment-fill-pointer (Assembler-segment a)))

(def (check-byte a offset value msg)
  (unless (= (bytes-ref (Segment-bytes (Assembler-segment a)) offset) value)
    (error msg)))

;; TODO: should we mask off all but the n-bits lowest bits of actual?
(def (check-uint a n-bits offset expected err)
  (def actual (u8vector-uint-ref (Segment-bytes (Assembler-segment a))
                                 offset big (n-bytes<-n-bits n-bits)))
  (unless (= actual expected)
    (err actual)))

(def (do-fixup a offset expr n-bits)
  (def value (eval-fixup-expression (Assembler-labels a) expr))
  (unless value
    (error "fixup has no computed value" offset expr n-bits value))
  (unless (and (<= 0 value) (< (integer-length value) n-bits))
    (error "fixup has incorrect computed value" offset expr n-bits value))
  (u8vector-uint-set! (Segment-bytes (Assembler-segment a)) offset value big (n-bytes<-n-bits n-bits))
  (hash-remove! (Assembler-fixups a) offset))

;; TODO: somehow check that fixup ranges don't overlap.
;; e.g. 32-bit fixup at address 10 and 8-bit fixup at address 12.
(def (&fixup a n-bits expr)
  (def offset (Segment-fill-pointer (Assembler-segment a)))
  (&int a 0 (n-bytes<-n-bits n-bits))
  (hash-put! (Assembler-fixups a) offset (cons expr n-bits)))

(def (&label a l)
  (def labels (Assembler-labels a))
  (def label-offset (hash-get labels l))
  (def offset (current-offset a))
  (if label-offset
    (error "label already defined" l offset label-offset)
    (hash-put! labels l offset)))

(def rev-opcodes (make-vector 256))
(def opcodes (make-hash-table))

(defrule (define-ethereum-bytecodes (code symbol . _) ...)
  (begin
    (begin (def (symbol a) (&byte a code))
           (hash-put! opcodes 'symbol code)
           (vector-set! rev-opcodes code 'symbol)
           (export symbol)) ...))

;; For precise semantics, see evm.md in https://github.com/kframework/evm-semantics
(define-ethereum-bytecodes
  (#x00 STOP 0) ;; Halts execution.
  (#x01 ADD 3) ;; Addition operation.
  (#x02 MUL 5) ;; Multiplication operation.
  (#x03 SUB 3) ;; Subtraction operation
  (#x04 DIV 5) ;; Integer division operation
  (#x05 SDIV 5) ;; Signed integer division operation (truncated)
  (#x06 MOD 5) ;; Modulo remainder operation
  (#x07 SMOD 5) ;; Signed modulo remainder operation
  (#x08 ADDMOD 8) ;; Modulo addition operation
  (#x09 MULMOD 8) ;; Modulo multiplication operation
  (#x0a EXP 10 #t) ;; Exponential operation
  (#x0b SIGNEXTEND 5) ;; Extend length of two's complement signed integer
  ;; #x0c - #x0f  Unused
  (#x10 LT 3) ;; Less-than comparison
  (#x11 GT 3) ;; Greater-than comparison
  (#x12 SLT 3) ;; Signed less-than comparison
  (#x13 SGT 3) ;; Signed greater-than comparison
  (#x14 EQ 3) ;; Equality comparison
  (#x15 ISZERO 3) ;; Simple not operator
  (#x16 AND 3) ;; Bitwise AND operation
  (#x17 OR 3) ;; Bitwise OR operation
  (#x18 XOR 3) ;; Bitwise XOR operation
  (#x19 NOT 3) ;; Bitwise NOT operation
  (#x1a BYTE 3) ;; Retrieve single byte from word
  (#x1b SHL #t) ;; logical shift left
  (#x1c SHR #t) ;; logical shift right
  (#x1d SAR #t) ;; arithmetic shift right
  ;; #x1e - #x1f  Unused
  (#x20 SHA3 30 #t) ;; Compute Keccak-256 hash
  ;; #x21 - #x2f  Unused
  (#x30 ADDRESS 2) ;; Get address of currently executing account
  (#x31 BALANCE 400) ;; Get balance of the given account
  (#x32 ORIGIN 2) ;; Get execution origination address
  (#x33 CALLER 2) ;; Get caller address
  (#x34 CALLVALUE 2) ;; Get deposited value by the instruction/transaction responsible for this execution
  (#x35 CALLDATALOAD 3) ;; Get input data of current environment
  (#x36 CALLDATASIZE 2 #t) ;; Get size of input data in current environment
  (#x37 CALLDATACOPY 3) ;; Copy input data in current environment to memory
  (#x38 CODESIZE 2) ;; Get size of code running in current environment
  (#x39 CODECOPY 3 #t) ;; Copy code running in current environment to memory
  (#x3a GASPRICE 2) ;; Get price of gas in current environment
  (#x3b EXTCODESIZE 700) ;; Get size of an account's code
  (#x3c EXTCODECOPY 700 #t) ;; Copy an account's code to memory
  (#x3d RETURNDATASIZE 2) ;; Pushes the size of the return data buffer onto the stack -- EIP 21
  (#x3e RETURNDATACOPY 3) ;; Copies data from the return data buffer to memory -- EIP 21
  (#x3f EXTCODEHASH #t) ;; ???
  (#x40 BLOCKHASH 20) ;; Get the hash of one of the 256 most recent complete blocks
  (#x41 COINBASE 2) ;; Get the block's beneficiary address
  (#x42 TIMESTAMP 2) ;; Get the block's timestamp
  (#x43 NUMBER 2) ;; Get the block's number
  (#x44 DIFFICULTY 2) ;; Get the block's difficulty
  (#x45 GASLIMIT 2) ;; Get the block's gas limit
  (#x46 CHAINID #t)
  (#x47 SELFBALANCE #t)
  ;; #x48 - #x4f  Unused
  (#x50 POP 2) ;; Remove word from stack
  (#x51 MLOAD 3 #t) ;; Load word from memory
  (#x52 MSTORE 3 #t) ;; Save word to memory
  (#x53 MSTORE8 3) ;; Save byte to memory
  (#x54 SLOAD 200) ;; Load word from storage
  (#x55 SSTORE 20000 #t #t) ;; Save word to storage. After refund, "only" 5000 per non-zero write.
  (#x56 JUMP 8) ;; Alter the program counter
  (#x57 JUMPI 10) ;; Conditionally alter the program counter
  (#x58 GETPC 2) ;; Get the value of the program counter prior to the increment
  (#x59 MSIZE 2) ;; Get the size of active memory in bytes
  (#x5a GAS 2) ;; the amount of available gas, including the corresponding reduction the amount of available gas
  (#x5b JUMPDEST 1) ;; Mark a valid destination for jumps
  ;; #x5c - #x5f  Unused
  (#x60 PUSH1 3) ;; Place 1 byte item on stack
  (#x61 PUSH2 3) ;; Place 2-byte item on stack
  (#x62 PUSH3 3) ;; Place 3-byte item on stack
  (#x63 PUSH4 3) ;; Place 4-byte item on stack
  (#x64 PUSH5 3) ;; Place 5-byte item on stack
  (#x65 PUSH6 3) ;; Place 6-byte item on stack
  (#x66 PUSH7 3) ;; Place 7-byte item on stack
  (#x67 PUSH8 3) ;; Place 8-byte item on stack
  (#x68 PUSH9 3) ;; Place 9-byte item on stack
  (#x69 PUSH10 3) ;; Place 10-byte item on stack
  (#x6a PUSH11 3) ;; Place 11-byte item on stack
  (#x6b PUSH12 3) ;; Place 12-byte item on stack
  (#x6c PUSH13 3) ;; Place 13-byte item on stack
  (#x6d PUSH14 3) ;; Place 14-byte item on stack
  (#x6e PUSH15 3) ;; Place 15-byte item on stack
  (#x6f PUSH16 3) ;; Place 16-byte item on stack
  (#x70 PUSH17 3) ;; Place 17-byte item on stack
  (#x71 PUSH18 3) ;; Place 18-byte item on stack
  (#x72 PUSH19 3) ;; Place 19-byte item on stack
  (#x73 PUSH20 3) ;; Place 20-byte item on stack
  (#x74 PUSH21 3) ;; Place 21-byte item on stack
  (#x75 PUSH22 3) ;; Place 22-byte item on stack
  (#x76 PUSH23 3) ;; Place 23-byte item on stack
  (#x77 PUSH24 3) ;; Place 24-byte item on stack
  (#x78 PUSH25 3) ;; Place 25-byte item on stack
  (#x79 PUSH26 3) ;; Place 26-byte item on stack
  (#x7a PUSH27 3) ;; Place 27-byte item on stack
  (#x7b PUSH28 3) ;; Place 28-byte item on stack
  (#x7c PUSH29 3) ;; Place 29-byte item on stack
  (#x7d PUSH30 3) ;; Place 30-byte item on stack
  (#x7e PUSH31 3) ;; Place 31-byte item on stack
  (#x7f PUSH32 3) ;; Place 32-byte (full word) item on stack
  (#x80 DUP1 3) ;; Duplicate 1st stack item
  (#x81 DUP2 3) ;; Duplicate 2nd stack item
  (#x82 DUP3 3) ;; Duplicate 3rd stack item
  (#x83 DUP4 3) ;; Duplicate 4th stack item
  (#x84 DUP5 3) ;; Duplicate 5th stack item
  (#x85 DUP6 3) ;; Duplicate 6th stack item
  (#x86 DUP7 3) ;; Duplicate 7th stack item
  (#x87 DUP8 3) ;; Duplicate 8th stack item
  (#x88 DUP9 3) ;; Duplicate 9th stack item
  (#x89 DUP10 3) ;; Duplicate 10th stack item
  (#x8a DUP11 3) ;; Duplicate 11th stack item
  (#x8b DUP12 3) ;; Duplicate 12th stack item
  (#x8c DUP13 3) ;; Duplicate 13th stack item
  (#x8d DUP14 3) ;; Duplicate 14th stack item
  (#x8e DUP15 3) ;; Duplicate 15th stack item
  (#x8f DUP16 3) ;; Duplicate 16th stack item
  (#x90 SWAP1 3) ;; Exchange 1st and 2nd stack items
  (#x91 SWAP2 3) ;; Exchange 1st and 3rd stack items
  (#x92 SWAP3 3) ;; Exchange 1st and 4th stack items
  (#x93 SWAP4 3) ;; Exchange 1st and 5th stack items
  (#x94 SWAP5 3) ;; Exchange 1st and 6th stack items
  (#x95 SWAP6 3) ;; Exchange 1st and 7th stack items
  (#x96 SWAP7 3) ;; Exchange 1st and 8th stack items
  (#x97 SWAP8 3) ;; Exchange 1st and 9th stack items
  (#x98 SWAP9 3) ;; Exchange 1st and 10th stack items
  (#x99 SWAP10 3) ;; Exchange 1st and 11th stack items
  (#x9a SWAP11 3) ;; Exchange 1st and 12th stack items
  (#x9b SWAP12 3) ;; Exchange 1st and 13th stack items
  (#x9c SWAP13 3) ;; Exchange 1st and 14th stack items
  (#x9d SWAP14 3) ;; Exchange 1st and 15th stack items
  (#x9e SWAP15 3) ;; Exchange 1st and 16th stack items
  (#x9f SWAP16 3) ;; Exchange 1st and 17th stack items
  (#xa0 LOG0 375) ;; Append log record with no topics
  (#xa1 LOG1 750) ;; Append log record with one topic
  (#xa2 LOG2 1125) ;; Append log record with two topics
  (#xa3 LOG3 1500) ;; Append log record with three topics
  (#xa4 LOG4 1875) ;; Append log record with four topics
  ;; #xa5 - #xef  Unused
  (#xf0 CREATE 32000) ;; Create a new account with associated code
  (#xf1 CALL Complicated) ;; Message-call into an account
  (#xf2 CALLCODE Complicated) ;; Message-call into this account with alternative account's code
  (#xf3 RETURN 0) ;; Halt execution returning output data
  (#xf4 DELEGATECALL Complicated) ;; Message-call into this account with an alternative account's code, but persisting into this account with an alternative account's code
  (#xf5 CALLBLACKBOX)
  ;; #xf6 - #xf9  Unused
  (#xfa STATICCALL 40) ;; Similar to CALL, but does not modify state
  (#xfb CREATE2 Complicated) ;; Create a new account and set creation address to sha3(sender + sha3(init code)) % 2
  (#xfd REVERT #t) ;; Stop execution and revert state changes, without consuming all provided gas and providing a reason
  (#xfe INVALID 0) ;; Designated invalid instruction
  (#xff SELFDESTRUCT 5000 #t)) ;; Halt execution and register account for later deletion

(def (jumplabel a l)
  (&label a l)
  (JUMPDEST a))
(def (pushlabel1 a l)
  (PUSH1 a)
  (&fixup a 8 l))
(def (pushlabel2 a l)
  (PUSH2 a)
  (&fixup a 16 l))
(def (jump1 a l)
  (pushlabel1 a l)
  (JUMP a))
(def (jump2 a l)
  (pushlabel2 a l)
  (JUMP a))
(def (jumpi1 a l)
  (pushlabel1 a l)
  (JUMPI a))
(def (jumpi2 a l)
  (pushlabel2 a l)
  (JUMPI a))
;; NB: fixed size for now, even if the address starts with zeros,
;; because the current assembler cannot deal with dynamic sizes
(def (&address a address)
  (&push a (nat<-bytes address)))
(def (&z a z)
  (cond
   ((and (> 0 z) (< (integer-length z) 240))
    (&z a (bitwise-not z))
    (NOT a))
   ((> 0 z)
    (&z a ((.@ UInt256 .normalize) z)))
   ((= 0 z)
    (PUSH1 a) (&byte a 0))
   (else
    (let ((n-bytes (n-bytes<-n-bits (integer-length z))))
      (assert! (<= n-bytes 32))
      (&byte a (+ #x5f n-bytes))
      (&bytes a (bytes<-nat z n-bytes))))))

(def (&directive a directive)
  (cond
   ((exact-integer? directive) (&push a directive))
   ((bytes? directive) (&bytes a directive))
   ((procedure? directive) (directive a))
   ((pair? directive) (apply (car directive) a (cdr directive)))
   (else (error "invalid directive" directive))))

(def (&directives a directives)
  (for-each (cut &directive a <>) directives))
(def (&begin . l) (lambda (a) (&directives a l)))

(def (assemble directives)
  (def a (new-assembler))
  (&directives a directives)
  (hash-for-each (lambda (offset fixup) (do-fixup a offset (car fixup) (cdr fixup)))
                 (Assembler-fixups a))
  (segment-contents (Assembler-segment a)))

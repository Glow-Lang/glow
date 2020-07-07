;; Support for Ethereum / Solidity contract ABI
;;    https://solidity.readthedocs.io/en/develop/abi-spec.html
;; This defines a standard way to serialize data structures for function call arguments,
;; logging, and function return values.
;; This file defines the plumbing for encoding and decoding.
;; Our type descriptors are actually declared in ./types and ./ethereum
;; See also in Rust: https://github.com/openethereum/ethabi
;; Finally, external representation of the abi is in abi-

(export #t)

(import
  :gerbil/gambit/bytes
  :std/misc/repr :std/srfi/1 :std/sugar
  :clan/utils/io :clan/utils/number :clan/poo/poo :clan/poo/mop
  :crypto/keccak)


;; for CallFunction:
;; Address should be a valid contract address.
;; For testing, it's a dummy address.
;; The bytes are a 4-byte prefix of the Keccak256 hash of the encoding of a method
;; signature, followed by the encoding of the method parameters, as described at:
;; < https://solidity.readthedocs.io/en/develop/abi-spec.html >
;; This data tells the EVM which method to call, with what arguments, in the contract
;; in this test, we just use a dummy hash to represent all of that

;; NIY: Not Implemented Yet
;;
;; The following elementary types exist:
;; [UInt<M>] uint<M>: unsigned integer type of M bits, 0 < M <= 256, M % 8 == 0. e.g. uint32, uint8, uint256.
;; [NIY] int<M>: two’s complement signed integer type of M bits, 0 < M <= 256, M % 8 == 0.
;; [Address] address: equivalent to uint160, except for the assumed interpretation and language typing. For computing the function selector, address is used.
;; [UInt] uint, [NIY] int: synonyms for uint256, int256 respectively. For computing the function selector, uint256 and int256 have to be used.
;; [Bool] bool: equivalent to uint8 restricted to the values 0 and 1. For computing the function selector, bool is used.
;; [NIY] fixed<M>x<N>: signed fixed-point decimal number of M bits, 8 <= M <= 256, M % 8 ==0, and 0 < N <= 80, which denotes the value v as v / (10 ** N).
;; [NIY] ufixed<M>x<N>: unsigned variant of fixed<M>x<N>.
;; [NIY] fixed, ufixed: synonyms for fixed128x18, ufixed128x18 respectively. For computing the function selector, fixed128x18 and ufixed128x18 have to be used.
;; [Bytes<M>] bytes<M>: binary type of M bytes, 0 < M <= 32.
;; [EthFunction] function: an address (20 bytes) followed by a function selector (4 bytes). Encoded identical to bytes24.
;;
;; The following (fixed-size) array type exists:
;; [Vector] <type>[M]: a fixed-length array of M elements, M >= 0, of the given type.
;;
;; The following non-fixed-size types exist:
;; [Bytes] bytes: dynamic sized byte sequence.
;; [String] string: dynamic sized unicode string assumed to be UTF-8 encoded.
;; [Vector] <type>[]: a variable-length array of elements of the given type.
;;
;; Types can be combined to a tuple by enclosing them inside parentheses, separated by commas:
;; [Tuple] (T1,T2,...,Tn): tuple consisting of the types T1, …, Tn, n >= 0
;; It is possible to form tuples of tuples, arrays of tuples and so on. It is also possible to form zero-tuples (where n == 0).

;; type AbiFunctionSignature = (Pair (name: String) (parameters: (List AbiType)))

;; : String <- AbiFunctionSelector
(def (string<-signature signature)
  (call-with-output-string
   (lambda (port)
     (display (car signature) port)
     (display #\( port)
     (ethabi-display-types (cdr signature) port)
     (display #\) port))))

(def (ethabi-display-types types port)
  (display-separated
   types
   port
   separator: ","
   display-element: (cut .call <> .ethabi-display-type <>)))

;; : Bytes32 <- AbiFunctionSignature
(def (digest<-function-signature signature)
  (keccak256<-string (string<-signature signature)))

;; : Bytes4 <- AbiFunctionSignature
(def (selector<-function-signature signature)
  (subu8vector (digest<-function-signature signature) 0 4))

(def (ethabi-head-length types)
  (reduce + 0 (map (cut .@ <> .ethabi-head-length) types)))
(def (ethabi-tail-length types xs)
  (reduce + 0 (map (cut .call <> .ethabi-tail-length <>) types xs)))
(def (ethabi-length types xs)
  (+ (ethabi-head-length types) (ethabi-tail-length types xs)))

;; : Bytes <- [Listof Type] [Listof Any] Bytes
;; types and xs are lists of the same length,
;; each x at index i corresponds to the type at index i.
;; The prefix is typically (a) the 4-bytes identifying a function call, or (b) an ABI-following contract
(def (ethabi-encode types xs (prefix #u8()))
  (def prefix-length (bytes-length prefix))
  (def head-length (ethabi-head-length types))
  (def tail-length (ethabi-tail-length types xs))
  (def tail (+ prefix-length head-length))
  (def end (+ tail tail-length))
  (def (get-tail) tail)
  (def (set-tail! new-tail)
    (assert! (<= tail new-tail end))
    (set! tail new-tail))
  (def bytes (make-bytes end))
  (subu8vector-move! prefix 0 prefix-length bytes 0)
  (ethabi-encode-into types xs bytes prefix-length prefix-length get-tail set-tail!)
  bytes)

;; : Void <- [Listof Type] [Listof Any] Bytes Nat Nat [-> Nat] [Nat -> Void]
;; types and xs are lists of the same length,
;; each x corresponds to the type at the same index.
;; start is the location that addresses are relative to, at the beginning of the current fixed-size header section,
;; head is the location within that header section at which the current/next entry will be stored.
;; get-tail and set-tail! are a getter/setter pair for where the next tail goes.
(def (ethabi-encode-into types xs bytes start head get-tail set-tail!)
  (for-each (lambda (type x)
              (.call type .ethabi-encode-into x bytes start head get-tail set-tail!)
              (inc! head (.@ type .ethabi-head-length)))
            types xs))

;; : [Listof Any] <- [Listof Type] Bytes Nat Nat
(def (ethabi-decode types bytes (start 0) (end (bytes-length bytes)))
  (def head-end (+ start (ethabi-head-length types)))
  (def tail head-end)
  (def (get-tail) tail)
  (def (set-tail! new-tail)
    (assert! (<= tail new-tail end))
    (set! tail new-tail))
  (begin0 (ethabi-decode-from types bytes start start get-tail set-tail!)
    (assert! (= tail end))))

;; : [Listof Any] <- [Listof Type] Bytes Nat Nat [-> Nat] [Nat -> Void]
;; start is the location that addresses are relative to, at the beginning of the current fixed-size header section,
;; head is the location within that header section at which the current/next entry will be stored.
;; get-tail and set-tail! are a getter/setter pair for where the next tail goes.
(def (ethabi-decode-from types bytes start head get-tail set-tail!)
  (def head-end (get-tail))
  (begin0 (map-in-order
           (lambda (type)
             (begin0 (.call type .ethabi-decode-from bytes start head get-tail set-tail!)
               (inc! head (.@ type .ethabi-head-length))))
           types)
    (assert! (= head head-end))))

;; : Bytes <- AbiFunctionSignature [Listof Any]
;; Encode a function call to pass it to an Ethereum contract
(def (bytes<-ethereum-function-call signature arguments)
  (ethabi-encode (cdr signature) arguments (selector<-function-signature signature)))

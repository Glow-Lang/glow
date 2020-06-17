;;;-*- Gerbil -*-
;;; Ethereum-style hexadecimal encoding of quantities and data.
;;
;; The following format represents data as hexadecimal strings with the prefix "0x".
;; It is defined as part of the Ethereum JSON-RPC protocol, variants of which are used
;; by go-ethereum, the parity client, etc. The definitional document is:
;;    https://github.com/ethereum/wiki/wiki/JSON-RPC#hex-value-encoding

;;
;; The Ethereum JSON-RPC 0x format comes in two distinct, incompatible, flavors,
;; that it is important to distinguish:
;;   - Natural numbers, that the Ethereum document calls "quantities", are represented
;;     with the minimal positive amount of hexits. Thus, the representation of a "quantity"
;;     will start with "0x0" if and only if it is zero.
;;   - Byte vectors, that the Ethereum document calls "unformatted data", are represented
;;     with an even number of hexits, exactly double the number of bytes in the vector,
;;     not counting the prefix "0x". The empty vector is represented as "0x".
;;
;; The two flavors are NOT interoperable, and it is important to know which flavor is used
;; for which data element. Unhappily, the servers we interoperate with may or may not agree
;; about which flavor is used where, and we may have to accommodate for that -- but this
;; is out of the scope of this file.
;;
;; Regarding numbers, note that although the "0x" format applies to numbers of any size,
;; in practice all numbers used by Ethereum fit in a uint256 (256-bit natural integer).
;; The 0x format has no provision for encoding negative numbers. Relative numbers must
;; first be encoded as natural numbers, which in Ethereum is typically done using
;; 2's complement within a fixed size unsigned integer, typically (always?) a uint256.
;; Encoding between (intervals of) relative integers and natural integers is out of the
;; scope of this file.
;;
;; Similarly, marshalling data into bytevectors, and unmarshalling data from them
;; is out of the scope of this file.
;;
;; See test vectors in t/hex-test.ss.
;;
;; NB: Conversion to this format is onerous. It should be avoided within internal computations,
;; and reserved for the purpose of using JSON-RPC, Web interfaces, debugging.

(export #t)

(import
  :gerbil/gambit/bits :gerbil/gambit/bytes
  :std/iter :std/misc/bytes :std/srfi/13 :std/text/hex
  :crypto/keccak)

;; Raise an error if the string doesn't strictly start with "0x"
;; : Unit <- 0xString
(def (validate-0x-prefix hs)
  (unless (string-prefix? "0x" hs)
    (error "Hex string does not begin with 0x" hs)))

;; Assuming the input string is a 0xString starting with "0x", remove that prefix
;; : 0xString <- 0xString
(def (remove-0x-from-string hs)
  (substring hs 2 (string-length hs)))

;; : 'a <- ('a <- 0xString) 0xString
(def (parse-0x-prefix parser hs)
  (validate-0x-prefix hs)
  (parser (remove-0x-from-string hs)))

;; : 0xString <- (0xString <- 'a) 'a
(def (unparse-0x-prefix unparser x)
  (string-append "0x" (unparser x)))

;; Decoding a "quantity"
;; : Nat <- 0xQuantityString
(def (nat<-0x hs)
  (validate-0x-prefix hs)
  (def len (string-length hs))
  (cond
   ((= len 2) (error "Hex quantity has no digits" hs)) ;; 0 is "0x0"
   ((eqv? (string-ref hs 2) #\0)
    (if (= len 3) 0 (error "Hex quantity has leading zero" hs)))
   (else
    (bytevector->uint (hex-decode (remove-0x-from-string hs)) big))))

;; Encoding a "quantity"
;; : 0xQuantityString <- Nat
(def (0x<-nat nat)
  (string-append "0x" (number->string nat 16)))

;; Decoding "unformatted data"
;; : Bytes <- 0xDataString
(def (bytes<-0x hs)
  (validate-0x-prefix hs)
  (def len (string-length hs))
  (when (odd? len) (error "Odd number of digits in hex string"))
  (hex-decode (remove-0x-from-string hs)))

;; Encoding "unformatted data"
;; : 0xDataString <- Bytes
(def (0x<-bytes s)
  (string-append "0x" (hex-encode s)))

;; Address format with builtin checksum, from EIP-55
;; https://github.com/ethereum/EIPs/blob/master/EIPS/eip-55.md
(def (0x<-address address)
  (def hex-digits (0x<-bytes address))
  (def hashed-digits (keccak256<-string (substring hex-digits 2 42)))
  (for (i (in-range 40))
    (def j (+ i 2))
    (def ch (string-ref hex-digits j))
    (when (and (char<=? #\a ch #\f)
               (not (zero? (bitwise-and (bytes-ref hashed-digits (arithmetic-shift i -1))
                                        (if (even? i) #x80 #x08)))))
      (string-set! hex-digits j (char-upcase ch))))
  hex-digits)

(def (validate-address-0x hs)
  ;; see https://www.quora.com/How-can-we-do-Ethereum-address-validation *)
  ;; which says to check a bit of the hash, but means byte
  (unless (string-prefix? "0x" hs) (error "Not a 0x string" hs))
  (unless (= (string-length hs) 42) (error "invalid address string length" hs))
  (def hex-digits (substring hs 2 42))
  (def hashed-digits (keccak256<-string (string-downcase hex-digits)))
  (for (i (in-range 40))
    (def ch (string-ref hex-digits i))
    (cond
     ((char<=? #\0 ch #\9) (void))
     ((not (or (char<=? #\a ch #\f) (char<=? #\A ch #\F)))
      (error "Invalid hex digit" hs (+ i 2)))
     ((not (eq? (char<=? #\a ch #\f)
                (zero? (bitwise-and (bytes-ref hashed-digits (arithmetic-shift i -1))
                                    (if (even? i) #x80 #x08)))))
      (error "Invalid address checksum" hs (+ i 2))))))

(def (address<-0x hs)
  (validate-address-0x hs)
  (bytes<-0x hs))

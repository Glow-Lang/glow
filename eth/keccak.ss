;;; -*- Scheme -*-
;;;; Gerbil FFI for keccak-tiny by David Leon Gil
;;
;; To be linked with: keccak-tiny.c OR keccak-tiny-unrolled.c from
;;     https://github.com/coruus/keccak-tiny
;; This file is released under CC0, after keccak-tiny itself.
;;     https://creativecommons.org/publicdomain/zero/1.0/
;; NB: The C files were extracted from upstream commit 64b6647514212b76ae7bca0dea9b7b197d1d8186
;;
;; Yes, keccak-tiny is limited, it requires the input data to be a single
;; contiguous memory buffer, preventing incremental hashing, or hashing of large files;
;; but for our intended purpose of decentralized applications the rules of which are
;; enforced by a "smart contract", we're limited by said "smart contracts",
;; that can only afford to handle a dozen kilobytes of data or two at once at the very most,
;; so we have to use Merkle trees to do anything deeper, any way.
;; On the upside, keccak-tiny is short and simple and easier to assess and to interface to.
;; In the future, we may or may not replace it with another implementation.

(export #t)

(import
  :gerbil/gambit/exceptions
  :std/foreign :std/misc/bytes :std/text/hex :std/sugar
  :clan/utils/base)

;; Return an u8vector of length 32 that represents the digest of the input bytes
;; (taken from the given u8vector with given start and end positions, by default the entire u8vector),
;; using the keccak256 algorithm used by Ethereum, as per the original entry that won
;; the SHA-3 competition by NIST, but differing from the later SHA-3 standard
;; in the value of a padding byte.
;; Note that the standard text representation of the digest can be obtained using function
;; std/text/hex#hex-encode, and that Ethereum usually precedes that representation by 0x
;; when transmitting digests in JSON-RPC, as per function glow/eth/hex#0x<-data.
;; : Bytes32 <- Bytes
(def (keccak256<-bytes b (start 0) (end (u8vector-length b)))
  (unless (and (u8vector? b) (<= 0 start end (u8vector-length b))) (error 'keccak256 b start end))
  (let ((result (make-u8vector 32)))
    (ffi-keccak-256 result 0 32 b start end)
    result))

;; Convenience function to compute the keccak256 hash of a string, as first reduced to bytes,
;; using the UTF-8 encoding by default. This function should probably only be used for debugging.
;; : Bytes32 <- String ?EncodingSymbol
(def (keccak256<-string s (encoding 'UTF-8))
  (keccak256<-bytes (string->bytes s encoding)))

;; TODO: once our API is stable enough, also support other digests in this family,
;; using some macro to generate all the support.

(begin-ffi (ffi-shake128 ffi-shake256
            ffi-sha3-224 ffi-sha3-256 ffi-sha3-384 ffi-sha3-512
            ffi-keccak-224 ffi-keccak-256 ffi-keccak-384 ffi-keccak-x512)

(c-declare #<<END-C
#include "eth/keccak-tiny-unrolled.c"

#ifndef ___HAVE_FFI_U8VECTOR
#define ___HAVE_FFI_U8VECTOR
#define U8_DATA(obj) ___CAST (___U8*, ___BODY_AS (obj, ___tSUBTYPED))
#define U8_LEN(obj) ___HD_BYTES (___HEADER (obj))
#endif

#define ffifun(fun) \
  static int ffi_ ## fun (___SCMOBJ out_bytes, int out_start, int out_end, ___SCMOBJ in_bytes, int in_start, int in_end) { \
    return fun (U8_DATA (out_bytes) + out_start, out_end - out_start, \
                U8_DATA (in_bytes) + in_start, in_end - in_start); \
  }

ffifun(shake128)
ffifun(shake256)
ffifun(sha3_224)
ffifun(sha3_256)
ffifun(sha3_384)
ffifun(sha3_512)
ffifun(keccak_224)
ffifun(keccak_256)
ffifun(keccak_384)
ffifun(keccak_512)
END-C
)

(define-macro (def-ffi name num)
  (let ((sym (string->symbol (string-append "ffi-" (symbol->string name) "-" (number->string num))))
        (cname (string-append "ffi_" (symbol->string name) (if (eq? name 'shake) "" "_") (number->string num))))
    `(define-c-lambda ,sym (scheme-object int int scheme-object int int) void ,cname)))
(def-ffi shake 128)
(def-ffi shake 256)
(def-ffi sha3 224)
(def-ffi sha3 256)
(def-ffi sha3 384)
(def-ffi sha3 512)
(def-ffi keccak 224)
(def-ffi keccak 256)
(def-ffi keccak 384)
(def-ffi keccak 512)
);foreign

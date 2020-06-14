(export #t)

(import :gerbil/gambit/bytes
        :std/test
        :std/text/hex
        :glow/crypto/keccak)

;; keccak-test : TestSuite
(def keccak-test
  (test-suite "test suite for glow/crypto/keccak"
    (test-case "digest test vectors for keccak256"
      (for-each (match <> ([h s] (check-equal? (hex-encode (keccak256<-string s)) h)))
                [;; Vectors from https://github.com/ethereum/eth-hash/blob/master/tests/backends/test_results.py
                 ["c5d2460186f7233c927e7db2dcc703c0e500b653ca82273b7bfad8045d85a470" ""]
                 ["b1f354b28ff28452d6b9d61f41061bbe82beb1fc98f33364a8058d1a5d164d05" "arsttsra"]
                 ["c3305bc9de1244e48050962ced50b75934c37006e99e8b7a62213e945c3dfcd7" "arst"]
                 ;; Vectors from https://github.com/xavierleroy/cryptokit/blob/master/test/test.ml
                 ["4e03657aea45a94fc7d47ba826c8d667c0d1e6e33a64a036ec44f58fa12d6c45" "abc"]
                 ["45d3b367a6904e6e8d502ee04999a7c27647f91fa845d456525fd352ae3d7371" "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq"]
                 ["f519747ed599024f3882238e5ab43960132572b7345fbeb9a90769dafd21ad67" "abcdefghbcdefghicdefghijdefghijkefghijklfghijklmghijklmnhijklmnoijklmnopjklmnopqklmnopqrlmnopqrsmnopqrstnopqrstu"]
                 ;; Other random test
                 ["9fd09c38c2a5ae0a0bcd617872b735e37909ccc05c956460be7d3d03d881a0dc" "this is a test"]])
      (check-equal? (hex-encode (keccak256<-bytes #u8(0 0 0 0 0 0 0 1)))
                    "6c31fc15422ebad28aaf9089c306702f67540b53c7eea8b7d2941044b027100f"))))

(export #t)

(import :gerbil/gambit/bytes
        :std/test
        :std/text/hex
        :glow/crypto/keccak)

;; keccak-test : TestSuite
(def keccak-test
  (test-suite "test suite for glow/crypto/keccak"
    (test-case "digest test vectors for keccak256"
      (for-each (match <> ([s h] (check-equal? (hex-encode (keccak256<-string s)) h)))
                [["" "c5d2460186f7233c927e7db2dcc703c0e500b653ca82273b7bfad8045d85a470"]
                 ["abc" "4e03657aea45a94fc7d47ba826c8d667c0d1e6e33a64a036ec44f58fa12d6c45"]
                 ["this is a test" "9fd09c38c2a5ae0a0bcd617872b735e37909ccc05c956460be7d3d03d881a0dc"]
                 ["abcdefghbcdefghicdefghijdefghijkefghijklfghijklmghijklmnhijklmnoijklmnopjklmnopqklmnopqrlmnopqrsmnopqrstnopqrstu" "f519747ed599024f3882238e5ab43960132572b7345fbeb9a90769dafd21ad67"]])
      (check-equal? (hex-encode (keccak256<-bytes #u8(0 0 0 0 0 0 0 1)))
                    "6c31fc15422ebad28aaf9089c306702f67540b53c7eea8b7d2941044b027100f"))))

(export hex-test)

(import
  :gerbil/gambit/bytes
  :gerbil/gambit/exceptions
  :std/sugar
  :std/error :std/text/hex :std/test :std/srfi/1
  :glow/ethereum/hex)

(def hex-test
  (test-suite "test suite for glow/ethereum/hex"
    (test-case "nat<-0x, 0x<-nat"
      (for-each (match <>
                  ([dec hex]
                   (check-equal? dec (nat<-0x hex))
                   (check-equal? hex (0x<-nat dec))))
                [[0 "0x0"]
                 [10 "0xa"]
                 [3735928559 "0xdeadbeef"]
                 [291 "0x123"]
                 [8271117963530313756381553648673 "0x68656c6c6f2c20776f726c6421"]])
      (for-each (match <>
                  ([hex err]
                   (check-equal? err (with-catch error-exception-message (cut nat<-0x hex)))))
                [["0" "Hex string does not begin with 0x"]
                 ["" "Hex string does not begin with 0x"]
                 ["0x" "Hex quantity has no digits"]
                 ["0x0123" "Hex quantity has leading zero"]]))

    (test-case "bytes<-0x, 0x<-bytes"
      (for-each (match <>
                  ([dec hex]
                   (check-equal? dec (bytes<-0x hex))
                   (check-equal? hex (0x<-bytes dec))))
                [[#u8() "0x"]
                 [#u8(0) "0x00"]
                 [#u8(0 0) "0x0000"]
                 [#u8(1 35) "0x0123"]
                 [(@bytes "abcd") "0x61626364"]
                 [(@bytes "\r\n") "0x0d0a"]
                 [(@bytes "hello, world!") "0x68656c6c6f2c20776f726c6421"]])
      (for-each (match <>
                  ([hex err]
                   (check-equal? err (with-catch error-exception-message (cut bytes<-0x hex)))))
                [["0" "Hex string does not begin with 0x"]
                 ["" "Hex string does not begin with 0x"]
                 ["004200" "Hex string does not begin with 0x"]
                 ["0x0" "Odd number of digits in hex string"]
                 ["0xf0f0f" "Odd number of digits in hex string"]]))
    ))

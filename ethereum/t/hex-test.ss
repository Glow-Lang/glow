(export hex-test)

(import
  :gerbil/gambit/bytes
  :gerbil/gambit/exceptions
  :std/sugar
  :std/error :std/text/hex :std/test :std/srfi/1
  :glow/crypto/keccak
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
                 [(@bytes "hello, world!") "0x68656c6c6f2c20776f726c6421"]
                 [(keccak256<-bytes #u8()) "0xc5d2460186f7233c927e7db2dcc703c0e500b653ca82273b7bfad8045d85a470"]])
      (for-each (match <>
                  ([hex err]
                   (check-equal? err (with-catch error-exception-message (cut bytes<-0x hex)))))
                [["0" "Hex string does not begin with 0x"]
                 ["" "Hex string does not begin with 0x"]
                 ["004200" "Hex string does not begin with 0x"]
                 ["0x0" "Odd number of digits in hex string"]
                 ["0xf0f0f" "Odd number of digits in hex string"]]))

    (test-case "0x <-> address"
      (for-each (lambda (hex)
                  (check-equal? (0x<-address (bytes<-0x hex)) hex)
                  (check-equal? (address<-0x hex) (bytes<-0x hex)))
                [;; These 4 test vectors are from EIP-55 itself:
                 "0x5aAeb6053F3E94C9b9A09f33669435E7Ef1BeAed"
                 "0xfB6916095ca1df60bB79Ce92cE3Ea74c37c5d359"
                 "0xdbF03B407c01E7cD3CBea99509d93f8DDDC8C6FB"
                 "0xD1220A0cf47c7B9Be7A2E6BA89F429762e7b9aDb"
                 ;; Random addresses
                 "0x9797809415E4B8efEa0963E362ff68B9d98F9e00"
                 "0x507877C2E26f1387432D067D2DaAfa7d0420d90a"
                 ])
      (for-each (match <>
                  ([hex err]
                   (check-equal? (with-catch error-message (cut address<-0x hex)) err)))
                [["0x9797809415e4b8efea0963e362ff68b9d98f9e00"
                  "Invalid address checksum \"0x9797809415e4b8efea0963e362ff68b9d98f9e00\" 12\n"]
                 ["0x507877C2E26f1387432D067D2DaAfa7D0420d90a"
                  "Invalid address checksum \"0x507877C2E26f1387432D067D2DaAfa7D0420d90a\" 33\n"]
                 ["0x507877" "invalid address string length \"0x507877\"\n"]]))
    ))

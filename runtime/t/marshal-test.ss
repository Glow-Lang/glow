(export marshal-test)

(import
  :gerbil/gambit/bytes :gerbil/gambit/exceptions
  :std/error :std/text/hex :std/test :std/srfi/1 :std/sugar
  :glow/runtime/marshal)

(def marshal-test
  (test-suite "test suite for glow/runtime/marshal"
    (test-case "read-uint16, write-uint16"
      (for-each (match <>
                  ([int hex]
                   (check-equal? int ((<-bytes<-unmarshal read-uint16) (hex-decode hex)))
                   (check-equal? hex (hex-encode ((bytes<-<-marshal write-uint16) int)))))
                [[0 "0000"]
                 [10 "000a"]
                 [65535 "ffff"]
                 [256 "0100"]
                 [258 "0102"]]))))

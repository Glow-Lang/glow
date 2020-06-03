(export json-rpc-test)

(import
  :gerbil/gambit/exceptions
  :std/sugar
  :std/test
  :glow/ethereum/json-rpc)

(def json-rpc-test
  (test-suite "Test suite for glow/ethereum/json-rpc"
#|
    (test-case "parse-signed-signature"
      (def stj "{\"raw\":\"0xf8c90302830f4240940000000000000000000000000000000000000000820404b864cf2c52cb000000000000000000000000f47408143d327e4bc6a87ef4a70a4e0af09b9a1c00000000000000000000000000000000000000000000000000000000000000400000000000000000000000000000000000000000000000000000000000000000820a96a0f6683d2489560376326818813d4d2aac304feba152111c75d1a192c5b2660493a052660483b5855f5f2ca61c24682869702d3ed0c5b838eb1b7ed36c804221ed43\",\"tx\":{\"nonce\":\"0x3\",\"gasPrice\":\"0x2\",\"gas\":\"0xf4240\",\"to\":\"0x0000000000000000000000000000000000000000\",\"value\":\"0x404\",\"input\":\"0xcf2c52cb000000000000000000000000f47408143d327e4bc6a87ef4a70a4e0af09b9a1c00000000000000000000000000000000000000000000000000000000000000400000000000000000000000000000000000000000000000000000000000000000\",\"v\":\"0xa96\",\"r\":\"0xf6683d2489560376326818813d4d2aac304feba152111c75d1a192c5b2660493\",\"s\":\"0x52660483b5855f5f2ca61c24682869702d3ed0c5b838eb1b7ed36c804221ed43\",\"hash\":\"0xc34293fefd30282a189cce127a3636e2076b0fdf843bcf10361b0784061db2cf\"}}")
      (def st {<-json-string SignedTransaction::t st})
      (check-equal? (bytes-ref (SignedTransaction-raw st) 0) #xf8))

    (test-case "txpool-content round-trip decode/encode/decode succeeds"
      (def a {<-json-string TxPoolContent example-valid-response})
      (def b {<-json-string TxPoolContent {json-string<- a}})
      (check-equal? a b))

    (test-case "txpool-content decode errors"
      ;; null `block-hash` field yields failed decode"
      (check-exception {<-json-string TxPoolContent example-invalid-response-null-block-hash} true)
      ;; missing `pending` + `queued` fields yields failed decode
      (check-exception {<-json-string TxPoolContent "{\"foo\":[1,2,3],\"bar\":\"baz\"}"} true)
      ;; malformed nonces yield failed decode
      (check-exception {<-json-string TxPoolContent example-invalid-response-malformed-nonces} true)
      ;; malformed nonces list yields failed decode
      (check-exception {<-json-string TxPoolContent example-invalid-response-malformed-nonces-list} true))
|#
    (void)
    ))

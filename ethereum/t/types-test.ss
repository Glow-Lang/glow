(export types-test)

(import
  :gerbil/gambit/bytes
  :gerbil/gambit/exceptions
  :std/error :std/text/hex :std/sort :std/srfi/1 :std/sugar :std/test
  :clan/utils/base :clan/poo/poo :clan/poo/io (only-in :clan/poo/mop define-type)
  :glow/ethereum/types :glow/ethereum/hex)

(define-type EthereumRpcConfig
  (Record
   scheme: [Symbol]
   host: [String]
   port: [UInt16]))

(def (sort-alist alist) (sort alist (comparing-key test: string<? key: car)))

(defrule (check-rep parse unparse rep obj)
  (begin ;;let ((rep rep) (obj obj))
    (check-equal? (parse rep) obj)
    (check-equal? (unparse obj) rep)))

(def types-test
  (test-suite "test suite for glow/ethereum/types"
    (test-case "Record"
      (check-rep (compose .sorted-alist (.@ EthereumRpcConfig methods .<-json) list->hash-table)
                 (compose sort-alist hash->list (.@ EthereumRpcConfig methods .json<-) .<-alist)
                 '(("host" . "localhost") ("port" . "0x50") ("scheme" . "http"))
                 '((host . "localhost") (port . 80) (scheme . http)))
      (check-rep (compose .sorted-alist (.@ EthereumRpcConfig methods .<-bytes) bytes<-0x)
                 (compose 0x<-bytes (.@ EthereumRpcConfig methods .bytes<-) .<-alist)
                 "0x00046874747000096c6f63616c686f73740050"
                 '((host . "localhost") (port . 80) (scheme . http)))
      )))

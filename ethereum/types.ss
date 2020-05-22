(export #t)

(import
  :gerbil/gambit/bits :gerbil/gambit/bytes
  :std/misc/bytes :std/sugar
  :clan/utils/base :clan/utils/list
  :clan/poo/poo :clan/poo/mop :clan/poo/brace
  (prefix-in :clan/poo/type poo.)
  :glow/runtime/marshal :glow/ethereum/hex)

;; --- something for types in general, including Record, Union, Maybe
;; --- something for ethereum types in particular

;; For now, types are just runtime descriptors...
(defrule (define-type a ...) (def a ...))

;; Integer and Bytes types
(.def (UInt. @ poo.UInt. bits)
  .repr: `(UInt ,bits)
  methods: =>.+ {(:: @ [] length-in-bytes)
    .json<-: 0x<-nat
    .<-json: (λ (x) (validate @ (nat<-0x x)))
    .bytes<-: (λ (n) (def bytes (make-bytes length-in-bytes))
                 (u8vector-uint-set! bytes 0 n big length-in-bytes)
                 bytes)
    .<-bytes: (λ (bytes) (u8vector-uint-ref bytes 0 big length-in-bytes))
    .marshal: (λ (n port) (write-u8vector (.bytes<- n) port))
    .unmarshal: (λ (port) (.<-bytes (read-n-bytes length-in-bytes port)))
    })

(.def (Bytes. @ [] n)
   .repr: `(Bytes ,n)
   methods: =>.+ {(:: @ [])
    length-in-bytes: n
    .element?: (λ (x) (and (bytes? x) (= (bytes-length x) n)))
    .json<-: 0x<-bytes
    .<-json: (λ (x) (validate @ (bytes<-0x x)))
    .bytes<-: identity
    .<-bytes: (cut validate @ <>)
    .marshal: write-u8vector
    .unmarshal: (cut read-n-bytes n <>)
  })

;; TODO: define all the standard sizes of UInt and Bytes in a loop

;; Records
(def (Record . plist)
  (def a (alist<-plist plist))
  {(:: @ [Class])
   repr: 'Record
   slots: {}
   }
  (error "NIY"))

(.defgeneric (printable-slots x)
   from: type
   default: .all-slots
   slot: .printable-slots)

(.defgeneric (json<- type x)
   from: methods
   default: (lambda (_ x) {:json x})
   ;;combination: and-combination
   slot: .json<-)

(def (@@method :json poo) json<-)

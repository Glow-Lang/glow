(export #t)

(import
  :gerbil/gambit/bits :gerbil/gambit/bytes
  (for-syntax :std/iter)
  :std/misc/bytes :std/srfi/1 :std/sugar
  :clan/utils/base :clan/utils/list
  :clan/poo/poo :clan/poo/mop :clan/poo/brace
  (prefix-in :clan/poo/type poo.)
  (for-syntax :glow/compiler/common)
  :glow/runtime/marshal :glow/ethereum/hex)

;; --- something for types in general, including Record, Union, Maybe
;; --- something for ethereum types in particular

;; For now, types are just runtime descriptors...
(defrule (define-type a ...) (def a ...))

;; Integer and Bytes types
(.def (UInt. @ poo.UInt. bits)
  .repr: `(UInt ,bits)
  methods: =>.+ {(:: methods [] length-in-bytes)
    .json<-: 0x<-nat
    .<-json: (λ (x) (validate @ (nat<-0x x)))
    .bytes<-: (λ (n) (def bytes (make-bytes length-in-bytes))
                 (u8vector-uint-set! bytes 0 n big length-in-bytes)
                 bytes)
    .<-bytes: (λ (bytes) (u8vector-uint-ref bytes 0 big length-in-bytes))
    .marshal: (λ (n port) (write-u8vector (.bytes<- n) port))
    .unmarshal: (λ (port) (.<-bytes (read-bytes length-in-bytes port)))
    })

(defsyntax (defUIntNs stx)
  (cons #'begin (for/collect (i (in-range 8 257 8))
                  (with-syntax ((name (datum->syntax (stx-car stx) (format-symbol "UInt~d" i)))
                                (bits i))
                    #'(.def (name @ UInt.) (bits))))))
(defUIntNs)

(.def (Bytes. @ [] n)
   .repr: `(Bytes ,n)
   methods: =>.+ {
    length-in-bytes: n
    .element?: (λ (x) (and (bytes? x) (= (bytes-length x) n)))
    .json<-: 0x<-bytes
    .<-json: (λ (x) (validate @ (bytes<-0x x)))
    .bytes<-: identity
    .<-bytes: (cut validate @ <>)
    .marshal: write-u8vector
    .unmarshal: (cut read-bytes n <>)
  })
(defsyntax (defByteNs stx)
  (cons #'begin (for/collect (n (in-range 1 33))
                  (with-syntax ((name (datum->syntax (stx-car stx) (format-symbol "Byte~d" n)))
                                (n n))
                    #'(.def (name @ Bytes.) (n))))))
(defByteNs)


;; TODO: define all the standard sizes of UInt and Bytes in a loop

(def (sym<-kw k) (string->symbol (keyword->string k)))

;; Records
;; TODO: support defaults
(def (Record . plist)
  (def a (map/car sym<-kw (alist<-plist plist)))
  {(:: @ [Class])
   repr: ['Record . plist]
   slots: (.<-alist
           (map (match <> ([sym . type] (cons sym {type: Type}))) a))
   methods: =>.+ {(:: @ [bytes<-un/marshal])
     .json<-: (lambda (v)
                (list->hash-table
                 (map (match <> ([sym . type] (cons sym (.call type .json<- (.ref v sym))))) a)))
     .<-json: (lambda (j)
                (.mix
                    (.<-alist (map (match <> ([sym . type] (cons sym (.call type .<-json (hash-ref j sym))))) a))
                    proto))
     .marshal: (lambda (v port)
                 (for-each (match <> ([sym . type] (marshal type (.ref v sym) port))) a))
     .unmarshal: (lambda (v port)
                   (.mix
                    (.<-alist (map-in-order
                               (match <> ([sym . type] (cons sym (unmarshal type port)))) a))
                    proto))
     }
   })

#|
;;(def (Union ...)

(def (TaggedUnion . plist)
  (def a (map/car sym<-kw (alist<-plist plist)))
  (def tag-bits (integer-length (1- (length a))))
  {(:: @ [TagDependentPair])
   repr: ['TaggedUnion . plist]
   tag-bits: tag-bits
   types: (map type<-taggeduniondescriptor alist)

   methods: =>.+ {(:: @ [bytes<-un/marshal])
     .json<-: (lambda (v)
                (list->hash-table
                 (map (match <> ([sym . type] (cons sym (.call type .json<- (.ref v sym))))) a)))
     .<-json: (lambda (j)
                (.mix
                    (.<-alist (map (match <> ([sym . type] (cons sym (.call type .<-json (hash-ref j sym))))) a))
                    proto))
     .marshal: (lambda (v port)
                 (for-each (match <> ([sym . type] (marshal type (.ref v sym) port))) a))
     .unmarshal: (lambda (v port)
                   (.mix
                    (.<-alist (map-in-order
                               (match <> ([sym . type] (cons sym (unmarshal type port)))) a))
                    proto))
     }
   })
|#

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

(export #t)

(import
  (for-syntax :gerbil/gambit/exact :std/iter :glow/compiler/common)
  :gerbil/gambit/bits :gerbil/gambit/bytes :gerbil/gambit/exact :gerbil/gambit/hash :gerbil/gambit/ports
  :std/iter :std/misc/bytes :std/misc/hash :std/sort :std/srfi/1 :std/sugar
  (only-in :std/srfi/43 vector-index)
  :clan/utils/base :clan/utils/io :clan/utils/json :clan/utils/list :clan/utils/maybe :clan/utils/number
  :clan/poo/poo :clan/poo/io
  (only-in :clan/poo/mop .defgeneric Type Type. proto Class Class. Slot validate element?)
  (prefix-in (only-in :clan/poo/mop Integer String Symbol) poo.)
  (prefix-in :clan/poo/type poo.)
  :clan/poo/brace
  :glow/ethereum/hex)

;; --- something for types in general, including Record, Union, Maybe
;; --- something for ethereum types in particular

;; For now, types are just runtime descriptors...
(defrule (define-type a desc) (def a (.mix {repr: 'a} desc)))

;; Variable-length Int
(.def (Integer @ poo.Integer n-bits)
  methods: =>.+ {(:: methods [bytes<-un/marshal])
    .json<-: 0x<-nat
    .<-json: (λ (x) (validate @ (nat<-0x x)))
    })

(.def (IntegerSet @ Type.)
   .repr: 'IntegerSet
   .element?: (lambda (x) (and (table? x)
                          (every exact-integer? (hash-keys x))
                          (every (cut eq? #t <>) (hash-values x))))
   methods: =>.+ {(:: methods [bytes<-un/marshal])
     .json<-: (lambda (x) '(list-sort < (hash-keys x))) ;; XXXXX
     .<-json: (lambda (x) (list->hash-table (map (cut cons <> #t) x)))
     .marshal: (lambda (x port)
                 (def l (.json<- x))
                 (marshal Integer (length l) port)
                 (for-each (cut marshal Integer <> port) l))
     .unmarshal: (lambda (port)
                   (.<-json (for/collect (_ (in-range (unmarshal Integer port)))
                              (unmarshal Integer port))))
     .<-json: (lambda (x) (list->hash-table (map (cut cons <> #t) x)))
   })


;; Integer and Bytes types
(.def (UInt. @ poo.UInt. n-bits)
  repr: `(UInt ,n-bits)
  methods: =>.+ {(:: methods [un/marshal<-bytes] length-in-bytes)
    .json<-: 0x<-nat
    .<-json: (λ (x) (validate @ (nat<-0x x)))
    .bytes<-: (cut bytes<-nat <> length-in-bytes)
    .<-bytes: nat<-bytes
    })

(defsyntax (defUIntNs stx)
  (cons #'begin (for/collect (i (in-range 8 257 8))
                  (with-syntax ((name (datum->syntax (stx-car stx) (format-symbol "UInt~d" i)))
                                (i i))
                    #'(.def (name @ UInt.) n-bits: i repr: 'name)))))
(defUIntNs)

(.def (Bytes. @ Type. n)
   repr: `(Bytes ,n)
   .element?: (λ (x) (and (bytes? x) (= (bytes-length x) n)))
   methods: =>.+ {
    length-in-bytes: n
    zero: (make-bytes n)
    .json<-: 0x<-bytes
    .<-json: (λ (x) (validate @ (bytes<-0x x)))
    .bytes<-: identity
    .<-bytes: (cut validate @ <>)
    .marshal: write-u8vector
    .unmarshal: (cut read-bytes n <>)
  })
(defsyntax (defBytesN stx)
  (syntax-case stx ()
    ((_ i ctx) (exact-integer? (stx-e #'i))
     (with-syntax ((name (datum->syntax #'ctx (format-symbol "Bytes~d" (stx-e #'i)))))
       #'(.def (name @ Bytes.) n: i repr: 'name)))
    ((d i) #'(d i d))))
(defsyntax (defBytesNs stx)
  (syntax-case stx ()
    ((d) (cons #'begin (for/collect (i (in-range 1 33)) (with-syntax ((i i)) #'(defBytesN i d)))))))
(defBytesNs)
(defBytesN 65) ; Signature, PublicKey using Secp256k1

(.def (JsInt @ poo.Integer)
   repr: 'JsInt
   .element?: (λ (x) (and (exact-integer? x) (<= .most-negative x .most-positive)))
   .most-positive: (1- (expt 2 53))
   .most-negative: (- .most-positive)
   methods: =>.+ {(:: @@ [un/marshal<-bytes])
    length-in-bytes: 7
    .json<-: identity
    .<-json: (λ (x) (validate @ x))
    .bytes<-: (λ (n) (def bytes (make-bytes 7))
                 (u8vector-sint-set! bytes 0 n big 7)
                 bytes)
    .<-bytes: (λ (bytes) (validate @ (u8vector-sint-ref bytes 0 big 7)))
   })

(.def (BytesL16 @ Type.)
   repr: 'BytesL16
   .element?: (λ (x) (and (bytes? x) (<= (bytes-length x) 65535)))
   methods: =>.+ {(:: @@ [bytes<-un/marshal])
    .json<-: 0x<-bytes
    .<-json: (λ (x) (validate @ (bytes<-0x x)))
    .marshal: write-sized16-bytes
    .unmarshal: read-sized16-bytes
  })

(define-type Bytes BytesL16)

(.def (String @ poo.String)
   methods: =>.+ {(:: @@ [bytes<-un/marshal])
    .json<-: identity
    .<-json: (λ (x) (validate @ x))
    .marshal: (lambda (s port) (write-sized16-bytes (string->bytes s) port))
    .unmarshal: (lambda (port) (bytes->string (read-sized16-bytes port)))
  })

(.def (Symbol @ poo.Symbol)
   methods: =>.+ {(:: @@ [bytes<-un/marshal])
    .json<-: symbol->string
    .<-json: string->symbol
    .marshal: (lambda (s port) (write-sized16-bytes (string->bytes (symbol->string s)) port))
    .unmarshal: (lambda (port) (string->symbol (bytes->string (read-sized16-bytes port))))
  })


(def (bytes<-double d)
  (def bytes (make-bytes 8))
  (u8vector-double-set! bytes 0 d big)
  bytes)

(def (double<-bytes bytes)
  (u8vector-double-ref bytes 0 big))

(.def (Real @ Type.)
   repr: 'Real
   .element?: real?
   methods: =>.+ {(:: @@ [un/marshal<-bytes])
    length-in-bytes: 8
    .json<-: identity
    .<-json: (cut validate @ <>)
    .bytes<-: bytes<-double
    .<-bytes: double<-bytes
  })

(.def (Bool @ Type.)
   repr: 'Bool
   .element?: boolean?
   methods: =>.+ {(:: @@ [un/marshal<-bytes])
    length-in-bytes: 1
    .json<-: identity
    .<-json: (cut validate @ <>)
    .bytes<-: (lambda (x) (if x #u8(1) #u8(0)))
    .<-bytes: (λ (b) (< 0 (u8vector-ref b 0)))
  })

(.def (Json @ Type.)
   repr: 'Json
   .element?: true
   methods: =>.+ {
    .json<-: identity
    .<-json: identity
  })

(def (sym<-kw k) (string->symbol (keyword->string k)))

;; Records
;; TODO: support defaults
(def (RecordSlot type . options)
  (def o (.<-alist (alist<-plist options)))
  {(:: @ [o]) (type) optional: (.has? o default)})

(def (Record . plist)
  (def a (map (match <> ([kw type . options] (cons (sym<-kw kw) (apply RecordSlot type options))))
              (alist<-plist plist)))
  {(:: @ [Class.] proto)
   repr: ['Record . plist]
   slots: (.<-alist a)
   methods: =>.+ {(:: @ [bytes<-un/marshal])
     .json<-: (lambda (v)
                (list->hash-table
                 (map (match <> ([sym . slotdef] (cons (symbol->string sym) ((.@ slotdef type methods .json<-) (.ref v sym))))) a)))
     .<-json: (lambda (j)
                (.mix
                    (.<-alist (map (match <> ([sym . slotdef] (cons sym ((.@ slotdef type methods .<-json) (hash-ref j (symbol->string sym)))))) a))
                    proto))
     .marshal: (lambda (v port)
                 (for-each (match <> ([sym . slotdef]
                                      ((.@ slotdef type methods .marshal) (.ref v sym) port)))
                           a))
     .unmarshal: (lambda (port)
                   (.mix
                    (.<-alist (map-in-order
                               (match <> ([sym . slotdef]
                                          (cons sym ((.@ slotdef type methods .unmarshal) port))))
                               a))
                    proto))
     }
   })

(.def (Tuple. @ Type. types)
  methods: =>.+ {(:: @ [bytes<-un/marshal])
    .json<-: (lambda (v) (vector-map json<- types v))
    .<-json: (lambda (j) (vector-map <-json types (if (list? j) (list->vector j) j)))
    .marshal: (lambda (v port)
                (vector-for-each (lambda (type val) (marshal type val port))
                                 types v))
    .unmarshal: (lambda (port)
                  (vector-for-each (lambda (type) (unmarshal type port)) types))
  })

(def (Tuple . types_) ;; type of tuples, heterogeneous arrays of given length and type
  (def types (list->vector (map (cut validate Type <>) types_)))
  {(:: @ Tuple.) (types)})

;; Untagged union. Can be used for JSON, but no automatic marshaling.
(.def (Union. @ Type. types)
  repr: `(Union ,@(map (cut .@ <> repr) types))
  .element?: (λ (x) (any (cut element? <> x) types))
  methods: {(:: @ [bytes<-un/marshal])
    .json<-: (lambda (v) (let/cc return
                      (for-each (λ (type)
                                  (when (element? type v) (return (json<- type v))))
                                types)
                      (error "invalid element of type" v repr)))
    .<-json: (lambda (j) (let/cc return
                      (for-each (λ (type)
                                  (with-catch void (lambda () (return (<-json type j)))))
                                types)
                      (error "invalid json for type" j repr)))})

(def (Union . types) ;; type of tuples, heterogeneous arrays of given length and type
  {(:: @ Union.) (types)})

(.def (Enum. @ Type. vals)
  repr: `(Enum. ,@vals)
  .element?: (cut member <> vals)
  .vals@: (list->vector vals)
  .json@: (list->vector (map (lambda (x) (json<-string (string<-json x))) vals))
  methods: {(:: @ [un/marshal<-bytes])
    length-in-bytes: (n-bytes<-n-bits (integer-length (vector-length .vals@)))
    .<-nat: (cut vector-ref .vals@ <>)
    .nat<-: (lambda (v) (vector-index (cut equal? <> v) .vals@))
    .json<-: (lambda (v) (vector-ref .json@ (.nat<- v)))
    .<-json: (lambda (j) (vector-index (cut equal? <> j) .json@))
    .bytes<-: (compose (cut bytes<-nat <> length-in-bytes) .nat<-)
    .<-bytes: (compose .<-nat nat<-bytes)
})

(defrule (Enum values ...) {(:: @ Enum.) vals: '(values ...)})

(define-type Unit
  {(:: @ (Enum null))
   methods: =>.+ {
     .json<-: void
     .<-json: void
     .bytes<-: (lambda _ #u8())
     .<-bytes: void
     .marshal: void
     .unmarshal: void
}})

(.def (Maybe. @ Type. type)
  repr: `(Maybe ,type)
  .element?: (lambda (x) (or (eq? x null) (element? type x)))
  methods: {(:: @@ [bytes<-un/marshal])
    .json<-: (lambda (v) (if (eq? v null) v ((.@ type methods .json<-) v)))
    .<-json: (lambda (j) (if (eq? j null) j ((.@ type methods .<-json) j)))
    .marshal: (λ (x port) (cond ((eq? x null) (write-byte 0 port))
                                (else (write-byte 1 port) (marshal type x port))))
    .unmarshal: (λ (port) (if (zero? (read-byte port)) null (unmarshal type port)))
})

(def (Maybe type) {(:: @ Maybe.) (type)})

(.def (List. @ Type. type)
  repr: `(List ,type)
  .element?: (lambda (x) (let loop ((x x))
                      (or (null? x)
                          (and (pair? x) (element? type (car x)) (loop (cdr x))))))
  methods: {(:: methods [bytes<-un/marshal])
    .json<-: (lambda (v) (map (.@ type methods .json<-) v))
    .<-json: (lambda (j) (map (.@ type methods .<-json) j))
    .marshal: (let (m (.@ type methods .marshal))
                (lambda (v port) (write-uint16 (length v) port) (for-each (cut m <> port) v)))
    .unmarshal: (let (u (.@ type methods .unmarshal))
                  (lambda (port) (def l (read-uint16 port))
                     (for/collect (_ (in-range l)) (u port))))})

(def (List type) {(:: @ List.) (type)})

(.def (StringMap. @ Type. value-type)
  repr: `(StringMap ,value-type)
  key-type: String
  .element?: (lambda (x) (and (hash-table? x)
                         (let/cc return
                           (hash-for-each (lambda (k v) (unless (and (element? key-type k)
                                                                (element? value-type v))
                                                     (return #f)))
                                          x)
                           #t)))
  methods: =>.+ {
    .json<-: (lambda (m) (hash-value-map (.@ value-type methods .json<-) m))
    .<-json: (lambda (j) (hash-value-map (.@ value-type methods .<-json) j))
  })

(def (StringMap value-type) {(:: @ StringMap.) (value-type)})

(.defgeneric (printable-slots x)
   from: type
   default: .all-slots
   slot: .printable-slots)

(.defgeneric (<-json type j)
   from: methods
   slot: .<-json)

(.defgeneric (json<- type x)
   from: methods
   slot: .json<-)

(def (@@method :json poo) (json<- (.@ poo .type) poo))

(def (json-string<- type x)
  (string<-json (json<- type x)))

;; TODO:
#;(def (TaggedUnion . plist)
  (def a (map/car sym<-kw (alist<-plist plist)))
  (def tag-bits (integer-length (1- (length a))))
  ...)

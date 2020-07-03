(export #t)

(import
  (for-syntax :gerbil/gambit/exact :std/iter :glow/compiler/common)
  :gerbil/gambit/bits :gerbil/gambit/bytes :gerbil/gambit/exact
  :gerbil/gambit/hash :gerbil/gambit/ports
  :std/format :std/iter :std/misc/bytes :std/misc/completion
  :std/sort :std/srfi/1 :std/srfi/43 :std/sugar :std/text/json
  :clan/utils/base :clan/utils/io :clan/utils/json :clan/utils/list
  :clan/utils/maybe :clan/utils/number
  :clan/poo/poo :clan/poo/io
  (only-in :clan/poo/mop
           Type Type. proto Class Class. Slot
           .defgeneric validate element? .method define-type)
  (prefix-in (only-in :clan/poo/mop Bool String Symbol) poo.)
  (prefix-in (only-in :clan/poo/number Nat UInt. IntSet) poo.)
  (prefix-in :clan/poo/type poo.)
  :clan/poo/brace
  ./hex ./abi)

;; --- something for types in general, including Record, Union, Maybe
;; --- something for ethereum types in particular

;; Variable-length Nat
(.def (Nat @ poo.Nat n-bits)
  methods: =>.+ {(:: methods [bytes<-un/marshal])
    .sexp<-: (lambda (x) `(nat<-0x ,(0x<-nat x)))
    .json<-: 0x<-nat
    .<-json: (λ (x) (validate @ (nat<-0x x)))
    })

(.def (NatSet @ poo.IntSet)
  sexp: 'NatSet
  Int: Nat)

(def (ensure-zeroes bytes start len)
  (for (i (in-range len))
    (assert! (zero? (bytes-ref bytes (+ start i))))))

;; Integer types
(.def (UInt. @ poo.UInt. n-bits)
  sexp: `(UInt ,n-bits)
  ethabi-display-type: (lambda (port) (fprintf port "uint~d" n-bits))
  ethabi-static?: #t
  ethabi-head-length: 32
  methods: =>.+ {(:: methods [un/marshal<-bytes] length-in-bytes)
    .sexp<-: identity
    .json<-: 0x<-nat
    .<-json: (λ (x) (validate @ (nat<-0x x)))
    .bytes<-: (cut bytes<-nat <> length-in-bytes)
    .<-bytes: nat<-bytes
    .ethabi-padding: (- 32 length-in-bytes)
    .ethabi-tail-length: (lambda (_) 0)
    .ethabi-encode-into:
    (lambda (x bytes start head get-tail set-tail!)
      (u8vector-uint-set! bytes (+ head .ethabi-padding) x big length-in-bytes))
    .ethabi-decode-from:
    (lambda (bytes start head get-tail set-tail!)
      (ensure-zeroes bytes head .ethabi-padding)
      (u8vector-uint-ref bytes (+ head .ethabi-padding) big length-in-bytes))
    })
(defsyntax (defUIntNs stx)
  (cons #'begin (for/collect (i (in-range 8 257 8))
                  (with-syntax ((name (datum->syntax (stx-car stx) (format-symbol "UInt~d" i)))
                                (i i))
                    #'(.def (name @ UInt.) n-bits: i sexp: 'name)))))
(defUIntNs)

;; Bytes types
(.def (Bytes. @ Type. n)
  sexp: `(Bytes ,n)
  ethabi-display-type: (lambda (port) (fprintf port "bytes~d" n))
  ethabi-static?: #t
  ethabi-head-length: 32
  .element?: (λ (x) (and (bytes? x) (= (bytes-length x) n)))
  methods: =>.+ {
    .sexp<-: (lambda (x) ['bytes<-0x (0x<-bytes x)])
    length-in-bytes: n
    zero: (make-bytes n)
    .json<-: 0x<-bytes
    .<-json: (λ (x) (validate @ (bytes<-0x x)))
    .bytes<-: identity
    .<-bytes: (cut validate @ <>)
    .marshal: write-u8vector
    .unmarshal: (cut read-bytes n <>)
    .ethabi-padding: (- 32 n)
    .ethabi-tail-length: (lambda (_) 0)
    .ethabi-encode-into:
    (lambda (x bytes start head get-tail set-tail!)
      (subu8vector-move! x 0 n bytes head))
    .ethabi-decode-from:
    (lambda (bytes start head get-tail set-tail!)
      (def end (+ head n))
      (ensure-zeroes bytes end .ethabi-padding)
      (subu8vector bytes head end))
  })
(defsyntax (defBytesN stx)
  (syntax-case stx ()
    ((_ i ctx) (exact-integer? (stx-e #'i))
     (with-syntax ((name (datum->syntax #'ctx (format-symbol "Bytes~d" (stx-e #'i)))))
       #'(.def (name @ Bytes.) n: i sexp: 'name)))
    ((d i) #'(d i d))))
(defsyntax (defBytesNs stx)
  (syntax-case stx ()
    ((d) (cons #'begin (for/collect (i (in-range 1 33)) (with-syntax ((i i)) #'(defBytesN i d)))))))
(defBytesNs)
(defBytesN 65) ; Signature, PublicKey using Secp256k1
(defBytesN 256) ; Bloom filter

(.def (BytesL16 @ Type.)
   sexp: 'BytesL16
   ethabi-display-type: (cut display "bytes" <>)
   ethabi-static?: #f
   ethabi-head-length: 32
   .element?: (λ (x) (and (bytes? x) (<= (bytes-length x) 65535)))
   methods: =>.+ {(:: @@ [bytes<-un/marshal])
    .sexp<-: (.@ Bytes. methods .sexp<-)
    .json<-: 0x<-bytes
    .<-json: (λ (x) (validate @ (bytes<-0x x)))
    .marshal: write-sized16-bytes
    .unmarshal: read-sized16-bytes
    .ethabi-tail-length: (lambda (x) (+ 32 (ceiling-align (bytes-length x) 32)))
    .ethabi-encode-into:
    (lambda (x bytes start head get-tail set-tail!)
      (def tail (get-tail))
      (u8vector-uint-set! bytes head (- tail start) big 32)
      (u8vector-uint-set! bytes tail (bytes-length x) big 32)
      (subu8vector-move! x 0 (bytes-length x) bytes (+ tail 32))
      (set-tail! (+ tail 32 (ceiling-align (bytes-length x) 32))))
    .ethabi-decode-from:
    (lambda (bytes start head get-tail set-tail!)
      (def tail (+ start (u8vector-uint-ref bytes head big 32)))
      (assert! (= tail (get-tail)))
      (def data-start (+ tail 32))
      (set-tail! data-start)
      (def len (u8vector-uint-ref bytes tail big 32))
      (set-tail! (+ data-start (ceiling-align len 32)))
      (subu8vector bytes data-start (+ data-start len)))
  })
(define-type Bytes BytesL16)

(.def (un/marshal<-dynamic-bytes @ [] .<-bytes .bytes<-)
  .marshal: (lambda (x port) (marshal Bytes (.bytes<- x) port))
  .unmarshal: (lambda (port) (.<-bytes (unmarshal Bytes port))))

(.def (Json @ Type.)
   sexp: 'Json
   .element?: true
   methods: =>.+ {(:: @@ un/marshal<-dynamic-bytes)
    .json<-: identity
    .<-json: identity
    .bytes<-: (lambda (x) (call-with-output-u8vector [] (cut write-json x <>)))
    .<-bytes: (lambda (bytes) (call-with-input-u8vector bytes json<-port))
  })

(.def (un/marshal<-json @ un/marshal<-dynamic-bytes .json<- .<-json)
   .bytes<-: (lambda (x) (bytes<- Json (.json<- x)))
   .<-bytes: (lambda (x) (.<-json (<-bytes Json x))))

(.def (String @ poo.String)
   ethabi-display-type: (cut display "string" <>)
   ethabi-static?: #f
   ethabi-head-length: 32
   .element?: (λ (x) (and (string? x)
                          (or (< (string-length x) 16384)
                              (and (not (< 65535 (string-length x)))
                                   (<= (bytes-length (string->bytes x)) 65535)))))
   methods: =>.+ {(:: @@ un/marshal<-dynamic-bytes)
    .json<-: identity
    .<-json: (λ (x) (validate @ x))
    .<-bytes: bytes->string
    .bytes<-: string->bytes
    .ethabi-tail-length: (rcompose string->bytes (.@ Bytes methods .ethabi-tail-length))
    .ethabi-encode-into:
    (lambda (x bytes start head get-tail set-tail!)
      (.method Bytes .ethabi-encode-into (string->bytes x) bytes start head get-tail set-tail!))
    .ethabi-decode-from:
    (lambda (bytes start head get-tail set-tail!)
      (bytes->string (.method Bytes .ethabi-decode-from bytes start head get-tail set-tail!)))
  })

;; TODO: have a function that only interns the string to a symbol if already found?
(.def (Symbol @ poo.Symbol)
   ethabi-display-type: (cut display "bytes" <>)
   ethabi-static?: #f
   ethabi-head-length: 32
   methods: =>.+ {(:: @@ un/marshal<-dynamic-bytes)
    .json<-: symbol->string
    .<-json: maybe-intern-symbol
    .bytes<-: (rcompose symbol->string string->bytes)
    .<-bytes: (rcompose bytes->string string->symbol)
    .ethabi-tail-length: (rcompose symbol->string (.@ String methods .ethabi-tail-length))
    .ethabi-encode-into:
    (lambda (x bytes start head get-tail set-tail!)
      (.method String .ethabi-encode-into (symbol->string x) bytes start head get-tail set-tail!))
    .ethabi-decode-from:
    (lambda (bytes start head get-tail set-tail!)
      (maybe-intern-symbol (.method String .ethabi-decode-from bytes start head get-tail set-tail!)))
    })

(.def (Bool @ poo.Bool)
   ethabi-display-type: (cut display "bool" <>)
   ethabi-static?: #t
   ethabi-head-length: 32
   methods: =>.+ {
    .ethabi-padding: 31
    .ethabi-tail-length: (lambda (_) 0)
    .ethabi-encode-into:
    (lambda (x bytes start head get-tail set-tail!)
      (u8vector-set! bytes (+ head 31) (if x 1 0)))
    .ethabi-decode-from:
    (lambda (bytes start head get-tail set-tail!)
      (ensure-zeroes bytes head 31)
      (case (u8vector-ref bytes (+ head 31))
        ((1) #t) ((0) #f) (else (error "Invalid bool"))))
  })

;; Records
;; TODO: support defaults
(def (RecordSlot type . options)
  (def o (.<-alist (alist<-plist options)))
  {(:: @ [o]) (type) optional: (.has? o default)})
(def (Record . plist)
  (def a (map (match <> ([kw type . options] (cons (sym<-kw kw) (apply RecordSlot type options))))
              (alist<-plist plist)))
  {(:: @ [Class.] proto)
   sexp: ['Record . plist]
   slots: (.<-alist a)
   types: (map cadr plist)
   ethabi-display-type: (cut ethabi-display-types types <>)
   ethabi-static?: (every (cut .@ <> ethabi-static?) types)
   ethabi-head-length: (ethabi-head-length types)
   methods: =>.+ {(:: @ [bytes<-un/marshal])
     .sexp<-: (lambda (v) `(.cc (proto ,sexp) ,@(append-map (match <> ([sym . slotdef] [(kw<-sym sym) (sexp<- (.@ slotdef type) (.ref v sym))])) a)))
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
     .tuple-list<-: (lambda (x) (map (lambda (s) (.ref x (car s))) a))
     .<-tuple-list: (lambda (x) (.<-alist (map (lambda (s v) (cons (car s) v)) a x)))
     .tuple<-: (compose list->vector .tuple-list<-)
     .<-tuple: (compose .<-tuple-list vector->list)
     .ethabi-tail-length: (lambda (x) (ethabi-tail-length types (.tuple-list<- x)))
     .ethabi-encode-into:
     (lambda (x bytes start head get-tail set-tail!)
       (ethabi-encode-into types (.tuple-list<- x) bytes start head get-tail set-tail!))
     .ethabi-decode-from:
     (lambda (bytes start head get-tail set-tail!)
       (.<-tuple (ethabi-decode-from types bytes start head get-tail set-tail!)))
     }})

(.def (Tuple. @ Type. types)
  type-list: (vector->list types)
  ethabi-display-type: (cut ethabi-display-types type-list <>)
  ethabi-static?: (every (cut .@ <> ethabi-static?) type-list)
  ethabi-head-length: (ethabi-head-length type-list)
  methods: =>.+ {(:: @ [bytes<-un/marshal])
    .json<-: (lambda (v) (vector->list (vector-map (lambda (_ t x) (json<- t x)) types v)))
    .<-json: (lambda (j) (vector-map (lambda (_ t x) (<-json t x)) types (if (list? j) (list->vector j) j)))
    .marshal: (lambda (v port)
                (vector-for-each (lambda (_ type val) (marshal type val port))
                                 types v))
    .unmarshal: (lambda (port) (vector-map (lambda (_ type) (unmarshal type port)) types))
    .ethabi-tail-length: (lambda (x) (ethabi-tail-length type-list (vector->list x)))
    .ethabi-encode-into:
    (lambda (x bytes start head get-tail set-tail!)
      (ethabi-encode-into type-list (vector->list x) bytes start head get-tail set-tail!))
    .ethabi-decode-from:
    (lambda (bytes start head get-tail set-tail!)
      (list->vector (ethabi-decode-from type-list bytes start head get-tail set-tail!)))
  })
(def (Tuple . types_) ;; type of tuples, heterogeneous arrays of given length and type
  (def types (list->vector (map (cut validate Type <>) types_)))
  {(:: @ Tuple.) (types)})

;; Untagged union. Can be used for JSON, but no automatic marshaling.
(.def (Union. @ Type. types)
  sexp: `(Union ,@(map (cut .@ <> sexp) types))
  .element?: (λ (x) (any (cut element? <> x) types))
  methods: {(:: @ [bytes<-un/marshal])
    .json<-: (lambda (v) (let/cc return
                      (for-each (λ (type)
                                  (when (element? type v) (return (json<- type v))))
                                types)
                      (error "invalid element of type" v sexp)))
    .<-json: (lambda (j) (let/cc return
                      (for-each (λ (type)
                                  (with-catch void (lambda () (return (<-json type j)))))
                                types)
                      (error "invalid json for type" j sexp)))})
(def (Union . types) ;; type of tuples, heterogeneous arrays of given length and type
  {(:: @ Union.) (types)})

(.def (Enum. @ Type. vals)
  sexp: `(Enum. ,@vals)
  .element?: (cut member <> vals)
  .vals@: (list->vector vals)
  .json@: (list->vector (map json-normalize vals))
  n-bytes: (n-bytes<-n-bits (integer-length (vector-length .vals@)))
  ethabi-display-type: (lambda (port) (fprintf port "uint~d" (* 8 n-bytes)))
  ethabi-static?: #t
  ethabi-head-length: 32
  methods: {(:: @ [un/marshal<-bytes])
    length-in-bytes: n-bytes
    .<-nat: (cut vector-ref .vals@ <>)
    .nat<-: (lambda (v) (vector-index (cut equal? <> v) .vals@))
    .json<-: (lambda (v) (vector-ref .json@ (.nat<- v)))
    .<-json: (lambda (j) (vector-index (cut equal? <> j) .json@))
    .bytes<-: (compose (cut bytes<-nat <> length-in-bytes) .nat<-)
    .<-bytes: (compose .<-nat nat<-bytes)
    .ethabi-padding: (- 32 length-in-bytes)
    .ethabi-tail-length: (lambda (_) 0)
    .ethabi-encode-into:
    (lambda (x bytes start head get-tail set-tail!)
      (u8vector-uint-set! bytes (+ head .ethabi-padding) (.nat<- x) big length-in-bytes))
    .ethabi-decode-from:
    (lambda (bytes start head get-tail set-tail!)
      (ensure-zeroes bytes head .ethabi-padding)
      (.<-nat (u8vector-uint-ref bytes (+ head .ethabi-padding) big length-in-bytes)))
})
(defrule (Enum values ...) {(:: @ Enum.) vals: '(values ...)})

(.def (FixedVector. @ Type. type size)
  sexp: `(Vector ,(.@ type sexp) ,(.@ type size))
  .element?: (let (e? (.@ type methods .element?))
               (lambda (x) (and (vector? x) (= (vector-length x) size) (vector-every e? x))))
  ethabi-display-type: (lambda (port) (.call type ethabi-display-type port)
                          (fprintf port "[~d]" size))
  ethabi-static?: (.@ type ethabi-static?)
  ethabi-element-head-length: (.@ type ethabi-head-length)
  ethabi-head-length: (* ethabi-element-head-length size)
  ethabi-element-tail-length: (.@ type methods .ethabi-tail-length)
  methods: {(:: methods [bytes<-un/marshal])
    .json<-: (lambda (v) (vector-map (lambda (_ x) (.method type .json<- x)) v))
    .<-json: (lambda (j) (vector-unfold (lambda (_ l) (values (.method type .json<- (car l)) (cdr l))) (length j) j))
    .marshal: (let (m (.@ type methods .marshal))
                (lambda (v port) (vector-for-each (lambda (_ x) (m x port)) v)))
    .unmarshal: (let (u (.@ type methods .unmarshal))
                  (lambda (port) (vector-unfold (lambda (_) (u port)) size)))
    .ethabi-tail-length: (lambda (x) (vector-fold (lambda (_ acc v) (+ acc (ethabi-element-tail-length v))) 0 x))
    .ethabi-encode-into:
    (lambda (x bytes start head get-tail set-tail!)
      (vector-for-each (lambda (i v)
                         (.method type .ethabi-encode-into
                                  v bytes start (+ head (* i ethabi-element-head-length))
                                  get-tail set-tail!)) x))
    .ethabi-decode-from:
    (lambda (bytes start head get-tail set-tail!)
      (vector-unfold (lambda (i)
                       (.method type .ethabi-decode-from
                                bytes start (+ head (* i ethabi-element-head-length))
                                get-tail set-tail!)) size))
    })
(.def (DynamicVector. @ Type. type)
  sexp: `(Vector ,(.@ type sexp))
  .element?: (let (e? (.@ type methods .element?))
               (lambda (x) (and (vector? x) (vector-every e? x))))
  ethabi-display-type: (lambda (port) (.call type ethabi-display-type port) (display "[]" port))
  ethabi-static?: #f
  ethabi-element-head-length: (.@ type ethabi-head-length)
  ethabi-head-length: 32
  ethabi-element-tail-length: (.@ type methods .ethabi-tail-length)
  methods: {(:: methods [bytes<-un/marshal])
    .json<-: (lambda (v) (vector-map (lambda (_ x) (.method type .json<- x)) v))
    .<-json: (lambda (j) (vector-unfold (lambda (_ l) (values (.method type .json<- (car l)) (cdr l))) (length j) j))
    .marshal: (let (m (.@ type methods .marshal))
                (lambda (v port) (write-uint16 (vector-length v) port)
                   (vector-for-each (lambda (_ x) (m x port)) v)))
    .unmarshal: (let (u (.@ type methods .unmarshal))
                  (lambda (port)
                    (def size (read-uint16 port))
                    (vector-unfold (lambda (_) (u port)) size)))
    .ethabi-tail-length: (lambda (x) (vector-fold (lambda (_ acc v) (+ acc (ethabi-element-tail-length v)))
                                             (+ 32 (* ethabi-element-head-length (vector-length x))) x))
    .ethabi-encode-into:
    (lambda (x bytes start head get-tail set-tail!)
      (def tail (get-tail))
      (def new-start (+ tail 32))
      (def new-tail (+ new-start (* ethabi-element-head-length (vector-length x))))
      (set-tail! new-tail)
      (u8vector-uint-set! bytes head (- tail start) big 32)
      (u8vector-uint-set! bytes tail (vector-length x) big 32)
      (vector-for-each (lambda (i v)
                         (.method type .ethabi-encode-into
                                  v bytes new-start (+ new-start (* i ethabi-element-head-length))
                                  get-tail set-tail!)) x))
    .ethabi-decode-from:
    (lambda (bytes start head get-tail set-tail!)
      (def tail (get-tail))
      (assert! (= tail (+ start (u8vector-uint-ref bytes head big 32))))
      (def new-start (+ tail 32))
      (set-tail! new-start)
      (def size (u8vector-uint-ref bytes tail big 32))
      (set-tail! (+ new-start (* ethabi-element-head-length size)))
      (vector-unfold (lambda (i)
                       (.method type .ethabi-decode-from
                                bytes new-start (+ new-start (* i ethabi-element-head-length))
                                get-tail set-tail!))
                     size))
    })
(def (Vector type (size #f))
  (if size
    {(:: @ FixedVector.) (type) (size)}
    {(:: @ DynamicVector.) (type)}))

;; TODO:
#;(def (TaggedUnion . plist)
  (def a (map/car sym<-kw (alist<-plist plist)))
  (def tag-bits (integer-length (1- (length a))))
  ...)

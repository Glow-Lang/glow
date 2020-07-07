(export #t)
(export Type. .defgeneric validate element? .method define-type)

;; We are shadowing existing types. Should we monkey-patch them instead? Let's hope not.

(import
  (for-syntax :gerbil/gambit/exact :std/iter :glow/compiler/common)
  :gerbil/gambit/bits :gerbil/gambit/bytes :gerbil/gambit/exact
  :gerbil/gambit/hash :gerbil/gambit/ports
  :std/format :std/iter :std/misc/bytes :std/misc/completion :std/misc/list
  :std/sort :std/srfi/1 :std/srfi/43 :std/sugar :std/text/json
  :clan/utils/base :clan/utils/io :clan/utils/json :clan/utils/list
  :clan/utils/maybe :clan/utils/number :clan/utils/syntax
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
(.def (Nat @ [methods.bytes<-marshal poo.Nat] .validate)
  .sexp<-: (lambda (x) `(nat<-0x ,(0x<-nat x)))
  .json<-: 0x<-nat
  .<-json: (compose .validate nat<-0x))

(.def (NatSet @ poo.IntSet)
  sexp: 'NatSet
  .Int: Nat)

(def (ensure-zeroes bytes start len)
  (for (i (in-range len))
    (assert! (zero? (bytes-ref bytes (+ start i))))))

(def simple-eth-types (make-hash-table))
(def (register-simple-eth-type type (name (.@ type .ethabi-name)))
  (hash-put! simple-eth-types name type))

;; Integer types
(.def (UInt. @ [poo.UInt.] .length-in-bits .length-in-bytes .validate)
  .json<-: 0x<-nat
  .<-json: (compose .validate nat<-0x)
  .ethabi-name: (format "uint~d" .length-in-bits)
  .ethabi-display-type: (cut display .ethabi-name <>)
  .ethabi-head-length: 32
  .ethabi-padding: (- 32 .length-in-bytes)
  .ethabi-tail-length: (lambda (_) 0)
  .ethabi-encode-into:
  (lambda (x bytes start head get-tail set-tail!)
    (u8vector-uint-set! bytes (+ head .ethabi-padding) x big .length-in-bytes))
  .ethabi-decode-from:
  (lambda (bytes start head get-tail set-tail!)
    (ensure-zeroes bytes head .ethabi-padding)
    (u8vector-uint-ref bytes (+ head .ethabi-padding) big .length-in-bytes)))
(defsyntax (defUIntNs stx)
  (with-syntax ((((id i)...)
                 (for/collect (j (in-range 8 257 8))
                   [(datum->syntax (stx-car stx) (format-symbol "UInt~d" j)) j])))
    #'(begin
        (.def (id @ UInt.) .length-in-bits: i sexp: 'id)...
        (register-simple-eth-type id)...
        (register-simple-eth-type UInt256 "uint"))))
(defUIntNs)

;; Bytes types
(.def (BytesN. @ poo.BytesN. n .ethabi-name .validate)
  sexp: `(BytesN ,n)
  .ethabi-name: (format "bytes~d" n)
  .ethabi-display-type: (cut display .ethabi-name <>)
  .ethabi-head-length: 32
  .sexp<-: (lambda (x) `(bytes<-0x ,(0x<-bytes x)))
  .json<-: 0x<-bytes
  .<-json: (compose .validate bytes<-0x)
  .ethabi-padding: (- 32 n)
  .ethabi-tail-length: (lambda (_) 0)
  .ethabi-encode-into:
  (lambda (x bytes start head get-tail set-tail!)
    (subu8vector-move! x 0 n bytes head))
  .ethabi-decode-from:
  (lambda (bytes start head get-tail set-tail!)
    (def end (+ head n))
    (ensure-zeroes bytes end .ethabi-padding)
    (subu8vector bytes head end)))

(defsyntax (defBytesNs stx)
  (def (foo n) [(datum->syntax (stx-car stx) (format-symbol "Bytes~d" n)) n])
  (with-syntax* ((((rid ri)...) (map foo (iota 32 1)))
                 (((uid ui)...) (map foo [60 65 256])) ;; Shh id / Secp256k1 sig&pubkey / Bloom filter
                 (((id i)...) #'((rid ri)... (uid ui)...)))
    #'(begin
        (defrule (d name n) (.def (name @ BytesN.) n: n sexp: 'name))
        (d id i)...
        (register-simple-eth-type rid)...)))
(defBytesNs)

(.def (BytesL16 @ [methods.bytes<-marshal BytesN.])
   sexp: 'BytesL16
   .Length: UInt16
   .ethabi-name: "bytes"
   .ethabi-display-type: (cut display .ethabi-name <>)
   .ethabi-head-length: 32
   .element?: (λ (x) (and (bytes? x) (<= (bytes-length x) 65535)))
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
     (subu8vector bytes data-start (+ data-start len))))
(define-type Bytes BytesL16)
(register-simple-eth-type Bytes)

(.def (String @ poo.String)
   .element?: (λ (x) (and (string? x)
                          (or (< (string-length x) 16384)
                              (and (not (< 65535 (string-length x)))
                                   (<= (bytes-length (string->bytes x)) 65535)))))
   .ethabi-name: "string"
   .ethabi-display-type: (cut display .ethabi-name <>)
   .ethabi-head-length: 32
   .Bytes: Bytes
   .ethabi-tail-length: (rcompose string->bytes (.@ .Bytes .ethabi-tail-length))
   .ethabi-encode-into:
   (lambda (x bytes start head get-tail set-tail!)
     (.call .Bytes .ethabi-encode-into (string->bytes x) bytes start head get-tail set-tail!))
   .ethabi-decode-from:
   (lambda (bytes start head get-tail set-tail!)
     (bytes->string (.call .Bytes .ethabi-decode-from bytes start head get-tail set-tail!))))
(register-simple-eth-type String)

;; TODO: have a function that only interns the string to a symbol if already found?
(.def (Symbol @ poo.Symbol)
  .ethabi-name: "string"
  .ethabi-display-type: (cut display .ethabi-name <>)
  .ethabi-head-length: 32
  .String: String
  .<-string: maybe-intern-symbol
  .ethabi-tail-length: (rcompose symbol->string (.@ .String .ethabi-tail-length))
  .ethabi-encode-into:
  (lambda (x bytes start head get-tail set-tail!)
    (.call .String .ethabi-encode-into (symbol->string x) bytes start head get-tail set-tail!))
  .ethabi-decode-from:
  (lambda (bytes start head get-tail set-tail!)
    (maybe-intern-symbol (.call .String .ethabi-decode-from bytes start head get-tail set-tail!))))

(.def (Bool @ poo.Bool)
  .ethabi-name: "bool"
  .ethabi-display-type: (cut display .ethabi-name <>)
  .ethabi-head-length: 32
  .ethabi-padding: 31
  .ethabi-tail-length: (lambda (_) 0)
  .ethabi-encode-into:
  (lambda (x bytes start head get-tail set-tail!)
    (u8vector-set! bytes (+ head 31) (if x 1 0)))
  .ethabi-decode-from:
  (lambda (bytes start head get-tail set-tail!)
    (ensure-zeroes bytes head 31)
    (case (u8vector-ref bytes (+ head 31))
      ((1) #t) ((0) #f) (else (error "Invalid bool")))))
(register-simple-eth-type Bool)

;; Records
;; TODO: support defaults
(def (RecordSlot type . options)
  (def o (.<-alist (map (match <> ([k . v] (cons (symbolify k) v))) (alist<-plist options))))
  {(:: @ [o]) (type) optional: (.has? o default)})
(def (Record . plist)
  (def a (map (match <> ([kw type . options] (cons (symbolify kw) (apply RecordSlot type options))))
              (alist<-plist plist)))
  {(:: @ [methods.bytes<-marshal Class.] proto)
   sexp: ['Record . plist]
   slots: (.<-alist a)
   slot-names: (map car a)
   types: (map (lambda (s) (.@ (.ref slots s) type)) slot-names)
   optionals: (map (lambda (s) (.@ (.ref slots s) optional)) slot-names)
   .ethabi-display-type: (cut ethabi-display-types types <>)
   .ethabi-head-length: (ethabi-head-length types)
   .sexp<-: (lambda (v) `(.cc (proto ,sexp)
                         ,@(append-map (lambda (s t o)
                                         (when/list (or (not o) (.key? v s))
                                           [[(keywordify s) (sexp<- t (.ref v s))]]))
                                       slot-names types optionals)))
   .json<-: (lambda (v) (list->hash-table
                    (append-map (lambda (s t o) (when/list (or (not o) (.key? v s))
                                             [(cons (symbol->string s) (json<- t (.ref v s)))]))
                         slot-names types optionals)))
   .<-json: (lambda (j)
              (.mix (.<-alist (append-map (lambda (s t o)
                                            (def ss (symbol->string s))
                                            (when/list (or (not o) (hash-key? j ss))
                                              [(cons s (<-json t (hash-ref j ss)))]))
                                          slot-names types optionals))
                    proto))
   .marshal: (lambda (v port) (for-each (lambda (s t o)
                                     (if o
                                       (let (has? (.key? v s))
                                         (marshal Bool (.key? v s) port)
                                         (when has? (marshal t (.ref v s) port)))
                                       (marshal t (.ref v s) port)))
                                   slot-names types optionals))
   .unmarshal: (lambda (port)
                 (.mix (.<-alist
                        (with-list-builder (c)
                          (for-each (lambda (s t o)
                                      (if o
                                        (let (has? (unmarshal Bool port))
                                          (when has? (c (cons s (unmarshal t port)))))
                                        (c (cons s (unmarshal t port)))))
                                    slot-names types optionals)))
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
     (.<-tuple (ethabi-decode-from types bytes start head get-tail set-tail!)))})

(.def (Tuple. @ poo.Tuple. type-list)
  .ethabi-display-type: (cut ethabi-display-types type-list <>)
  .ethabi-head-length: (ethabi-head-length type-list)
  .ethabi-tail-length: (lambda (x) (ethabi-tail-length type-list (vector->list x)))
  .ethabi-encode-into:
  (lambda (x bytes start head get-tail set-tail!)
    (ethabi-encode-into type-list (vector->list x) bytes start head get-tail set-tail!))
  .ethabi-decode-from:
  (lambda (bytes start head get-tail set-tail!)
    (list->vector (ethabi-decode-from type-list bytes start head get-tail set-tail!))))
(def (Tuple . types_) ;; type of tuples, heterogeneous arrays of given length and type
  (def types (list->vector (map (cut validate Type <>) types_)))
  {(:: @ Tuple.) (types)})

;; Untagged union. Can be used for JSON, but no automatic marshaling.
(.def (Union. @ [poo.methods.string&bytes&marshal<-json Type.] types)
  sexp: `(Union ,@(map (cut .@ <> sexp) types))
  .element?: (λ (x) (any (cut element? <> x) types))
  .json<-: (lambda (v) (let/cc return
                    (for-each (λ (t) (when (element? t v) (return (json<- t v))))
                              types)
                    (error "invalid element of type" v sexp)))
  .<-json: (lambda (j) (let/cc return
                    (for-each (λ (t) (ignore-errors (return (<-json t j))))
                              types)
                    (error "invalid json for type" j sexp))))
(def (Union . types) ;; type of tuples, heterogeneous arrays of given length and type
  {(:: @ Union.) (types)})

(.def (Enum. @ [poo.Enum.] vals .nat<- .<-nat .length-in-bytes)
  .ethabi-name: (format "uint~d" (* 8 .length-in-bytes))
  .ethabi-display-type: (cut display .ethabi-name <>)
  .ethabi-head-length: 32
  .ethabi-padding: (- 32 .length-in-bytes)
  .ethabi-tail-length: (lambda (_) 0)
  .ethabi-encode-into:
  (lambda (x bytes start head get-tail set-tail!)
    (u8vector-uint-set! bytes (+ head .ethabi-padding) (.nat<- x) big .length-in-bytes))
  .ethabi-decode-from:
  (lambda (bytes start head get-tail set-tail!)
    (ensure-zeroes bytes head .ethabi-padding)
    (.<-nat (u8vector-uint-ref bytes (+ head .ethabi-padding) big .length-in-bytes))))
(defrule (Enum values ...) {(:: @ Enum.) vals: '(values ...)})

(.def (FixedVector. @ [methods.bytes<-marshal Type.] type size)
  sexp: `(Vector ,(.@ type sexp) ,(.@ type size))
  .element?: (let (e? (.@ type .element?))
               (lambda (x) (and (vector? x) (= (vector-length x) size) (vector-every e? x))))
  .ethabi-display-type: (lambda (port) (.call type .ethabi-display-type port)
                           (fprintf port "[~d]" size))
  .ethabi-element-head-length: (.@ type .ethabi-head-length)
  .ethabi-head-length: (* .ethabi-element-head-length size)
  .ethabi-element-tail-length: (.@ type .ethabi-tail-length)
  .json<-: (lambda (v) (vector-map (lambda (_ x) (json<- type x)) v))
  .<-json: (lambda (j) (vector-unfold (lambda (_ l) (values (json<- type (car l)) (cdr l))) (length j) j))
  .marshal: (let (m (.@ type .marshal))
              (lambda (v port) (vector-for-each (lambda (_ x) (m x port)) v)))
  .unmarshal: (let (u (.@ type .unmarshal))
                (lambda (port) (vector-unfold (lambda (_) (u port)) size)))
  .ethabi-tail-length: (lambda (x) (vector-fold (lambda (_ acc v) (+ acc (.ethabi-element-tail-length v))) 0 x))
  .ethabi-encode-into:
  (lambda (x bytes start head get-tail set-tail!)
    (vector-for-each (lambda (i v)
                       (.call type .ethabi-encode-into
                              v bytes start (+ head (* i .ethabi-element-head-length))
                              get-tail set-tail!)) x))
  .ethabi-decode-from:
  (lambda (bytes start head get-tail set-tail!)
    (vector-unfold (lambda (i)
                     (.call type .ethabi-decode-from
                            bytes start (+ head (* i .ethabi-element-head-length))
                            get-tail set-tail!)) size)))
(.def (DynamicVector. @ [methods.bytes<-marshal Type.] type)
  sexp: `(Vector ,(.@ type sexp))
  .element?: (let (e? (.@ type .element?))
               (lambda (x) (and (vector? x) (vector-every e? x))))
  .ethabi-display-type: (lambda (port) (.call type .ethabi-display-type port) (display "[]" port))
  .ethabi-element-head-length: (.@ type .ethabi-head-length)
  .ethabi-head-length: 32
  .ethabi-element-tail-length: (.@ type .ethabi-tail-length)
  .json<-: (lambda (v) (vector-map (lambda (_ x) (json<- type x)) v))
  .<-json: (lambda (j) (vector-unfold (lambda (_ l) (values (json<- type (car l)) (cdr l))) (length j) j))
  .marshal: (let (m (.@ type .marshal))
              (lambda (v port) (write-uint16 (vector-length v) port)
                 (vector-for-each (lambda (_ x) (m x port)) v)))
  .unmarshal: (let (u (.@ type .unmarshal))
                (lambda (port)
                  (def size (read-uint16 port))
                  (vector-unfold (lambda (_) (u port)) size)))
  .ethabi-tail-length: (lambda (x) (vector-fold (lambda (_ acc v) (+ acc (.ethabi-element-tail-length v)))
                                           (+ 32 (* .ethabi-element-head-length (vector-length x))) x))
  .ethabi-encode-into:
  (lambda (x bytes start head get-tail set-tail!)
    (def tail (get-tail))
    (def new-start (+ tail 32))
    (def new-tail (+ new-start (* .ethabi-element-head-length (vector-length x))))
    (set-tail! new-tail)
    (u8vector-uint-set! bytes head (- tail start) big 32)
    (u8vector-uint-set! bytes tail (vector-length x) big 32)
    (vector-for-each (lambda (i v)
                       (.call type .ethabi-encode-into
                              v bytes new-start (+ new-start (* i .ethabi-element-head-length))
                              get-tail set-tail!)) x))
  .ethabi-decode-from:
  (lambda (bytes start head get-tail set-tail!)
    (def tail (get-tail))
    (assert! (= tail (+ start (u8vector-uint-ref bytes head big 32))))
    (def new-start (+ tail 32))
    (set-tail! new-start)
    (def size (u8vector-uint-ref bytes tail big 32))
    (set-tail! (+ new-start (* .ethabi-element-head-length size)))
    (vector-unfold (lambda (i)
                     (.call type .ethabi-decode-from
                            bytes new-start (+ new-start (* i .ethabi-element-head-length))
                            get-tail set-tail!))
                   size)))
(def (Vector type (size #f))
  (if size
    {(:: @ FixedVector.) (type) (size)}
    {(:: @ DynamicVector.) (type)}))

;; TODO:
#;(def (TaggedUnion . plist)
  (def a (map/car sym<-kw (alist<-plist plist)))
  (def tag-bits (integer-length (1- (length a))))
  ...)

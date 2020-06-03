(export #t)

(import
  :gerbil/gambit/bytes :gerbil/gambit/ports
  :std/misc/bytes :std/sugar
  :clan/utils/base
  :clan/poo/poo :clan/poo/mop
  )

(def (write-u8vector v p) (write-subu8vector v 0 (u8vector-length v) p))
(def (read-u8vector v p) (def l (u8vector-length v)) (read-subu8vector v 0 l p l))

(def (read-uint16 port) ;; big endian
  (def hi (read-u8 port))
  (def lo (read-u8 port))
  (fx+ lo (fxarithmetic-shift hi 8)))

(def (read-sized16-bytes port)
  (def size (read-uint16 port))
  (read-bytes size port))

(def (write-uint16 n port)
  (assert! (<= 0 n 65535))
  (def bytes (make-bytes 2))
  (bytevector-u16-set! bytes 0 n big)
  (write-bytes bytes port))

(def (write-sized16-bytes bytes port)
  (write-uint16 (bytes-length bytes) port)
  (write-bytes bytes port))

(def (bytes<-<-marshal marshal)
  (nest (lambda (bytes)) (call-with-output-u8vector) (lambda (port))
        (marshal bytes port)))

(def (<-bytes<-unmarshal unmarshal)
  (nest (lambda (bytes)) (call-with-input-u8vector bytes) (lambda (port))
        (begin0 (unmarshal port))
        (assert! (eq? #!eof (read-u8 port)))))

(def (marshal<-bytes<- bytes<-)
  (lambda (x port) (write-u8vector (bytes<- x) port)))

(def (unmarshal<-<-bytes <-bytes n)
  (lambda (port) (<-bytes (read-bytes n port))))

(.defgeneric (marshal type x port)
   slot: .marshal)

(.defgeneric (unmarshal type port)
   slot: .unmarshal)

(.def (bytes<-un/marshal @ [] .marshal .unmarshal)
   .bytes<-: (bytes<-<-marshal .marshal)
   .<-bytes: (<-bytes<-unmarshal .unmarshal))


(export #t)

(import
  :gerbil/gambit/bytes :gerbil/gambit/ports
  :std/misc/bytes :std/sugar
  )

(def (write-u8vector v p) (write-subu8vector v 0 (u8vector-length v) p))
(def (read-u8vector v p) (def l (u8vector-length v)) (read-subu8vector v 0 l p l))

(def (read-n-bytes n port)
  (def bytes (make-bytes n))
  (read-bytes bytes)
  bytes)

(def (read-uint16 port)
  (bytevector->uint (read-n-bytes 2 port) big))

(def (read-sized16-bytes port)
  (def size (read-uint16 port))
  (read-n-bytes size port))

(def (write-uint16 n port)
  (assert! (<= 0 n 65535))
  (def bytes (make-bytes 2))
  (bytevector-u16-set! bytes 0 n big)
  (write-bytes bytes port))

(def (write-sized16-bytes bytes port)
  (write-uint16 (bytes-length bytes) port)
  (write-bytes bytes port))


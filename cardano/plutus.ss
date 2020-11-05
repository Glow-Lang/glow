(export #t)

(import :std/format)

(defvalues (default-version)
  (values '1.0.0))

(def (program version body)
  ['program version body])

;; GRAMMAR
(def (con x)
  ['con x])

(def (lam x body)
  ['lam x body])

(def (delay x)
  ['delay x])

(def (apply x y)
  [x y])

(def (force x)
  ['force x])

(def (error)
  ['error])

(def (addInteger x y)
  ['builtin 'addInteger (con x) (con y)])

;; BUILT-INS
;; (defbuiltin (addInteger Integer Integer) Integer) -> (def (addInteger x y) ['builtin 'addInteger x y)) plus typechecking
;; (defbuiltin (subtractInteger Integer Integer) Integer)
;; (defbuiltin (multiplyInteger Integer Integer) Integer)
;; (defbuiltin (divideInteger Integer Integer) Integer)
;; (defbuiltin (modInteger Integer Integer) Integer)
;; (defbuiltin (quotientInteger Integer Integer) Integer)
;; (defbuiltin (remainderInteger Integer Integer) Integer)
;; (defbuiltin (lessThanInteger Integer Integer) Bool)
;; (defbuiltin (lessThanEqualsInteger Integer Integer) Bool)
;; (defbuiltin (greaterThanInteger Integer Integer) Bool)
;; (defbuiltin (equalsInteger Integer Integer) Bool)
;; (defbuiltin (emptyByteString) ByteString)
;; (defbuiltin (concatenate ByteString ByteString) ByteString)
;; (defbuiltin (equalsByteString ByteString ByteString) Boolean)
;; (defbuiltin (lessThanByteString ByteString ByteString) Boolean)
;; (defbuiltin (greaterThanByteString ByteString ByteString) Boolean)
;; (defbuiltin (takeByteString Integer ByteString) ByteString)
;; (defbuiltin (dropByteString Integer ByteString) ByteString)
;; (defbuiltin (sha2_256 ByteString) ByteString)
;; (defbuiltin (sha3_256 ByteString) ByteString)
;; (defbuiltin (verifySignature ByteString ByteString ByteString) ByteString)

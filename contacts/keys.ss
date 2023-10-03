;;;; Only encrypted secret keys are stored in the database.
;;;; The global encryption key is stored in a separate file.

(export #t)

(import
 ;; gerbil
 :gerbil/gambit
 :std/crypto
 ;; gerbil-utils
 :clan/config
 :clan/io)

;; The key used to encrypt all secret keys.
(def global-key #f)
(def (global-key-path)
  (xdg-config-home "glow" "global.key"))

;; The default cipher used to encrypt secret keys.
;; TODO: cipher::aes-256-gcm would be a better choice since it supports
;; authentication, but Gerbil's crypto drivers can't handle it yet.
(def secret-key-cipher cipher::aes-256-ctr)

;; Supported ciphers, indexed by name.
(def secret-key-ciphers
  (list->hash-table
   (map (lambda (cipher) (cons (cipher-name cipher) cipher))
        [cipher::aes-256-ctr])))

;; Read or create a global key, stored as raw bytes on disk.
;; TODO: Obtain key from an OS-level key management service.
(def (ensure-global-key!)
  (unless global-key
    (let ((key-length (cipher-key-length secret-key-cipher))
          (key-path (global-key-path)))
      (cond ((file-exists? key-path)
             (if (= key-length (file-size key-path))
                 (set! global-key (call-with-input-file key-path
                                    (cut unmarshal-n-u8 key-length <>)))
                 (error "Global key length does not match secret key cipher"
                        (cipher-name secret-key-cipher))))
            (else
             (set! global-key (random-bytes key-length))
             (call-with-output-file [path: (global-key-path) permissions: #o600]
               (cut write-u8vector global-key <>))))))
  global-key)

;; Encrypt a secret key with the global key and a random IV (nonce).
(def (encrypt-secret-key secret-key)
  (let* ((cipher (and secret-key secret-key-cipher
                      (make-cipher secret-key-cipher)))
         (iv (and cipher
                  (random-bytes (cipher-iv-length cipher)))))
    (if (and cipher iv secret-key)
        (values (encrypt cipher (ensure-global-key!) iv secret-key) iv)
        (values #f #f))))

;; Decrypt a secret key with the global key.
(def (decrypt-secret-key cipher-name iv secret-key)
  (let ((cipher (and cipher-name iv secret-key
                     (hash-key? secret-key-ciphers cipher-name)
                     (make-cipher (hash-ref secret-key-ciphers cipher-name)))))
    (if cipher
        (decrypt cipher (ensure-global-key!) iv secret-key)
        (error "Cannot decrypt secret key"))))

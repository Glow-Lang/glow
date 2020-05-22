(export #t)

;; TODO: move stuff here, then move it out of here into gerbil-utils or gerbil itself

;; Simple client
(import
  :gerbil/gambit/bytes :gerbil/gambit/threads
  :std/misc/bytes :std/misc/completion :std/sugar)

(def (simple-client send! make-message)
  (lambda (request)
    (def c (make-completion))
    (send! (make-message request (cut completion-post! c <>)))
    (cut completion-wait! c)))

(def (simple-server receive! process!)
  (while #t
    (match (receive!)
      ([input . continuation]
       (continuation (process! input))))))

(def (make-simple-client-server make-processor (name (##procedure-name make-processor)))
  (def (server-loop)
    (simple-server thread-receive (make-processor)))
  (def server (spawn/name name server-loop))
  (simple-client (cut thread-send server <>) cons))

(def (completion-done val)
  (def c (make-completion))
  (completion-post! c val)
  c)

;; (deftype (FutureStream A) (Fun (Or Nil (Cons A (FutureStream A)))))
(def future-nil (lambda () '()))
(defrule (future-cons head tail) (lambda () (cons head tail)))

(def (future-stream-take n s)
  (unless (<= 0 n) (error "Negative value" 'future-stream-take n))
  (let loop ((acc []) (s s) (n n))
    (if (zero? n) (values (reverse acc) s)
        (match (s)
          ([] (error "Took too many entries from this stream!"))
          ([hd . tl] (loop [hd . acc] tl (1- n)))))))

(defclass simple-actor (state thread) constructor: :init!)
(defmethod {:init! simple-actor}
  (lambda (self init (name 'simple-actor))
    (def mx (make-mutex 'init))
    (mutex-lock! mx)
    (def (thunk)
      (set! (simple-actor-state self) (init))
      (mutex-unlock! mx)
      (while #t
        (match (thread-receive)
          ((? procedure? f) (set! (simple-actor-state self) (f (simple-actor-state self)))))))
    (set! (simple-actor-thread self) (spawn/name name thunk))
    (with-lock mx true)))
(defmethod {peek simple-actor} (cut simple-actor-state <>))
(defmethod {poke simple-actor} (lambda (self transform) (thread-send (simple-actor-state self) transform)))
(defmethod {action simple-actor}
  (lambda (self f in)
    (def c (make-completion))
    (def (g state) (let (new (f state))
                     (completion-post! c new)
                     new))
    (thread-send (simple-actor-state self) g)
    (completion-wait! c)))

;; ~~~~~~~~ p2p.ss ~~~~~~~~~
;;
;; p2p communication methods for glow

(export #t)

;;~~~~~~~~~~~~~~~~~~~~Import~~~~~~~~~~~~~~~
(import
  :std/misc/channel
  :std/iter
  :std/net/bio
  :std/sugar
  :std/format
  :std/os/pid
  :clan/debug
  :clan/exception
  :gerbil/gambit/exceptions
  :clan/path-config
  :clan/shell
  :clan/io
  :gerbil/gambit/ports
  :mukn/ethereum/known-addresses
  ./pb/private-key
  :std/misc/uuid
  ../compiler/project/runtime-2
    :gerbil/gambit/threads
  :vyzo/libp2p
  (only-in :vyzo/libp2p/client make-client)
  :vyzo/libp2p/daemon)


;; ------------------ Libp2p client generation~~~~~~~~~~~~~~~~~~~~
;;
;; For a complete reference of libp2p API.
;; See: https://github.com/vyzo/gerbil-libp2p#libp2p-api
;;
;; To understand what multiaddresses are,
;; see: https://github.com/multiformats/multiaddr
;;
;; The implementation of client methods here are influenced by those found in:
;; https://github.com/vyzo/gerbil-libp2p/blob/master/example/libp2p-chat.ss

;; TODO: Eventually upstream changes to gerbil-libp2p to accept passing
;; a seckey via an environment variable, instead of a file.

(def (ensure-libp2p-client nickname: nickname host-address: host-address)
    (new-libp2p-client nickname: nickname host-address: host-address))

(def (new-libp2p-client nickname: nickname host-address: host-address)
  (call-with-seckey-tempfile nickname: nickname
    (lambda (seckey-filename)
      (open-libp2p-client
       host-addresses: host-address
       wait: 5
       options: ["-id" seckey-filename]))))

(def (get-libp2p-client path: path)
  (displayln "Path:")
  (displayln path)
  (and (file-exists? path)
    (let ()
      (def libp2p-daemon (use-libp2p-daemon! path))
      (make-client libp2p-daemon (make-mutex 'libp2p-client) (make-hash-table) #f #f #f))))

;; Creates a seckey temporary file,
;; and passes its filename as an argument to f,
;; purging the file after that.
;; The seckey used is determined using the nickname.
(def (call-with-seckey-tempfile nickname: nickname f)
  (def file-name (make-seckey-tempfile nickname: nickname))
  (with-unwind-protect (lambda () (f file-name))
                       (lambda () (delete-file file-name))))

(def (make-seckey-tempfile nickname: nickname
                           filename: (filename (string-append "/tmp/glow-seckey-"
                                                              (uuid->string (random-uuid)))))
  (def seckey (get-my-seckey nickname: nickname))
  (def seckey/bytes (export-secret-key/bytes seckey))
  (def seckey/proto (make-seckey/proto seckey/bytes: seckey/bytes))
  (write-seckey/proto filepath: filename seckey/proto: seckey/proto)
  filename)

(def (write-seckey/proto filepath: filepath seckey/proto: seckey/proto)
  (def buf (open-file-output-buffer filepath))
  (bio-write-PrivateKey seckey/proto buf)
  (bio-force-output buf)
  (close-file-input-buffer buf))

;; TODO: accept other types of secret keys
(def (make-seckey/proto seckey/bytes: seckey/bytes)
  (make-PrivateKey Type: 'Secp256k1 Data: seckey/bytes))

;; TODO: Upstream to gerbil-ethereum
(def (get-my-seckey nickname: nickname)
  (def my-address (address<-nickname nickname))
  (def my-seckey (secret-key<-address my-address))
  my-seckey)


;; FIXME:
;; In the upstream branch,
;; the `start-libp2p-daemon!' procedure doesn't redirect output
;; to a logfile, instead just directly outputs it to the terminal.
;;
;; To fix this we have to:
;; 1. Upstream to gambit
;;    Extend open-process to allow you to redirect to file or file descriptor.
;;    (In our case this allows us to redirect the output / error messages to a log-file)
;;    See how `run-program` in uiop does this:
;;    https://common-lisp.net/project/asdf/uiop.html#UIOP_002fRUN_002dPROGRAM
;;
;; 2. Upstream to gerbil-libp2p
;;    process-options to use the redirecting option
;;    for writing the error logs to a file instead
;;    of the console.
;;
;; In the short run we use the shell to redirect error logs to a file (see the implementation below).
(def (start-libp2p-daemon! host-addresses: (host-addrs #f) daemon: (bin "p2pd")
                           options: (options [])
                           address: (sock #f)
                           wait: (timeo 0.4)
                           p2pd-log-path: (p2pd-log-path (log-path "p2pd.log")))
  (cond
   ((current-libp2p-daemon)
    => values)
   (else
    (let* ((path (or sock (string-append "/tmp/p2pd." (number->string (getpid)) ".sock")))
           (addr (string-append "/unix" path))

           (raw-cmd (escape-shell-tokens [bin "-q" "-listen" addr
                                          (if host-addrs ["-hostAddrs" host-addrs "-pubsub" "-connManager"] [])...
                                          options ...]))
           (cmd (format "{ echo ~a ; exec ~a ; } < /dev/null >> ~a 2>&1"
                 raw-cmd raw-cmd p2pd-log-path))

           (proc (open-process [path: "/bin/sh" arguments: ["-c" cmd]]))

           (d (daemon proc path)))
      (cond
       ((process-status proc timeo #f)
        => (lambda (status)
             (error "p2pd exited prematurely" status))))
      (current-libp2p-daemon d)
      d))))



;; NOTE: This is needed to call initialize our version
;; of the daemon process (by `start-libp2p-daemon!'),
;; see the implementation above.
;; TODO: Once the above changes are upstreamed for `start-libp2p-daemon!',
;; this command can be made obsolete too.
(def (open-libp2p-client host-addresses: (host-addresses #f) options: (args [])  address: (sock #f)  wait: (timeo 12) (path #f)) ;; Extra arguments host-address and options
  (let (d (start-libp2p-daemon! host-addresses: host-addresses options: args address: sock wait: timeo)) ;; Should go with host-address/tranpsort/port
    (make-client d (make-mutex 'libp2p-client) (make-hash-table) path #f #f)))


;;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~Direct p2p communication methods
;;
;;
;; peer-multiaddr-str: destination multiaddress,
;; It has the additional constraint that it needs to contain a peerId,
;; so we can verify the recipient's identity.
;;
;; host-addresses: Multi addresses this participant listens to on their host machine.
;;
;; contents: string
;;
;; This is used to connect to a destination address of another participant,
;; and send the contents over the opened connection.
;; If the other participant is not online,
;; it will poll until `timeout'.
(def (dial-and-send-contents libp2p-client dest-address-str contents timeout: (timeout 10))
  (let* ((self (libp2p-identify libp2p-client))
         (peer-multiaddr (string->peer-info dest-address-str)))
    (for (p (peer-info->string* self))
      (displayln "I am " p))
    (displayln "Connecting to " dest-address-str)
    (libp2p-connect/poll libp2p-client peer-multiaddr timeout: timeout)
    (let (s (libp2p-stream libp2p-client peer-multiaddr [chat-proto]))
      (chat-writer s contents)
      (stream-close s))))

;; This is used to connect to a destination address of another participant,
;; and send the contents over the opened connection.
;; If the other participant is not online,
;; it will poll until `timeout'.
(def (libp2p-connect/poll libp2p-client peer-multiaddr timeout: (timeout #f))
  (try (libp2p-connect libp2p-client peer-multiaddr)
    (catch (e)
      (printf "Unable to connect to client ~a ...\n" (peer-info->string peer-multiaddr))
      (display-exception e)
      (if (and timeout (> timeout 0))
          (let ()
            (displayln "Polling again in 1s...")
            (thread-sleep! 1)
            (libp2p-connect/poll libp2p-client peer-multiaddr timeout: (- timeout 1)))
          (error "Timeout while trying to connect to client.")))))


;; This is a libp2p protocol spec.
;; These protocols work on the application level.
;; See: https://docs.libp2p.io/concepts/protocols/
;;
;; NOTE: This libp2p-protocol is a placeholder,
;; it's not an actual libp2p protocol.
;; It just falls back to plaintext messaging.
;;
;; When we want to have a specific protocol
;; we will need to `register' handlers for them.
;; See: https://docs.libp2p.io/concepts/protocols/#handler-functions
(def chat-proto "/chat/1.0.0")

;; This function reads the
(def (chat-reader s)
  (let lp ()
    (let (line (bio-read-line (stream-in s)))
      (cond
       ((eof-object? line)
        (displayln "*** STREAM CLOSED"))
       ((string-empty? line)
        (displayln "*** Received"))
       (else
        line)))))

;; `s' here is a stream between two peers,
;; which is opened by the client.
;; This writes contents to the stream.
(def (chat-writer s contents)
  (display "> ")
  (bio-write-string contents (stream-out s))
  (bio-write-char #\newline (stream-out s))
  (bio-force-output (stream-out s)))

;;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Pubsub methods for Retreiving, Creating, and Adding data for ethaddr->multiaddr~~~~~~~~~~~~~~~~~


;;connect-to-multiaddr-pubsub
;; pubsub-node: the multiaddr of a pubsub node to connect to
;; c: a libp2p client object representing you
;; connects to the provided pubsub node and subscribes to the "chat" pubsub
;; also spawns a reader that will respond to identify requests
;;
;; returns (values pubsubchatchannel cancelprocforpubsubchannel readerthread)
(def (connect-to-multiaddr-pubsub pubsub-node: pubsub-node c: c my-0xaddr: my-0xaddr)
  (libp2p-connect c (string->peer-info pubsub-node))
  (let*-values (((sub cancel) (pubsub-subscribe c "chat"))
                (reader (spawn subscription-reader sub c my-0xaddr)))
    (values sub cancel reader)))

;; subscription-reader
;; sub : the pubsub channel
;; c : the libp2p client object
;; my-0xaddr : the blockchain address of you
;; will read the pubsub channel and when requested to identify itself, it will broadcast the client's identity
;;
;; TODO: Allow for specific identity requests
(def (subscription-reader sub c my-0xaddr)
 (let ((self (libp2p-identify c)))
   (for (m sub)

     ;;if message says "IDENTIFY" then publish your 0xaddr, else do nothing
     (if (string=? "IDENTIFY" (bytes->string (vector-ref m 1)))
       (pubsub-publish c "chat" (string->bytes my-0xaddr))))))

;; get-peerID-from-pubsub
;;
;; sub : the pubsub chat channel
;; c : your libp2p client
;; peer0x : the peer you are waiting for to come online
;;
;; ask everyone in pubsub channel to IDENTIFY, then read messages until you see one matching peer0x
;; block until peerID is found
;; TODO: verify that peer0x comes from correct sender
;; returns the peer-ID related to peer0x
(def (get-peerID-from-pubsub sub
                             c
                             peer0x
                             timeout)
  (pubsub-publish c "chat" (string->bytes "IDENTIFY"))

  (let lp ()
    (let ((m (channel-get sub 5)))
      (if m
        ;;if the message matches the peer0x you are requesting, return the peerID, else move on TODO: VERIFY FROM CORRECT SENDER AS WELL
        (if (string=? peer0x (bytes->string (vector-ref m 1)))
          (ID->string (vector-ref m 0)) ;;FIXME: Does this return the id??
          (lp))

        (if (and timeout (> timeout 0))
          (begin
            (displayln "Failed to find peer ... Trying again ...")
            (set! timeout (- timeout 1))
            (pubsub-publish c "chat" (string->bytes "IDENTIFY"))
            (lp))
          (error "Timeout when getting peerID from pubsub"))))))

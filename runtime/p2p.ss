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
  :clan/concurrency
  :gerbil/gambit/ports
  :mukn/ethereum/known-addresses
  ./pb/private-key
  :std/misc/uuid
  ../compiler/project/runtime-2
    :gerbil/gambit/threads
    :vyzo/libp2p
    :vyzo/libp2p/client
  :vyzo/libp2p/daemon)




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
(def (dial-and-send-contents libp2p-client dest-address-str contents max-retries: (max-retries 10))
  (let* ((self (libp2p-identify libp2p-client))
         (peer-multiaddr (string->peer-info dest-address-str)))
    (for (p (peer-info->string* self))
      (displayln "I am " p))
    (displayln "Connecting to " dest-address-str)
    (libp2p-connect/poll libp2p-client peer-multiaddr max-retries: max-retries)
    (let (s (libp2p-stream libp2p-client peer-multiaddr [chat-proto]))
      (chat-writer s contents)
      (stream-close s))))

;; This is used to connect to a destination address of another participant,
;; and send the contents over the opened connection.
;; If the other participant is not online,
;; it will poll until `timeout' seconds.
(def (libp2p-connect/poll libp2p-client peer-multiaddr max-retries: (max-retries 10))
  (retry retry-window: 1 max-window: 10 max-retries: max-retries
         (lambda ()
           (displayln "Trying to connect to peer...")
           (libp2p-connect libp2p-client peer-multiaddr))))


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

;; This function reads an entry in stream s.
;; If entry is eof, then close stream
;; If entry is empty string, then print "Received"
;; else, return the stream entry
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




;;~~~~~~~~~~~~~~~~~~~~~~~~~~ Glow Bootstrap Node Methods for testing ~~~~~~~~~~~~~~~~~~~



;; do-bootstrap
;; host-addresses: a string of the ip address in multiaddr format to start the bootstrap node at
;;
;; returns: (values client-object-of-bootstrap-node daemon-object-of-bootstrap-node)
;; starts a bootstrap node
(def (do-bootstrap host-addresses)
 (let* ((boot-d (start-libp2p-daemon! host-addresses: host-addresses options: ["-relayHop" "-pubsub" "-connManager"] wait: 10))
        (boot-c (open-libp2p-client host-addresses: host-addresses wait: 5)))

   (pubsub-subscribe boot-c "chat")
   (values boot-c boot-d)))

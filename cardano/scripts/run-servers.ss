#!/usr/bin/env gxi

(import
  :mukn/glow/cardano/smart-contract-backend)

(def (server-thread)
  (make-thread
    (lambda ()
      (run-all-servers))))

(def (contract-outbox-thread)
  (make-thread
    (lambda ()
      (process-contract-outboxes))))

(def (delete-file-if-exists file-path)
  (when (file-exists? file-path)
    (delete-file file-path)))

(def (run-servers!)
  (begin
    (delete-file-if-exists "node-server.sock")
    (delete-file-if-exists "scb-core.db")
    (delete-file-if-exists "scb-core.db-shm")
    (delete-file-if-exists "scb-core.db-wal")

    (run-migrate)
    (thread-start! (server-thread)))
    (thread-start! (contract-outbox-thread))

    (thread-sleep! 100))

(begin
  (println "RUNNING SERVERS\n")
  (run-servers!))


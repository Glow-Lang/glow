(export #t)

(import
  :std/sugar :std/test
  :clan/utils/list :clan/poo/poo
  :clan/runtime/db
  ../signing ../json-rpc ../transaction ../nonce-tracker
  ./transaction-integrationtest)

(def nonce-tracker-integrationtest
  (test-suite "integration test for glow/ethereum/nonce-tracker"
    (test-case "Croesus nonce tracker"
      (defrule (in-tx foo ...) (with-tx (tx) (foo ... tx)))
      (in-tx .call NonceTracker reset croesus)
      (def initial (in-tx .call NonceTracker peek croesus))
      (check initial ? (cut <= 0 <>))
      (in-tx .call NonceTracker next croesus)
      (with-tx (tx)
        (.call NonceTracker next croesus tx)
        (.call NonceTracker next croesus tx)
        (.call NonceTracker next croesus tx))
      (check-equal? (in-tx .call NonceTracker peek croesus) (+ 4 initial))
      (in-tx .call NonceTracker reset croesus)
      (check-equal? (in-tx .call NonceTracker peek croesus) initial))))

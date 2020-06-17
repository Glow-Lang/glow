(export #t)

(import
  :std/test
  :clan/utils/list :clan/poo/poo
  :clan/runtime/db
  ../signing ../json-rpc ../transaction ../nonce-tracker
  ./transaction-integrationtest)

(def nonce-tracker-integrationtest
  (test-suite "integration test for glow/ethereum/nonce-tracker"
    (test-case "Croesus nonce tracker"
      (with-tx (tx)
        (.call NonceTracker reset croesus tx)
        (def initial (.call NonceTracker peek croesus))
        (check initial ? (cut <= 0 <>))
        (.call NonceTracker next croesus tx)
        (.call NonceTracker next croesus tx)
        (.call NonceTracker next croesus tx)
        (.call NonceTracker next croesus tx)
        (check-equal? (.call NonceTracker peek croesus) (+ 4 initial))
        (.call NonceTracker reset croesus tx)
        (check-equal? (.call NonceTracker peek croesus) initial)))))

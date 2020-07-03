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
      (.call NonceTracker reset croesus)
      (def initial (.call NonceTracker peek croesus))
      (check initial ? (cut <= 0 <>))
      (.call NonceTracker next croesus)
      (.call NonceTracker next croesus)
      (.call NonceTracker next croesus)
      (.call NonceTracker next croesus)
      (check-equal? (.call NonceTracker peek croesus) (+ 4 initial))
      (.call NonceTracker reset croesus)
      (check-equal? (.call NonceTracker peek croesus) initial))))

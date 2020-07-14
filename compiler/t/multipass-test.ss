(export #t)

(import
  :std/srfi/13 :std/test
  :utils/assert
  :glow/compiler/multipass :glow/compiler/passes)

(def multipass-test
  (test-suite "test suite for glow/compiler/multipass"
    (test-case "member/take"
      (assert-equal! (member/take 4 [1 2 3 4 5 6 7 8 9]) [1 2 3 4])
      (assert-equal! (member/take 4 [1 3 5 7 9]) #f))
    (test-case "identify-layer"
      (assert-equal! (identify-layer "foo.alpha.sexp") 'alpha.sexp)
      (assert-equal! (identify-layer "foo.beta.sexp") 'sexp))))

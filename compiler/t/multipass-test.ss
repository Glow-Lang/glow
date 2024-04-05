(export #t)

(import
  :std/srfi/13 :std/test
  :clan/assert
  :mukn/glow/compiler/multipass :mukn/glow/compiler/passes)

(def multipass-test
  (test-suite "test suite for glow/compiler/multipass"
    (test-case "member/take"
      (check (member/take 4 [1 2 3 4 5 6 7 8 9]) => [1 2 3 4])
      (check (member/take 4 [1 3 5 7 9]) => #f))
    (test-case "identify-layer"
      (check (identify-layer "foo.alpha.sexp") => 'alpha.sexp)
      (check (identify-layer "foo.beta.sexp") => 'sexp))))

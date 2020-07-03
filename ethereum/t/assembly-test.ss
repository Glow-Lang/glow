
(import :std/sort
        :std/iter
        :std/test
        :std/srfi/1
        :std/misc/list
        :glow/ethereum/assembly
        (only-in :clan/utils/number bytes<-nat)
        :clan/pure/dict/assq)

;; jumplabel-len : Nat
;; jump2-len : Nat
(def jumplabel-len 1)
(def jump2-len 4)

;; test-labels : [Assqof Symbol Nat] -> TestSuite
(def (test-labels names/nats)
  (unless (pair? names/nats) (error 'test-labels "expected at least one label"))
  (def n-labels (length names/nats))
  (let ((names/nats (sort names/nats (lambda (x y) (< (cdr x) (cdr y))))))
   (def names (map car names/nats))
   (def nats (map cdr names/nats))
   (def n-jumps (apply max (map cdr names/nats)))
   (def nats/names (map pair-flip names/nats))
   (def (nat->i-code nat)
     (unless (and (integer? nat) (not (negative? nat))) (error 'nat->i-code))
     ; if directly on one of the nats, it's on the JUMPDEST, not after
     (def n-labels-before (count (lambda (n) (< n nat)) nats))
     (+ (* nat jump2-len) (* n-labels-before jumplabel-len)))
   (def instrs
     (flatten1
      (for/collect ((i (in-range (1+ n-jumps))))
        (def instr-jump [jump2 (list-ref names (modulo i n-labels))])
        (def ?name-label (assq i nats/names))
        (cond (?name-label [[jumplabel (cdr ?name-label)] instr-jump])
              (else        [instr-jump])))))
   (def code (assemble instrs))
   (test-case "check code length"
     (check-equal? (u8vector-length code) (+ (nat->i-code (last nats)) jumplabel-len jump2-len)))
   (test-case "check JUMPDEST on every jumplabel"
     (for ((nat nats))
       (def i-code (nat->i-code nat))
       (def j-code (+ i-code jumplabel-len))
       ;; #x5b is JUMPDEST
       (check-equal? (subu8vector code i-code j-code) #u8(#x5b))))
   (test-case "check jump2 code pointers"
     (for ((i (in-range n-jumps)))
       (def nat-jump (list-ref nats (modulo i n-labels)))
       (def i-code-dest (nat->i-code nat-jump))
       (def i-code-jump (+ (nat->i-code i) (if (member i nats) jumplabel-len 0)))
       (def j-code-jump (+ i-code-jump jump2-len))
       (check-equal? (subu8vector code i-code-jump j-code-jump)
                     ;; #x61 is PUSH2, #x56 is JUMP
                     (u8vector-append #u8(#x61) (bytes<-nat i-code-dest 2) #u8(#x56)))))))

;; pair-flip : (cons a b) -> (cons b a)
(def (pair-flip p) (with ((cons a b) p) (cons b a)))

(def assembly-test
  (test-suite "test suite for glow/ethereum/assembly"
    (test-labels '((a . 13) (b . 8) (c . 21) (d . 0) (e . 34) (f . 3) (g . 5)))))

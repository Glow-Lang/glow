(@module
 (@ (interaction (@list A B))
  (def publishHello
    (λ ()
      (@ A (def ha "Hello, B, I am A. How do you do?"))
      (publish! A ha)

      (@ B (def hb "Hello, A. I am B. How do YOU do?"))
      (publish! B hb)))))

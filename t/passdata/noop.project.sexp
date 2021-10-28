(@module (begin end)
         (@label begin)
         (@debug-label dlb)
         (def noop
              (@make-interaction
               ((@record (participants (@list A B)) (assets (@list DefaultToken))))
               ()
               (begin0 end0)
               (#f (@label begin0) (@debug-label dlb0) (return (@tuple)) (@label end0))
               (A (@label begin0) (@debug-label dlb0) (return (@tuple)) (@label end0))
               (B (@label begin0) (@debug-label dlb0) (return (@tuple)) (@label end0))))
         (return (@tuple))
         (@label end))

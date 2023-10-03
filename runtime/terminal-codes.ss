(export #t)

(import :gerbil/gambit)

(def TTY (tty? (current-output-port)))
(def END (if TTY "\033[0m" ""))
(def BOLD (if TTY "\033[1m" ""))
(def CYAN (if TTY "\033[96m" ""))
(def MAGENTA (if TTY "\33[35m" ""))
(def RED (if TTY "\033[91m" ""))
(def FAIL RED)

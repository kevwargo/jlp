(for (c "lepecbeke")
     (print c))
(print "")

(for (word (java.util.Scanner "one two three"))
     (printf "word: %s" word))

(let ((it (iterator (java.util.Scanner "one two three"))))
  (for ((has-next it))
       (print (next it))))

; empty iterator
(for (i (iterator))
     (print i))

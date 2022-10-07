(for-each (c "lepecbeke")
          (print c))
(print "")

(for-each (word (java.util.Scanner "one two three"))
          (printf "word: %s" word))

(let ((it (iterator (java.util.Scanner "one two three"))))
  (while (has-next it)
         (print (next it))))

; empty iterator
(for-each (i (iterator))
          (print i))

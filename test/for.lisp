(for ((setq i 0) (not (equalp i 10)) (setq i (+ i 1)))
     (printf "i: %d" i))
(print (boundp 'i))
(print "")

(for-each (i '(a b c "str" (:a :b)))
          (print i))
(print "")

(setq x 0)
(while (not (equalp x 10))
       (printf "x: %d" x)
       (setq-global x (+ x 2)))
(print "")

(print (let ((it (iterator '(a x b x c x d x fff))))
         (while (has-next it)
                (emit (next it))
                (next it))))

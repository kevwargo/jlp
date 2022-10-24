(defun sleep-for (duration)
  (start-thread
   (-> Thread sleep (duration))
   (printf "Slept for %d" duration)))

(sleep-for 2000l)
(setq joinable (sleep-for 1000l))
(print "after start")

(-> joinable join ())
(print "after join")

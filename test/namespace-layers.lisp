(setq a 0)
(print (boundp 'a))

(del 'a)
(print (boundp 'a))

(let ((a 1))
  (print (boundp 'a))
  (setq b 2)
  (print (boundp 'b)))
(print (boundp 'b))

(setq defun "override")
(print (type defun))
(del 'defun)
(print (type defun))

(let ((foo 'bar))
  (print foo)
  (setq foo 'x)
  (print foo)
  (setq-global foo 'y)
  (print foo))
(print foo)

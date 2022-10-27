(defun outer (a)
  (lambda () `(,a)))
(setq fn (outer 2))
(print (fn))

(setq fn (let ((a 5)
               (b 6))
           (let ((a 'override))
             (lambda () `(,a ,b)))))
(print (fn))

(setq fn (let (lexical)
           (lambda () `(,(boundp 'lexical) ,(boundp 'dynamic)))))
(let (dynamic)
  (print (fn)))

(setq use-global-var (lambda () (if (boundp 'global-var) `(,global-var))))

(print (boundp 'outer))
(print (boundp 'fn))
(print (boundp 'lexical))
(print (boundp 'dynamic))
(print (boundp 'a))
(print (boundp 'b))

(print (use-global-var))
(setq global-var 1)
(print (use-global-var))

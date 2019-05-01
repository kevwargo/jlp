(defun mapcar (func lst)
  (collect
   (for (elt lst)
        (yield (func elt)))))

(defun car (list)
  (nth 0 list))

(defun cdr (list)
  (let ((idx 0))
    (collect
     (for (x list)
          (if (not (equalp idx 0))
              (yield x))
          (setq idx (+ idx 1))))))

(defmacro cond (&rest clauses)
  (for (clause clauses)
       (if (eval (car clause))
           (return (append (list 'progn)
                           (cdr clause))))))

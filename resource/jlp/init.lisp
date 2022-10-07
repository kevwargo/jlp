(defun mapcar (func lst)
  (for (elt lst)
       (emit (func elt))))

(defmacro setq (&rest defs)
  `(progn
     ,@(let ((it (iterator defs)))
         (for ((has-next it))
              (emit `(set ',(next it) ,(next it)))))))

(defmacro setq-global (&rest defs)
  `(progn
     ,@(let ((it (iterator defs)))
         (for ((has-next it))
              (emit `(set-global ',(next it) ,(next it)))))))

(defun car (list)
  (nth 0 list))

(defun cdr (list)
  (let ((idx 0))
    (for (x list)
         (if (not (equalp idx 0))
             (emit x))
         (setq idx (+ idx 1)))))

(defmacro cond (&rest clauses)
  (for (clause clauses)
       (if (eval (car clause))
           (return (if (cdr clause)
                       (append (list 'progn)
                               (cdr clause))
                       (car clause))))))

(defmacro %~> (object &rest methods)
  (for (method methods)
       (setq object
             (if (isinstance method list)
                 `((. ,object ,(car method)) ,@(cdr method))
                 `((. ,object ,method)))))
  object)

(defmacro ++ (var)
  (list setq var (list + var 1)))

(defmacro -- (var)
  (list setq var (list - var 1)))

(defun printf (fmt &rest args)
  (print (apply format
                (append (list fmt)
                        args))))

(defmacro with-getters (getters object &rest body)
  `(let (,@(mapcar (lambda (getter)
                     `(,getter ((. ,object (concat "get"
                                                   ,(capitalize (str getter)))))))
                   getters))
     ,@body))

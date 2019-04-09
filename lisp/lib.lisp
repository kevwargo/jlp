(defmacro ++ (var)
  (list setq var (list + var 1)))

(defmacro -- (var)
  (list setq var (list - var 1)))

(defun printf (fmt &rest args)
  (print (apply format
                (append (list fmt)
                        args))))

(defmacro ++ (var)
  (list 'setq var (list '+ var 1)))

(defmacro -- (var)
  (list 'setq var (list '- var 1)))

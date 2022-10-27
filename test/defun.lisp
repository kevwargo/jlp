(defun test-fn-1 (a (b c)
                &optional x (y 'default)
                &rest rest
                &key k1 (k2 'default-key-value)
                  &other-keys other-keys)
  `(,a ,b ,c ,x ,y ,rest ,k1 ,k2 ,other-keys))

(print (test-fn-1 1 '(2 3) :k1 333))
(print (test-fn-1 'x1 '("asdf" "foobar") 4 5 'ok 'nope :k2 2 :k1 1 :k3 'o :key4 'p))

(defun test-fn-2 (a b ;positionals
               (unpacked-1 unpacked-2) (unp-1 (unp-nested-1 unp-nested-2)) ;unpacked
               &optional opt1 (opt2 'non-nil-default) ;optionals
               &rest rest ;rest
               &key k1 k2 (k3 'default) ;keywords
                 &other-keys all-keys ;custom keywords
                 )
  `(,a ,b ,unpacked-1 ,unpacked-2 ,unp-1 ,unp-nested-1 ,unp-nested-2 ,opt1 ,opt2 ,rest ,k1 ,k2 ,all-keys))

(print (test-fn-2 1 2 '(3 4) '(5 (6 7)) 'o1 'o2 'r1 'r2 'r3 :k1 'v1 :k2 'v2 :other1 'ov1 :other2 'ov2))

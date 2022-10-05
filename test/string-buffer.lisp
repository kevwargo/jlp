(setq sb (StringBuffer))
((. sb append) "foo ")
((. sb append) 235)
((. sb append) " bar")
(print sb)

(setq sb (StringBuffer "init: "))
((. sb append) "test")
(print sb)

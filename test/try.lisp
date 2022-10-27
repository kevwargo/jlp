(try asdfasdfasdfasdfadsfsad
     (kevwargo.jlp.exceptions.LispException (print "LispException 1"))
     (finally (print "finally 1")))
(print)

(try (java.io.FileInputStream "53fb6eef-02af-4a9c-89ed-1422b3a01a83")
     ((java.io.FileNotFoundException e) (print e)))

(print (try (java.io.FileInputStream "bbcdb361-f64f-4111-b197-640c3411e204")
            (java.io.FileNotFoundException)
            (finally (print "finally 2"))))

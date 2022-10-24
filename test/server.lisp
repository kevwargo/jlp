(setq addr (-> java.net.InetAddress getLocalHost ()))
(setq server (java.net.ServerSocket 0 5 addr))
(setq port (-> server getLocalPort ()))

(let (client)
  (start-thread
   (setq client (-> server accept ()))
   (print "Client connected")
   (print "Server response" :stream (-> client getOutputStream ()))
   (-> client close ())))

(let* ((client (java.net.Socket addr port))
       (stream (java.io.BufferedReader (java.io.InputStreamReader (-> client getInputStream ())))))
  (printf "Received: '%s'" (-> stream readLine ()))
  (-> client close ()))

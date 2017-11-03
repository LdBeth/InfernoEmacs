#/usr/bin/env newlisp
;; A SDF.ORG `com` style IRC interface.

(define nick (env "NICK"))

(define (send-input str , input)
  (write-line (or input current-file) str))

(define (msg-loop)
  (catch
      (while true
        (print (format "%s@%s " nick chan))
        (handle-input (read-line)))))

(define (handle-input str)
  (case str
    ("" (println "Nothing was sent.")
     (throw true))
    (true (send-input str))))

(define (command-loop)
  (while true
    (print "%s %s" chan serv)
    (case (read-key)
      (113 (quit)); q
      (108 (glist)); l
      (103 (goto)); g
      (65 (afk)); A
      (97 (anwser)); a
      (114 (rhist)); r
      (116 (topic)); t
      (104 (help)); h
      (true (msg-loop))
      )))

(define (msg-loop)
  ())

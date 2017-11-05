#/usr/bin/env newlisp
;; A SDF.ORG `com` style IRC interface.

(context 'com)

(define (send-input str , input)
  (write-line (or input current-file) str))

(define (msg-loop)
  (catch
      (while true
        (print (format "%s@%s " MAIN:nick MAIN:chan))
        (handle-input (read-line))))
  (command-loop))

(define (handle-input str)
  (when (= str "")
    (println "Nothing was sent.")
    (throw true))
  (send-input str)
  (let (cmd (append (lower-case (regex "^/[A-Za-z0-9]+" str)) " "))
    (cond
     ((regex "^/quit |^/q" cmd)
      (println "Quitting...Wait processes to quit.")
      (exit)
      ;; (until process-quit)
      )
     ;; ("/join" open-new-window)
     ;;((regex "^/part |^/leave") leave)
     )))

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



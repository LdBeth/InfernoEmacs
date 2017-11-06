#/usr/bin/env newlisp
;; A SDF.ORG `com` style IRC interface.

(context 'com)

(define (get-list)
  (exec (format )))

(define (send-input str , input)
  (write-line (or input current-file) str))

(define (msg-loop)
  (catch
      (while true
        (print (format "%s@%s " MAIN:nick MAIN:chan))
        (handle-input (read-line)))))

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
      (113 (println "Detach to process.")
           (exit)); q - Quit
      (108 (glist)); l - Get list
      (103 (goto)); g -Goto
      (65 (afk)); A - Away
      (97 (anwser)); a - reply
      (114 (rhist)); r - Show history
      (116 (topic)); t - Set topic
      (104 (help)); h -Dispay help
      (true (msg-loop))
      )))



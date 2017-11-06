#!/usr/bin/env newlisp

(define (msg-loop)
  (print ">> ")
  (while (!= (set 'line (read-line)) "/exit")
    (write-line input line)
    (print ">> ")))

(define (main)
  (set 'input (open ".irc/irc.freenode.net/#linuxba/in" "write"))
  (println "Now you can start typing. Use /EXIT to leave.")
  (msg-loop)
  (close input)
  (exit))

(main)

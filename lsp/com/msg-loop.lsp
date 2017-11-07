#!/usr/bin/env newlisp

(define dir "/Users/ldbeth/.irc/irc.freenode.net/#linuxba/")

(context 'msg)

(define (loop)
  (print ">> ")
  (while (!= (set 'line (read-line)) "/exit")
    (write-line input line)
    (print ">> ")))

(define (main)
  (set 'input (open (append MAIN:dir "in") "write"))
  (println "Now you can start typing. Use /EXIT to leave.")
  (msg-loop)
  (close input))

(context 'tail)

(define (loop)
  (while true (read-line MAIN:out)
         (unless (= (current-line) "")
           (println (current-line)))))


(context 'MAIN)

(define (loop)
  (catch
      (while true
        (spawn 'p1 (tail:loop))
        (when (= (read-key) 113); q
          (throw 1))
        (msg:loop))))

(define (main)
  (set 'out (open (append dir "out") "read"))
  (loop)
  (close out)
  (exit))

(main)

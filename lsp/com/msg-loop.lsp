#!/usr/bin/env newlisp

(define dir "/Users/ldbeth/.irc/irc.freenode.net/#linuxba/")

(context 'msg)

(define (loop)
  (print ">> ")
  (while (!= (set 'line (read-line)) "")
    (write-line input line)
    (print ">> ")))

(define (main)
  (set 'input (open (append MAIN:dir "in") "write"))
  (println "Now you can start typing. Type a empty line to quit loop.")
  (loop)
  (close input))

(context 'tail)

(define (loop)
  (while true
    (unless (= (peek MAIN:out) 0)
      (read-line MAIN:out)
      (println (current-line)))))

;; (define (main)
;;   (process (append "/usr/bin/tail -f " MAIN:dir "out") 0 0))

(context 'MAIN)

(define (loop)
  (catch
      (while true
        (spawn 'ps (tail:loop))
        (when (= (read-key) 113); q
          (throw 1))
        (abort)
        (msg:main))))

(define (main)
  (set 'out (open (append dir "out") "read"))
  (loop)
  (close out)
  (exit))

(main)

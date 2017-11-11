#!/usr/bin/env newlisp

(define dir "/Users/ldbeth/.irc/irc.freenode.net/#linuxba/")

(context 'msg)

(define (loop , line)
  (print ">> ")
  (while (and (!= (set 'line (read-line)) "") line)
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
    (while (read-line MAIN:out)
      (println (current-line)))
    (sleep 2000)))

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

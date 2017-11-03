#!/usr/bin/env newlisp

;; Servers
(define freenode
  '((server "irc.freenode.net")
    (channels "#linuxba" "#gentoo-cn" "#conkeror")))

;; Network
(define networks
  '(freenode))

;; Timeout
(define timeout 25)

(context 'con)

(define (main)
  (dolist (net networks)
    (letn (data (eval net)
                ser (1 (assoc 'server data))
                chan (1 (assoc 'channels data))
                po (last (or (assoc 'port data) '(6667)))
                in)
      (unless
          (=
           (catch
               (while ser
                 (letn (serv (pop ser)
                             inf (append (env "IRC") "/" serv "/in"))
                   (setq in inf)
                   (exec (append "rm " in))
                   (println
                    (format "Connecting to network %s, server %s"
                            (string net) serv))

                   (! (format "/usr/bin/env iim -i %s -n %s -k %s -p %d &"
                               (env "IRC") (env "NICK") serv po))
                   ;; (process)
                   (let (t 1)
                     (while (not (when (file? in) (throw 0)))
                       (if (= t timeout) (throw 1))
                       (println (format "Connecting ... %d " t))
                       (inc t)
                       (sleep 1000)))
                   (println "Timeout. Retry.")))) 0)
        (println "Abort. Quitting.")
        (exit 1))

      (set 'input (open in "write"))
      (dolist (ch chan)
        (write-line input (append "/j " ch)))
      (close input)

            (println "Connected.")))
  (exit 0))


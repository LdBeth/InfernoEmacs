(pushnew #P"~/Public/Projects/" ql:*local-project-directories*)
(ql:register-local-projects)

(ql:quickload :cl-irc)
(ql:quickload :linedit)
(require "PTY")

(defpackage #:lirc
  (:use #:common-lisp #:irc #:linedit)
  (:export #:cmd-loop)
  (:documentation "A line based IRC client."))

(in-package #:lirc)

(defun join-channels (list &aux (connection (car list)))
  (mapc #'(lambda (channel) (join connection channel))
        (cadr list)))


(defvar *current-connection*)
(defvar *current-channel*)
(defparameter *command-table* (make-hash-table :test 'equal))
(defparameter *message-process* (ccl:make-process 'message-proc))

(defun cmd-loop ()
(ccl::disable-tty-local-modes 0 #$ICANON)
(ccl::disable-tty-local-modes 0 #$ECHO)
(format t "Hello, this is lirc v0.0.1
press q to quit.
")
(ccl:process-preset *message-process* #'read-message-loop *current-connection*)
(ccl:process-enable *message-process*)
  (loop
    (exec (read-char))))

(defmacro defcommand (char function)
  `(setf (gethash ,char *command-table*) ,function))

(defun prompt (p)
  (let ((l (linedit:linedit :prompt (format nil ":~A > " p))))
    (if (string= l "")
        (throw 'abort nil)
        l)))

(defcommand #\q
    (lambda () (quit *current-connection*)
      (ccl:quit 0)))

(defcommand #\g
    (lambda ()
      (let ((c (prompt "goto")))
        (if (gethash c (channels *current-connection*))
            (setf *current-channel* c)
            (format t "no such channel: ~A~%" c)))))


(defun msg ()
  (privmsg *current-connection* *current-channel* (prompt "ldb")))

(defun exec (c)
  (ccl:process-suspend *message-process*)
  (catch 'abort
    (let ((f (gethash c *command-table*)))
      (funcall (or f 'msg))))
  (ccl:process-resume *message-process*))



(load #P"~/.lirc")
#|
(defparameter *freenode* `(,(connect :nickname "john" :password "*******"
                                     :server "irc.freenode.org")
                           ("#linuxba" "#lisp")))

(join-channels *freenode*)

(setf *current-connection* (car *freenode*))
(setf *current-channel* "#linuxba")
|#


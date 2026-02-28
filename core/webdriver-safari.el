;; -*- lexical-binding: t; -*-
(require 'webdriver)

(defclass webdriver-service-safari (webdriver-service)
  ((executable
    :initform "safaridriver"
    :documentation "Executable when running a Firefox Service.
This is usually \"safaridriver\", and it should be in `exec-path'.")
   (port
    :initform 4444
    :documentation "Port to pass as an option to geckodriver.
By default, it is 4444, which is the default for geckodriver.")
   (buffer
    :initform " *webdriver-safaridriver*"
    :initarg :buffer
    :type (or null string buffer)
    :documentation "Buffer to use for I/O with the process."))
  "The Firefox Service, that runs the geckodriver.")

(setq webdriver-default-service 'webdriver-service-safari)

(cl-defmethod webdriver-service-get-port ((self webdriver-service-safari))
  "Get port where SELF is listening on.

Looks up the port from the associated buffer to SELF.  It is an error to call
this function if SELF doesn't have an associated buffer."
  (oref self port))

(cl-defmethod webdriver-service-start :before
  ((self webdriver-service-safari) &optional _retries &rest _process-args)
  "Add the port argument to the command line."
  (unless (member "--port" (oref self args))
    (oset self args (append (oref self args)
                            (list "--port" (number-to-string (oref self port)))))))

(provide 'webdriver-safari)

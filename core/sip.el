;;; sip.el -*- lexical-binding:t -*-

;;; Commentary:

;;; Code:

(require 'url)
(require 'auth-source)

;;; Customization variables start here.

(defgroup openvoip nil
  "An interface for aria2c command."
  :group 'external
  :group 'execute
  :prefix "voip-")

(defcustom voip-http-port 6060
  "Port on which JSON HTTP server will listen."
  :type '(integer :tag "Http port")
  :group 'openvoip)

(defcustom voip-sip-domain "sip.sdf.org"
  "The server to connect with."
  :type 'string
  :group 'openvoip)

(defcustom voip-sip-extention "1257"
  "The username on SIP server."
  :type 'string
  :group 'openvoip)

(defun voip-send-request (cmd method &optional data)
  (let ((url-request-method method)
        (url-request-data (and data
                               (json-serialize
                                data)))
        (url-request-extra-headers
         (and data
              '(("Content-Type" . "application/json"))))
        url-history-track
        json-response)
    (with-current-buffer (url-retrieve-synchronously
                          (format "http://localhost:%d/%s"
                                  voip-http-port
                                  cmd)
                          t)
      ;; read last line, where json response is
      (goto-char (point-max))
      (beginning-of-line)
      (setq json-response (json-parse-buffer :object-type 'alist))
      (kill-buffer))
    json-response))

;;;###autoload
(defun voip-login ()
  (interactive)
  (let ((found (nth 0 (auth-source-search :host voip-sip-domain
                                          :user voip-sip-extention))))
    (when found
      (voip-send-request "login" "POST"
                         `((username . ,(or voip-sip-extention
                                            (plist-get found :user)
                                            (error "Username cannot be nil!")))
                           (password . ,(let ((secret (plist-get found :secret)))
                                          (if (functionp secret)
                                              (funcall secret)
                                            secret)))
                           (domain . ,voip-sip-domain))))))

(defun voip-dial (number)
  (interactive "Mnumber: ")
  (message "Dial %s." number)
  (voip-send-request "dial" "POST" `((uri . ,number))))

(defun voip-hangall ()
  (interactive)
  (voip-send-request "hangup_all" "POST"))

(defun voip-exit ()
  (interactive)
  (let ((url-request-method "POST")
        url-history-track)
    (url-retrieve (format "http://localhost:%d/exit" voip-http-port)
                  #'ignore)))

(defun voip-calls ()
  (voip-send-request "calls" "GET"))

(defun voip-accounts ()
  (voip-send-request "accounts" "GET"))
  
(provide 'sip)

;; Local Variables:
;; coding: utf-8-unix
;; indent-tabs-mode: nil
;; End:

;;; sip.el ends here

;;; lsml.el --- Compose Rich Text Email with XML -*- lexical-binding: t; -*-

(require 'nxml-mode)

;;;###autoload
(defun lsml-compose ()
  "Compose mail from current lsml file."
  (interactive)
  (let (dom content subject to otherheader)
    (let ((b (current-buffer)))
      (with-temp-buffer
        (insert-buffer-substring b)
        (setq dom (assoc 'head (libxml-parse-xml-region 1 (point-max))))
        (call-process-region
         nil 0 "xsltproc" t
         t nil
         (expand-file-name "~/.emacs.d/mail/lsml.xsl") "-")
        (setq content (buffer-string))))
    (setq subject (cadr (assoc-default 'subject dom 'eq '(nil . nil))))
    (setq to (cadr (assoc-default 'to dom 'eq '(nil . nil))))
    (setq otherheader
          (cons (cons "body"
                      (concat "--[[text/html]]\n" content))
                (mapcar (lambda (x)
                          (cons (symbol-name (car x))
                                (cdr x)))
                        (car (assoc-default 'headers dom)))))
    (compose-mail to subject otherheader)))

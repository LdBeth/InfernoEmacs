;;; lsml.el --- Compose Rich Text Email with XML -*- lexical-binding: t; -*-

(require 'nxml-mode)
(require 'dom)

(defvar lsml-xsl-directory "~/.emacs.d/mail/")

(defun lsml--call-xsl (file)
  (call-process-region
   nil 0 "xsltproc" t
   t nil
   (expand-file-name file lsml-xsl-directory) "-"))

(defun lsml--normalize-mime (pair)
  (let ((cid (car pair))
        (src (cdr pair)))
    (if (string-match "^data:\\([^;]+\\);\\([^,]+\\),\\(.+\\)$" src)
        (progn
          (insert
           (format "--[[%s;\nContent-Disposition: inline\nContent-ID:<%s>][%s]]\n"
                   (match-string 1 src)
                   cid
                   (match-string 2 src)))
          (save-restriction
            (narrow-to-region (1- (point)) (point))
            (insert (match-string 3 src))
            (when mime-auto-hide-body
              (invisible-region (point-min) (point-max)))))
      (mime-edit-insert-tag
       "image" (file-name-extension src)
       (format "\nContent-Disposition: inline\nContent-ID:<%s>"
               cid))
      (mime-edit-insert-binary-file src "base64")))
  nil)

(defun lsml--insert-html (mime content)
  (when mime
    (insert "--<<related>>-{\n"))
  (mime-edit-insert-tag "text" "html" ";charset=utf-8")
  (insert content)
  (when mime
    (dolist (i mime)
      (lsml--normalize-mime i))
    (insert "--}-<<related>>\n"))
  nil)

(defun lsml--compose-body (mime content text)
  (with-temp-buffer
    (if text
        (progn
          (insert "--<<alternative>>-{\n")
          (mime-edit-insert-tag "text" "plain" ";charset=utf-8")
          (insert text)
          (lsml--insert-html mime content)
          (insert "--}-<<alternative>>\n"))
      (lsml--insert-html mime content))
    (buffer-string)))

;;;###autoload
(defun lsml-compose (&optional prefix)
  "Compose mail from current lsml file."
  (interactive "P")
  (let ((b (current-buffer))
        headers mime content text
        subject to)
    (with-temp-buffer
      (insert-buffer-substring b)
      (lsml--call-xsl "lsml-mime.xsl")
      (let ((dom (libxml-parse-xml-region 1 (point-max))))
        (setq headers (car (assoc-default 'headers dom))
              mime (mapcar (lambda (x)
                             (cons (dom-attr x 'cid)
                                   (dom-attr x 'src)))
                           (dom-children (assoc 'mime dom)))))
      (lsml--call-xsl "lsml.xsl")
      (setq content (buffer-string))
      (when prefix
        (shr-render-region (point-min) (point-max))
        (setq text (buffer-substring-no-properties
                    (point-min) (point-max)))))
    (setq subject (assoc-default 'Subject headers)
          to (assoc-default 'To headers)
          headers (assq-delete-all 'To
                                   (assq-delete-all 'Subject headers)))
    (setq headers
          (cons (cons "body"
                      (lsml--compose-body mime content text))
                (mapcar (lambda (x)
                          (cons (symbol-name (car x))
                                (cdr x)))
                        headers)))
    (compose-mail to subject headers)))

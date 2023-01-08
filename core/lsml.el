;;; lsml.el --- Compose Rich Text Email with XML -*- lexical-binding: t; -*-

(require 'nxml-mode)
(require 'dom)
(eval-when-compile
  (require 'wl-draft)
  (require 'mime-edit))

(defvar lsml-xsl-directory "~/.emacs.d/script/")

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
      (let* ((guess (mime-find-file-type src))
	     (type (nth 0 guess))
	     (subtype (nth 1 guess))
	     (parameters (nth 2 guess))
	     (encoding (nth 3 guess))
	     (disposition-type (nth 4 guess))
	     (disposition-params (nth 5 guess)))
        (setq parameters
	      (concat
	       (when (consp parameters)
	         (mime-edit-insert-file-parameters parameters src))
	       (when disposition-type
	         (concat "\n" "Content-Disposition: " disposition-type
		         (mime-edit-insert-file-parameters
		          disposition-params src)))
               (format "\nContent-ID: <%s>" cid)))
        (mime-edit-insert-tag type subtype parameters)
        (mime-edit-insert-binary-file src encoding))))
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
  (if text
      (progn
        (insert "--<<alternative>>-{\n")
        (mime-edit-insert-tag "text" "plain" ";charset=utf-8")
        (insert text)
        (lsml--insert-html mime content)
        (insert "--}-<<alternative>>\n"))
    (lsml--insert-html mime content))
  nil)

(defun lsml--is-valid ()
  (if (and rng-current-schema-file-name
           (string-match-p "lsml.rnc" rng-current-schema-file-name))
      (progn (rng-do-some-validation)
             (or (and rng-error-count (= rng-error-count 0))
                 (error "There are unsolved errors in markup.")))
    (error "Current buffer content is not LsML.")))

;;;###autoload
(defun lsml-compose ()
  "Compose mail from current lsml file."
  (interactive)
  (lsml--is-valid)
  (let ((b (current-buffer))
        headers mime content text
        subject to)
    (with-temp-buffer
      (insert-buffer-substring b)
      (lsml--call-xsl "lsml-mime.xsl")
      (let* ((dom (libxml-parse-xml-region 1 (point-max)))
             (config (dom-children (assoc 'config dom))))
        (setq headers (car (assoc-default 'headers config))
              mime (mapcar (lambda (x)
                             (cons (dom-attr x 'cid)
                                   (dom-attr x 'src)))
                           (dom-children (assoc 'mime config)))
              text (assoc 'usetext config)))
      (lsml--call-xsl "lsml.xsl")
      (setq content (buffer-string))
      (when text
        (shr-render-region (point-min) (point-max))
        (setq text (buffer-substring-no-properties
                    (point-min) (point-max)))))
    (setq subject (assoc-default 'Subject headers)
          to (assoc-default 'To headers)
          headers (assq-delete-all 'To
                                   (assq-delete-all 'Subject headers)))
    (setq headers
          (cons (cons "body"
                       (with-temp-buffer
                         (lsml--compose-body mime content text)
                         (buffer-string)))
                (mapcar (lambda (x)
                          (cons (symbol-name (car x))
                                (cdr x)))
                        headers)))
    (compose-mail to subject headers)))

;;;###autoload
(defun lsml-export-to-draft-buffer ()
  "Export the messaged to opened draft buffer. Usefully when
constructing a reply."
  (interactive)
  (lsml--is-valid)
  (let ((b (current-buffer))
        (draft (car (wl-collect-draft)))
        mime content text)
    (if (null draft)
        (message "No draft buffer exists.")
      (with-temp-buffer
        (insert-buffer-substring b)
        (lsml--call-xsl "lsml-mime.xsl")
        (let* ((dom (libxml-parse-xml-region 1 (point-max)))
               (config (dom-children (assoc 'config dom))))
        (setq mime (mapcar (lambda (x)
                             (cons (dom-attr x 'cid)
                                   (dom-attr x 'src)))
                           (dom-children (assoc 'mime config)))
              text (assoc 'usetext config)))
        (lsml--call-xsl "lsml.xsl")
        (setq content (buffer-string))
        (when text
          (shr-render-region (point-min) (point-max))
          (setq text (buffer-substring-no-properties
                      (point-min) (point-max)))))
      (with-current-buffer draft
        (goto-char (point-max))
        (lsml--compose-body mime content text))
      (let ((buf-win (get-buffer-window draft)))
	    (if buf-win
		(pop-to-buffer draft)
	      (set-buffer (wl-draft-do-reply-buffer-window-styling draft)))))))

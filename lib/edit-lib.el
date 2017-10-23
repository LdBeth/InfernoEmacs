(self-byte-compile)


;;; Insert Line Number
(defvar insert-line-number-separator "|"
  "The fancy separator used for `insert-line-number'.")

(defvar insert-line-number-alias-right t
  "If ture, alias numbers to right.")

(defvar insert-line-number-prefix ""
  "The optional prefix.")

;;;###autoload
(defun insert-line-number (beg end &optional arg)
  "Insert line numbers into buffer."
  (interactive "r")
  (if (called-interactively-p 'any)
      (if (and current-prefix-arg (= current-prefix-arg 0))
          (insert-line-number (point-min) (point-max))
        (save-excursion (insert-line-number (progn
                                              (goto-char beg)
                                              (backward-paragraph)
                                              (unless (= (point) (point-min))
                                                (beginning-of-line 2))
                                              (point))
                                            (progn
                                              (goto-char end)
                                              (forward-paragraph)
                                              (point))
                                            current-prefix-arg)))
    (save-excursion (let* ((max (count-lines beg end))
                           (line (or arg 1))
                           (string (concat insert-line-number-prefix
                                           "%"
                                           (unless insert-line-number-alias-right "-")
                                           (number-to-string
                                            (length (number-to-string (+ (1- line) max))))
                                           "d"
                                           insert-line-number-separator))
                           (counter 1))
                      (goto-char beg)
                      (while (<= counter max)
                        (insert (format string line))
                        (beginning-of-line 2)
                        (incf line)
                        (incf counter))))))

;;;###autoload
(defun strip-regular-expression-string (regular-expression)
  "Strip all string that match REGULAR-EXPRESSION in select area of buffer.
If not select any area, then strip current buffer"
  (interactive "sRegexp:")
  (let (begin
        end)
    (if mark-active
        (setq begin (region-beginning)
              end (region-end))
      (setq begin (point-min)))
    (save-excursion
      (goto-char (or end (point-max)))
      (while (and (> (point) begin)
                  (re-search-backward regular-expression nil t))
        (replace-match "" t t)))))

;;;###autoload
(defun strip-blank-lines()
  "Strip all blank lines in select area of buffer,
if not select any area, then strip all blank lines of buffer."
  (interactive)
  (strip-regular-expression-string "^[ \t]*\n")
  (message "Have strip blanks line. ^_^"))

;;;###autoload
(defun strip-line-number()
  "Strip all line number in select area of buffer,
if not select any area, then strip all line number of buffer."
  (interactive)
  (strip-regular-expression-string (concat (if insert-line-number-alias-right
                                               "^ ?[0-9]+"
                                             "^[0-9]+ ?")
                                           insert-line-number-separator))
  (message "Have strip line number. ^_^"))


;;; Pretty Print

;;;###autoload
(defun xprint (form &optional output-stream)
  "Pretty print FORM to OUTPUT-STREAM use `cl-prettyprint'"
  (princ (with-temp-buffer (cl-prettyprint form) (buffer-string))
         output-stream))


(provide 'edit-lib)

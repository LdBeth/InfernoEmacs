;;; mu-cite.el -*- lexical-binding:t -*-
;; --- yet another citation tool for GNU Emacs
;; Copyright (C) 1995-2001, 2005, 2007, 2012, 2014, 2018, 2019
;;        Free Software Foundation, Inc.

;; Author: MORIOKA Tomohiko <tomo@m17n.org>
;;         Shuhei KOBAYASHI <shuhei@aqua.ocn.ne.jp>
;; Maintainer: Katsumi Yamaoka <yamaoka@jpl.org>
;; Keywords: mail, news, citation

;; This file is part of MU (Message Utilities).

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; - How to use
;;   1. Bytecompile this file and copy it to the apropriate directory.
;;   2. Put the following lines in your ~/.emacs file:
;;      For EMACS 19 or later and XEmacs
;;		(autoload 'mu-cite-original "mu-cite" nil t)
;;		;; for all but message-mode
;;		(add-hook 'mail-citation-hook (function mu-cite-original))
;;		;; for message-mode only
;;		(setq message-cite-function (function mu-cite-original))

;;; Code:

;; For picking up the macros `char-next-index', `with-temp-buffer', etc.
(require 'poem)

(require 'std11)
(require 'alist)

(autoload 'mu-cite-get-prefix-method "mu-register")
(autoload 'mu-cite-get-prefix-register-method "mu-register")
(autoload 'mu-cite-get-prefix-register-verbose-method "mu-register")
(autoload 'mu-cite-get-no-prefix-register-verbose-method "mu-register")

;;; @ version
;;;

(defconst mu-cite-version "8.1")

;;; @ set up
;;;

(defgroup mu-cite nil
  "Yet another citation tool for GNU Emacs."
  :prefix "mu-cite-"
  :group 'mail
  :group 'news)

(defvar mu-cite-default-methods-alist
  (eval-when-compile
    `((from . ,(lambda ()
		         (mu-cite-get-field-value "From")))
	  (date . ,(lambda ()
		         (mu-cite-get-field-value "Date")))
	  (message-id . ,(lambda ()
		               (mu-cite-get-field-value "Message-Id")))
	  (subject . ,(lambda ()
		            (mu-cite-get-field-value "Subject")))
	  (ml-name . ,(lambda ()
		            (mu-cite-get-field-value "X-Ml-Name")))
	  (ml-count . ,#'mu-cite-get-ml-count-method)
	  (address-structure . ,(lambda ()
		                      (car
		                       (std11-parse-address-string
                                (mu-cite-get-value 'from)))))
	  (full-name . ,(lambda ()
		              (std11-full-name-string
		               (mu-cite-get-value 'address-structure))))
      (name-list . ,(lambda ()
		              (let ((namestring (mu-cite-get-value 'full-name)))
                        (split-string namestring "[ \t._]+" t))))
      (firstname . ,(lambda ()
		              (car (mu-cite-get-value 'name-list))))
      (lastname . ,(lambda ()
		             (car (reverse (mu-cite-get-value 'name-list)))))
      (initials . ,(lambda ()
		             (mapconcat
                      (lambda (name)
                        (if (< 0 (length name))
	                        (substring name 0 1)))
                      (mu-cite-get-value 'name-list) "")))
	  (address . ,(lambda ()
		            (std11-address-string
		             (mu-cite-get-value 'address-structure))))
	  (id . ,(lambda ()
		       (let ((ml-name (mu-cite-get-value 'ml-name))
		             (ml-count (mu-cite-get-value 'ml-count)))
		         (if ml-name
		             (concat "["
			                 ml-name
			                 (if ml-count
				                 (concat " : No." ml-count))
			                 "]")
		           (mu-cite-get-value 'message-id)))))
	  (in-id . ,(lambda ()
		          (let ((id (mu-cite-get-value 'id)))
		            (if id
		                (format ">>>>> In %s \n" id)
		              ""))))
	  (x-attribution . ,(lambda ()
		                  (mu-cite-get-field-value "X-Attribution")))
	  (x-cite-me . ,(lambda ()
		              (mu-cite-get-field-value "X-Cite-Me")))
	  (top-posting . ,#'mu-cite-get-original-header)
	  ;; mu-register
	  (prefix . ,#'mu-cite-get-prefix-method)
	  (prefix-register . ,#'mu-cite-get-prefix-register-method)
	  (prefix-register-verbose . ,#'mu-cite-get-prefix-register-verbose-method)
	  (no-prefix-register-verbose . ,#'mu-cite-get-no-prefix-register-verbose-method))))


;;; @ formats
;;;

(defcustom mu-cite-cited-prefix-regexp
  "^[ \t]*[-[:word:]]+>+[ \t]*"
  "Regexp to match the citation prefix.
If match, mu-cite doesn't insert citation prefix."
  :type 'regexp
  :group 'mu-cite)

(defcustom mu-cite-cited-line-regexp
  "^[ \t]*>+[ \t]*"
  "Regexp to match cited line.
If match, mu-cite insert a citation mark only."
  :type 'regexp
  :group 'mu-cite)

(defcustom mu-cite-prefix-format '(prefix-register-verbose "> ")
  "List to represent citation prefix.
Each elements must be a string or a method name."
  :type (list
	 'repeat
	 (list
	  'group
	  :convert-widget
	  (lambda (_widget)
	    (list
	     'choice
	     :tag "Method or String"
	     :args
	     (nconc
	      (mapcar
	       (lambda (elem) (list 'choice-item (car elem)))
	       mu-cite-default-methods-alist)
	      '((symbol :tag "Method")
	        (const :tag "-" nil)
	        (choice-item :tag "String: \"> \"" "> ")
	        (string)))))))
  :set (lambda (symbol value)
	 (set-default symbol (delq nil value)))
  :group 'mu-cite)

(defcustom mu-cite-top-format '(in-id ">>>>>\t" from " wrote:\n")
  "List to represent top string of citation.
Each elements must be a string or a method name."
  :type (list
	 'repeat
	 (list
	  'group
	  :convert-widget
	  (lambda (_widget)
	    (list 'choice
		  :tag "Method or String"
		  :args
		  (nconc
		   (mapcar
		    (lambda (elem) (list 'choice-item (car elem)))
		    mu-cite-default-methods-alist)
		   '((symbol :tag "Method")
		     (const :tag "-" nil)
		     (choice-item :tag "String: \">>>>>\\t\"" ">>>>>\t")
		     (choice-item :tag "String: \" wrote:\\n\"" " wrote:\n")
		     (string :tag "String")))))))
  :set (lambda (symbol value)
	 (set-default symbol (delq nil value)))
  :group 'mu-cite)


;;; @ hooks
;;;

(defcustom mu-cite-instantiation-hook nil
  "List of functions called just before narrowing to the message."
  :type 'hook
  :group 'mu-cite)

(defcustom mu-cite-pre-cite-hook nil
  "List of functions called before citing a region of text."
  :type 'hook
  :group 'mu-cite)

(defcustom mu-cite-post-cite-hook nil
  "List of functions called after citing a region of text."
  :type 'hook
  :group 'mu-cite)


;;; @ field
;;;

(defvar mu-cite-get-field-value-method-alist nil
  "Alist major-mode vs. function to get field-body of header.")

(defun mu-cite-get-field-value (name)
  "Return the value of the header field NAME.
If the field is not found in the header, a method function which is
registered in variable `mu-cite-get-field-value-method-alist' is called."
  (or (std11-field-body name)
      (let ((method (assq major-mode mu-cite-get-field-value-method-alist)))
	(if method
	    (funcall (cdr method) name)))))

;;; @ top-posting
;;;

(defun mu-cite-get-original-header ()
  "Return an original header used with the M$ O*tlook-style top-posting.

You can use the top-posting style with this most simple way:

(setq mu-cite-prefix-format nil)
(setq mu-cite-top-format \\='(top-posting))

But it might not necessarily be convenient as the case may be.  If you
want to use it for only replying to [1]certain recipients, or [2]those
who use the top-posting style, try this:

\(add-hook
 \\='mu-cite-pre-cite-hook
 (lambda ()
   (let ((last-point (point))
         (case-fold-search nil))
     (goto-char (point-min))
     (if (or
          ;; [1]certain recipients (an address contains \"co.jp\")
          (save-excursion
            (save-restriction
              (std11-narrow-to-header)
              (string-match \"\\\\Wco\\\\.jp\\\\(\\\\W\\\\|\\\\'\\\\)\"
                            (or (std11-fetch-field \"from\") \"\"))))
          ;; [2]those who use the top-posting style
          (re-search-forward \"\\n-----Original Message-----\\nFrom:\"
                             nil t)
          ;; [3]the last resort
          (y-or-n-p \"Use top-posting? \"))
         (progn
           (set (make-local-variable \\='mu-cite-prefix-format)
                nil)
           (set (make-local-variable \\='mu-cite-top-format)
                \\='(top-posting))))
     (goto-char last-point))))

This is just an example; modify it to make it suitable to your taste."
  (save-excursion
    (let ((case-fold-search t) cont rest)
      (save-restriction
	(std11-narrow-to-header)
	(dolist (field '("from" "date" "to" "cc" "subject"))
	  (goto-char (point-min))
	  (if (re-search-forward (concat "^" field ":") nil t)
	      (progn
		(setq cont (replace-regexp-in-string
			    "\"\"+" "\""
			    (subst-char-in-string
			     ?' ?\"
			     (replace-regexp-in-string
			      " \\'" ""
			      (replace-regexp-in-string
			       "[\t\n\r ]+" " "
			       (buffer-substring-no-properties
				(match-beginning 0)
				(std11-field-end)))))))
		;; Make it compatible with M$ O*tlook.
		(cond ((string-equal field "date")
		       (let* ((date (substring cont 5 (length cont)))
			      (tz (aref (timezone-parse-date date) 4)))
			 (if tz
			     (setq tz (string-to-number tz)
				   tz (+ (* (/ tz 100) 3600)
					 (* (if (>= tz 0) 60 -60)
					    (% (abs tz) 100))))
			   (setq tz (car (current-time-zone))))
			 (setq cont
			       (concat
				"Sent:"
				(condition-case nil
				    (format-time-string
				     " %a, %d %b %Y %T %z"
				     (date-to-time date)
				     tz)
				  (error date))))))
		      ((member field '("to" "cc"))
		       (setq cont
			     (concat
			      (capitalize field) ": "
			      (mapconcat
			       (lambda (addr)
				 (if (car addr)
				     (concat (car addr) " <" (nth 1 addr) ">")
				   (nth 1 addr)))
			       (mail-extract-address-components
				(substring cont 3 (length cont)) t)
			       "; ")))))
		(setq rest (cons cont rest)))))
	(goto-char (point-max)))
      (if (looking-at "\n\n+")
	  (delete-region (1+ (match-beginning 0)) (match-end 0)))
      (concat "\n-----Original Message-----\n"
	      (mapconcat #'identity (nreverse rest) "\n")
	      "\n\n"))))


;;; @ item methods
;;;

;;; @@ ML count
;;;

(defcustom mu-cite-ml-count-field-list
  '("X-Ml-Count" "X-Mail-Count" "X-Seqno" "X-Sequence" "Mailinglist-Id")
  "List of header fields which contains a sequence number of the mailing list."
  :type '(repeat (choice :tag "Field Name"
			 (choice-item "X-Ml-Count")
			 (choice-item "X-Mail-Count")
			 (choice-item "X-Seqno")
			 (choice-item "X-Sequence")
			 (choice-item "Mailinglist-Id")
			 (const :tag "-" nil)
			 (string :tag "Other")))
  :set (lambda (symbol value)
	 (set-default symbol (delq nil value)))
  :group 'mu-cite)

(defun mu-cite-get-ml-count-method ()
  "A mu-cite method to return a ML-count.
This function searches a field about ML-count, which is specified by
the variable `mu-cite-ml-count-field-list', in a header.
If the field is found, the function returns a number part of the
field.

Notice that please use (mu-cite-get-value \\='ml-count)
instead of to call the function directly."
  (catch 'tag
    (dolist (field mu-cite-ml-count-field-list)
      (let ((ml-count (mu-cite-get-field-value field)))
	(if (and ml-count (string-match "[0-9]+" ml-count))
	    (throw 'tag (match-string 0 ml-count)))))))


;;; @ fundamentals
;;;

(defvar mu-cite-methods-alist nil)

(defun mu-cite-make-methods ()
  (setq mu-cite-methods-alist
	(copy-alist mu-cite-default-methods-alist))
  (run-hooks 'mu-cite-instantiation-hook))

(defun mu-cite-get-value (item)
  "Return a current value of ITEM."
  (let ((ret (cdr (assoc item mu-cite-methods-alist))))
    (if (functionp ret)
	(prog1
	    (setq ret (save-excursion (funcall ret)))
	  (set-alist 'mu-cite-methods-alist item ret))
      ret)))

(defun mu-cite-eval-format (list)
  (mapconcat (lambda (elt)
	       (cond ((stringp elt) elt)
		     ((symbolp elt) (mu-cite-get-value elt))))
	     list ""))


;;; @ main function
;;;

;;;###autoload
(defun mu-cite-original ()
  "Citing filter function.
This is callable from the various mail and news readers' reply
function according to the agreed upon standard."
  (interactive)
  (mu-cite-make-methods)
  (save-restriction
    (if (< (mark t) (point))
	(exchange-point-and-mark))
    (narrow-to-region (point)(point-max))
    (run-hooks 'mu-cite-pre-cite-hook)
    (let ((last-point (point))
	  ;; Register a name before generating the top cite form.
	  (prefix (mu-cite-eval-format mu-cite-prefix-format))
	  (top (mu-cite-eval-format mu-cite-top-format)))
      (if (re-search-forward "^-*$" nil nil)
	  (forward-line 1))
      (widen)
      (delete-region last-point (point))
      (insert top)
      (setq last-point (point))
      (while (< (point)(mark t))
	(cond ((looking-at "^>>>>>\\|^[ \t]*$"))
              ((looking-at mu-cite-cited-prefix-regexp)
               (let ((beg (point)))
                 (skip-chars-forward " \t")
                 (delete-region beg (point))))
              ((looking-at mu-cite-cited-line-regexp)
               (let ((beg (point)))
                 (skip-chars-forward " \t")
                 (delete-region beg (point))
                 (insert ">")))
	      (t (insert prefix)))
	(forward-line 1))
      (goto-char last-point))
    (run-hooks 'mu-cite-post-cite-hook)))


;;; @ message editing utilities
;;;

(defcustom citation-mark-chars ">}|"
  "String of characters for citation delimiter."
  :type 'string
  :group 'mu-cite)

(defcustom citation-disable-chars "<{"
  "String of characters not allowed as citation-prefix."
  :type 'string
  :group 'mu-cite)

(defun mu-cite-char-category (character)
  "Return a string of category mnemonics for CHARACTER in TABLE.
CHARACTER can be any multilingual characters,
TABLE defaults to the current buffer's category table (it is currently
ignored)."
  (category-set-mnemonics (char-category-set character)))

(defun detect-paragraph-cited-prefix ()
  (save-excursion
    (goto-char (point-min))
    (let ((i 0)
	  (prefix
	   (buffer-substring (line-beginning-position)
			     (line-end-position))))
      (let ((init prefix)
	    str ret)
	(while (and (= (forward-line) 0)
		    (setq str (buffer-substring
			       (progn (beginning-of-line)(point))
			       (progn (end-of-line)(point))))
		    (setq ret (string-compare-from-top prefix str)))
	  (setq prefix
		(if (stringp ret)
		    ret
		  (car (cdr ret))))
	  (or (string-equal init prefix)
	      (setq i (1+ i)))))
      (cond ((> i 1) prefix)
	    ((> i 0)
	     (goto-char (point-min))
	     (save-restriction
	       (narrow-to-region (point)
				 (+ (point)(length prefix)))
	       (goto-char (point-max))
	       (if (re-search-backward
		    (concat "[" citation-mark-chars "]") nil t)
		   (progn
		     (goto-char (match-end 0))
		     (if (looking-at "[ \t]+")
			 (goto-char (match-end 0)))
		     (buffer-substring (point-min)(point)))
		 prefix)))
	    ((progn
	       (goto-char (point-max))
	       (re-search-backward
		(concat "[" citation-disable-chars "]") nil t)
	       (re-search-backward
		(concat "[" citation-mark-chars "]") nil t))
	     (goto-char (match-end 0))
	     (if (looking-at "[ \t]+")
		 (goto-char (match-end 0)))
	     (buffer-substring (line-beginning-position)(point)))
	    (t "")))))

(defcustom fill-column-for-fill-cited-region nil
  "Integer to override `fill-column' while `fill-cited-region' is being
executed.  If you wish people call you ****-san, you may set the value
of `fill-column' to 60 in the buffer for message sending and set this
to 70. :-)"
  :type `(choice (const :tag "Off" nil)
		 (integer ,(default-value 'fill-column)))
  :group 'mu-cite)

;;;###autoload
(defun fill-cited-region (beg end)
  "Fill each of the paragraphs in the region as a cited text."
  (interactive "*r")
  (save-excursion
    (save-restriction
      (goto-char end)
      (and (search-backward "\n" nil t)
	   (setq end (match-end 0)))
      (narrow-to-region beg end)
      (let* ((fill-prefix (detect-paragraph-cited-prefix))
	     (fill-column (max (+ 1 (current-left-margin)
				  (string-width fill-prefix))
			       (or fill-column-for-fill-cited-region
				   (current-fill-column))))
	     (pat (concat fill-prefix "\n")))
	(goto-char (point-min))
	(while (search-forward pat nil t)
	  (let ((b (match-beginning 0))
		(e (match-end 0)))
	    (delete-region b e)
	    (if (and (> b (point-min))
		     (let ((cat (mu-cite-char-category (char-before b))))
		       (or (string-match "a" cat)
			   (string-match "l" cat))))
		(insert " "))))
	(goto-char (point-min))
	(fill-region (point-min) (point-max))))))

;;;###autoload
(defun compress-cited-prefix ()
  "Compress nested cited prefixes."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (re-search-forward
     (concat "^" (regexp-quote mail-header-separator) "$") nil t)
    (while (re-search-forward
	        (concat "^\\([ \t]*[-[:word:]]*["
		            citation-mark-chars "]\\)+")
            nil t)
      (let* ((b (match-beginning 0))
	     (e (match-end 0))
	     (prefix (buffer-substring b e))
	     ps pe (s 0)
	     (nest (let ((i 0))
		     (if (string-match "<[^<>]+>" prefix)
			 (setq prefix
			       (substring prefix 0 (match-beginning 0))))
		     (while (string-match
			     (concat "\\([" citation-mark-chars "]+\\)[ \t]*")
			     prefix s)
		       (setq i (+ i (- (match-end 1)(match-beginning 1)))
			     ps s
			     pe (match-beginning 1)
			     s (match-end 0)))
		     i)))
	(if (and ps (< ps pe))
	    (progn
	      (delete-region b e)
	      (insert (concat (substring prefix ps pe)
			      (make-string nest ?>)))))
	))))

(defun replace-top-string (old new)
  (interactive "*sOld string: \nsNew string: ")
  (while (re-search-forward
	  (concat "^" (regexp-quote old)) nil t)
    (replace-match new)))

(defun string-compare-from-top (str1 str2)
  (let* ((len1 (length str1))
	 (len2 (length str2))
	 (len (min len1 len2))
	 (p 0)
	 c1 c2)
    (while (and (< p len)
		(progn
		  (setq c1 (aref str1 p)
			c2 (aref str2 p))
		  (eq c1 c2)))
      (setq p (char-next-index c1 p)))
    (and (> p 0)
	 (let ((matched (substring str1 0 p))
	       (r1 (and (< p len1)(substring str1 p)))
	       (r2 (and (< p len2)(substring str2 p))))
	   (if (eq r1 r2)
	       matched
	     (list 'seq matched (list 'or r1 r2)))))))


;;; @ end
;;;

(provide 'mu-cite)
;;; mu-cite.el ends here

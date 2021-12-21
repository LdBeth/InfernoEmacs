;;; mu-register.el --- registration feature of mu-cite
;; Copyright (C) 1995-2001, 2014
;;        Free Software Foundation, Inc.

;; Author: MINOURA Makoto <minoura@netlaputa.or.jp>
;;         MORIOKA Tomohiko <tomo@m17n.org>
;;; Created: 1995-12-27 by MINOURA Makoto
;; Maintainer: Katsumi Yamaoka <yamaoka@jpl.org>
;; Keywords: registration, citation, mail, news

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

;;; Code:

(require 'mu-cite)

(eval-when-compile (require 'static))


;;; @ variables
;;;

(defcustom mu-registration-file (expand-file-name "~/.mu-cite.el")
  "The name of the user environment file for mu-cite."
  :type 'file
  :group 'mu-cite)

(defcustom mu-registration-file-modes 384
  "Mode bits of `mu-registration-file', as an integer."
  :type 'integer
  :group 'mu-cite)

(defcustom mu-registration-file-coding-system-for-write
  (static-if (boundp 'MULE)
      '*iso-2022-jp*
    'iso-2022-7bit)
  "Coding-system used when writing a registration file.  If you set this
to nil, the value of `mu-registration-file-coding-system' will be used
for writing a file."
  :group 'mu-cite)

(defcustom mu-cite-allow-null-string-registration nil
  "If non-nil, null-string citation-name can be registered."
  :type 'boolean
  :group 'mu-cite)

(defvar mu-registration-symbol 'mu-citation-name-alist
  "*Name of the variable to register citation prefix strings.")

(defvar mu-registration-file-coding-system-for-read nil
  "*Coding-system used when reading a registration file.  Normally, you
have no need to set this option.  If you have many friends in various
countries and the file contains their names in various languages, you
may avoid mis-decoding them by setting this option to `iso-2022-7bit'
or the other universal coding-system.  Note that when you change this
value, you should save the file manually using the same coding-system
in advance.")

(defvar mu-registration-file-coding-system nil
  "Internal variable used to keep a default coding-system for writing
a current registration file.  The value will be renewed whenever a
registration file is read.")

(defvar mu-register-history nil)


;;; @ load / save registration file
;;;

(defun mu-cite-load-registration-file ()
  (if (file-readable-p mu-registration-file)
      (with-temp-buffer
	(set-buffer-multibyte t)
	(if mu-registration-file-coding-system-for-read
	    (insert-file-contents-as-coding-system
	     mu-registration-file-coding-system-for-read
	     mu-registration-file)
	  (insert-file-contents mu-registration-file))
	(setq mu-registration-file-coding-system
	      (static-cond
	       ((boundp 'buffer-file-coding-system)
		(symbol-value 'buffer-file-coding-system))
	       ((boundp 'file-coding-system)
		(symbol-value 'file-coding-system))
	       (t
		nil)))
	(let ((exp (read (current-buffer))))
	  (or (eq (car (cdr exp)) mu-registration-symbol)
	      (setcar (cdr exp) mu-registration-symbol))
	  (eval exp))))
  (or (boundp mu-registration-symbol)
      (set mu-registration-symbol nil)))

(defun mu-cite-save-registration-file ()
  (with-temp-buffer
    (set-buffer-multibyte t)
    (let ((name (file-name-nondirectory mu-registration-file))
	  (coding-system (or mu-registration-file-coding-system-for-write
			     mu-registration-file-coding-system)))
      (insert (format "\
;;; %s  -*- mode: emacs-lisp; coding: %s -*-
;; This file is generated automatically by MU-CITE v%s.

"
		      name coding-system mu-cite-version))
      (insert "(setq "
	      (symbol-name mu-registration-symbol)
	      "\n      '(")
      (insert (mapconcat
	       (function
		(lambda (elem)
		  (format "(%s . %s)"
			  (prin1-to-string
			   (mu-cite-remove-text-properties (car elem)))
			  (prin1-to-string
			   (mu-cite-remove-text-properties (cdr elem))))))
	       (symbol-value mu-registration-symbol) "\n\t"))
      (insert "))\n\n")
      (insert ";;; " name " ends here\n")
      (write-region-as-coding-system coding-system
				     (point-min) (point-max)
				     mu-registration-file nil 'nomsg)
      (condition-case nil
	  (set-file-modes mu-registration-file mu-registration-file-modes)
	(error nil)))))


;;; @ database accessors
;;;

;; get citation-name from the database
(defun mu-register-get-citation-name (from)
  (cdr (assoc from (symbol-value mu-registration-symbol))))

;; register citation-name to the database
(defun mu-register-add-citation-name (name from)
  (set-alist mu-registration-symbol from name)
  (mu-cite-save-registration-file))


;;; @ methods
;;;

;;;###autoload
(defun mu-cite-get-prefix-method ()
  (or (mu-register-get-citation-name (mu-cite-get-value 'address))
      ">"))

;;;###autoload
(defun mu-cite-get-prefix-register-method ()
  (let ((addr (mu-cite-get-value 'address)))
    (or (mu-register-get-citation-name addr)
	(let* ((minibuffer-allow-text-properties nil)
	       (return
		(mu-cite-remove-text-properties
		 (read-string "Citation name? "
			      (or (mu-cite-get-value 'x-attribution)
				  (mu-cite-get-value 'x-cite-me)
				  (mu-cite-get-value 'full-name))
			      'mu-register-history))))

	  (if (and (or mu-cite-allow-null-string-registration
		       (not (string-equal return "")))
		   (y-or-n-p (format "Register \"%s\"? " return)))
	      (mu-register-add-citation-name return addr))
	  return))))

;;;###autoload
(defun mu-cite-get-prefix-register-verbose-method (&optional no-return)
  (let* ((addr (mu-cite-get-value 'address))
	 (return1 (mu-register-get-citation-name addr))
	 (minibuffer-allow-text-properties nil)
	 (return (mu-cite-remove-text-properties
		  (read-string "Citation name? "
			       (or return1
				   (mu-cite-get-value 'x-attribution)
				   (mu-cite-get-value 'x-cite-me)
				   (mu-cite-get-value 'full-name))
			       'mu-register-history))))
    (if (and (or mu-cite-allow-null-string-registration
		 (not (string-equal return "")))
	     (not (string-equal return return1))
	     (y-or-n-p (format "Register \"%s\"? " return)))
	(mu-register-add-citation-name return addr))
    (if no-return "" return)))

;;;###autoload
(defun mu-cite-get-no-prefix-register-verbose-method ()
  ;; Allow registration of a name but don't use it as a citation prefix.
  (mu-cite-get-prefix-register-verbose-method t))


;;; @ end
;;;

(provide 'mu-register)

(mu-cite-load-registration-file)

;;; mu-register.el ends here

;;; -*- mode: emacs-lisp; lexical-binding: t -*-

;;; rpl-base.el -- basic setup for the RPL tools

;; Copyright (C) 2014 Paul Onions

;; Author: Paul Onions <paul.onions@acm.org>
;; Keywords: RPL, SysRPL, HP48, HP49, HP50

;; This file is free software, see the LICENCE file in this directory
;; for copying terms.

;;; Commentary:

;; Basic setup for the RPL tools.

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Customizations
;;
(defgroup rpl nil
  "Tools for working with the RPL calculator programming language."
  :group 'languages)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utility functions for generating/loading pre-computed data
;;
(defvar rpl-tools-data-dir
  (and load-file-name (concat (file-name-directory load-file-name) "data/"))
  "RPL tools data directory.")

(defun rpl-write-data-file (obj filename)
  "Write OBJ to FILENAME using function `print'.

The directory in which to write the file defaults to the value of
the variable `rpl-tools-data-dir'. This can be overridden by
specifying a different path in the FILENAME string (either
relative or absolute)."
  (let ((default-directory rpl-tools-data-dir))
    (with-temp-buffer
      (print obj (current-buffer))
      (write-region (point-min) (point-max) filename))))

(defun rpl-read-data-file (filename)
  "Read a Lisp object from FILENAME using function `read'.

The directory in which FILENAME resides is assumed to be the
value of the variable `rpl-tools-data-dir'. This can be
overridden by specifying a different path in the FILENAME
string (either relative or absolute)."
  (let ((default-directory rpl-tools-data-dir))
    (with-temp-buffer
      (insert-file-contents filename)
      (goto-char (point-min))
      (read (current-buffer)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Common keymap (including the ``RPL'' menu)
;;
(defvar rpl-menu-compile-buffer-enable nil)

(make-variable-buffer-local 'rpl-menu-compile-buffer-enable)

(defvar rpl-common-keymap
  (let ((map (make-sparse-keymap "RPL"))
        (menu-map (make-sparse-keymap "RPL")))
    (set-keymap-parent map prog-mode-map)
    ;; Key assignments
    (define-key map (kbd "C-c C-k") 'rpl-compile-buffer)
    (define-key map (kbd "C-c C-a") 'rpl-apropos-thing-at-point)
    ;; Menu items
    (define-key map [menu-bar rpl-menu] (cons "RPL" menu-map))
    (define-key menu-map [rpl-menu-compile-buffer]
      '(menu-item "Compile Buffer" rpl-compile-buffer
                  :enable rpl-menu-compile-buffer-enable))
    (define-key menu-map [rpl-menu-separator-1]
      '(menu-item "--"))
    (define-key menu-map [rpl-menu-apropos]
      '(menu-item "Apropos..." rpl-apropos-thing-at-point))
    map)
  "The RPL tools common keymap.")

(defun rpl-apropos-thing-at-point ()
  "Call the appropriate apropos command, depending on major mode."
  (interactive)
  (cond ((eql major-mode 'sysrpl-mode)
         (call-interactively 'sysrpl-apropos-thing-at-point))))

(defun rpl-compile-buffer ()
  "Call the appropriate compile-buffer command, depending on major mode."
  (interactive)
  (cond ((eql major-mode 'sysrpl-mode)
         (call-interactively 'sysrpl-compile-buffer))
        ((eql major-mode 'sasm-mode)
         (call-interactively 'sasm-compile-buffer))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Developer utils
;;
(defvar rpl-debug nil)

(defmacro rpl-debug-message (msg)
  (if rpl-debug
      `(message ,msg)
    nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; End of file
;;
(provide 'rpl-base)

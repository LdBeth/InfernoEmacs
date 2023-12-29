;;; -*- mode: emacs-lisp; lexical-binding: t -*-

;;; sasm-mode.el -- Major mode for Saturn assembly language

;; Copyright (C) 2015 Paul Onions

;; Author: Paul Onions <paul.onions@acm.org>
;; Keywords: Saturn, HP48, HP49, HP50, calculator

;; This file is free software, see the LICENCE file in this directory
;; for copying terms.

;;; Commentary:

;; A major mode for Saturn assembly language, the processor (perhaps
;; emulated) in HP48/49/50-series calculators.

;;; Code:
(require 'rpl-base)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Customizations
;;
(defcustom sasm-assembler-program "sasm"
  "External SASM assembler program name."
  :type 'string
  :group 'rpl)

(defcustom sasm-assembler-output-bufname "*sasm*"
  "Buffer name in which to capture SASM assembler output."
  :type 'string
  :group 'rpl)

(defface sasm-label '((t :inherit font-lock-constant-face))
  "Face used for displaying SASM labels."
  :group 'rpl)

(defface sasm-mnemonic '((t :inherit font-lock-keyword-face))
  "Face used for displaying SASM mnemonics."
  :group 'rpl)

(defface sasm-comment '((t :inherit font-lock-comment-face))
  "Face used for displaying SASM comments."
  :group 'rpl)

(defcustom sasm-font-lock-label-face 'sasm-label
  "Name of face to use for displaying SASM labels."
  :type 'symbol
  :group 'rpl)

(defcustom sasm-font-lock-keyword-face 'sasm-mnemonic
  "Name of face to use for displaying SASM mnemonics."
  :type 'symbol
  :group 'rpl)

(defcustom sasm-font-lock-comment-face 'sasm-comment
  "Name of face to use for displaying SASM comments."
  :type 'symbol
  :group 'rpl)

(defvar sasm-mode-syntax-table
  (let ((table (make-syntax-table prog-mode-syntax-table)))
    (modify-syntax-entry ??  "w" table)
    (modify-syntax-entry ?#  "w" table)
    (modify-syntax-entry ?=  "w" table)
    (modify-syntax-entry ?<  "w" table)
    (modify-syntax-entry ?>  "w" table)
    (modify-syntax-entry ?+  "w" table)
    (modify-syntax-entry ?-  "w" table)
    (modify-syntax-entry ?!  "w" table)
    (modify-syntax-entry ?&  "w" table)
    (modify-syntax-entry ?\( "()" table)
    (modify-syntax-entry ?\) "()" table)
    table)
  "The SASM syntax table.")

(defalias 'sasm-mode-syntax-propertize
  (syntax-propertize-rules
   ("^*" (0 "<"))
   ("\\((\\)\s" (1 "<1b"))
   ("\\({\\)\s" (1 "(}"))
   ("\s\\(}\\)" (1 "){"))))

(defvar sasm-keywords '("BSS" "CON" "REL" "NIBASC" "STRING" "NIBHEX"
                        "NIBFS" "LINK" "SLINK" "INC" "ASC" "ASCM"
                        "HEX" "HEXM" "NIBBIN" "NIBGRB"
                        "TITLE" "STITLE" "EJECT" "UNLIST"
                        "LIST" "LISTM" "LISTALL" "CLRLIST" "SETLIST")
  "Keywords used by the SASM assembler.")

(defvar sasm-font-lock-keywords
  `(("^\\*.*$" (0 sasm-font-lock-comment-face))
    (,(concat "\\<" (regexp-opt sasm-keywords) "\\>")
     (0 sasm-font-lock-keyword-face))))

(defun sasm-compile-buffer ()
  "Assemble the current buffer."
  (interactive)
  (let ((tmp-filename (make-temp-file "sasm" nil ".a"))
        (rtn-code 0))
    (write-region (point-min) (point-max) tmp-filename)
    (with-current-buffer (get-buffer-create sasm-assembler-output-bufname)
      (setq buffer-read-only nil)
      (erase-buffer)
      (setq rtn-code (call-process sasm-assembler-program tmp-filename t nil)))
    (display-buffer sasm-assembler-output-bufname)
    (if (eql rtn-code 0)
        (message "Assembly complete")
      (message "*** Assembled with ERRORS ***"))))

(defun sasm-get-eldoc-message ()
  (interactive)
  ;; !!! TODO !!!
  "")

(defvar sasm-mode-map
  (let ((map (make-sparse-keymap))
        (menu-map (make-sparse-keymap)))
    (set-keymap-parent map rpl-common-keymap)
    ;; Menu items
    (define-key map [menu-bar rpl-menu] (cons "RPL" menu-map))
    (define-key menu-map [sasm-menu-separator-1]
      '(menu-item "--"))
    ;; !!! TODO !!!
    map)
  "The SASM mode local keymap.")

(defvar sasm-mode-hook nil
  "Hook for customizing SASM mode.")

;;;###autoload
(define-derived-mode sasm-mode prog-mode "SASM"
  "Major mode for SASM assembler language."
  :group 'rpl
  :syntax-table sasm-mode-syntax-table
  (setq-local eldoc-documentation-function #'sasm-get-eldoc-message
              syntax-propertize-function #'sasm-mode-syntax-propertize)
  (setq font-lock-defaults (list 'sasm-font-lock-keywords))
  (setq rpl-menu-compile-buffer-enable t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; End of file
;;
(provide 'sasm-mode)

;;; -*- mode: emacs-lisp; lexical-binding: t -*-

;;; sysrpl-mode.el -- Major mode for the SysRPL programming language

;; Copyright (C) 2014 Paul Onions

;; Author: Paul Onions <paul.onions@acm.org>
;; Keywords: RPL, SysRPL, HP48, HP49, HP50, calculator

;; This file is free software, see the LICENCE file in this directory
;; for copying terms.

;;; Commentary:

;; A major mode for the SysRPL language, the system programming
;; language of HP48/49/50-series calculators.

;;; Code:
(require 'rpl-base)
(require 'rpl-edb)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Customizations
;;
(defcustom sysrpl-default-calculator :HP50G
  "Default calculator type for SysRPL mode."
  :type '(radio :HP48G :HP49G :HP50G)
  :group 'rpl)

(defcustom sysrpl-compiler-program "rplcomp"
  "External SysRPL compiler program name."
  :type 'string
  :group 'rpl)

(defcustom sysrpl-compiler-output-bufname "*rplcomp*"
  "Buffer name in which to capture SysRPL compiler output."
  :type 'string
  :group 'rpl)

(defcustom sysrpl-indent-offset 1
  "Amount of offset per level of indentation."
  :type 'natnum
  :group 'rpl)

(defface sysrpl-name '((t :inherit font-lock-builtin-face))
  "Face used for displaying SysRPL names (e.g DROP)."
  :group 'rpl)

(defface sysrpl-keyword '((t :inherit font-lock-keyword-face))
  "Face used for displaying SysRPL keywords (e.g. :: ;)."
  :group 'rpl)

(defcustom sysrpl-font-lock-name-face 'sysrpl-name
  "Name of face to use for displaying SysRPL names."
  :type 'symbol
  :group 'rpl)

(defcustom sysrpl-font-lock-keyword-face 'sysrpl-keyword
  "Name of face to use for displaying SysRPL keywords."
  :type 'symbol
  :group 'rpl)

(defun sysrpl-edb-calculator (calculator)
  "Map SysRPL calculator identifier to EDB identifier."
  (cond ((eql calculator :HP48G) :48G)
        ((eql calculator :HP49G) :49G)
        ((eql calculator :HP50G) :49G)))

(defvar sysrpl-mode-syntax-table
  (let ((table (make-syntax-table prog-mode-syntax-table)))
    (modify-syntax-entry ?\) ">4b" table)
    (modify-syntax-entry ?{  "(}" table)
    (modify-syntax-entry ?}  "){" table)
    (modify-syntax-entry ?:  "w" table)
    (modify-syntax-entry ?\; "w" table)
    (modify-syntax-entry ?!  "w" table)
    (modify-syntax-entry ?@  "w" table)
    (modify-syntax-entry ?#  "w" table)
    (modify-syntax-entry ?$  "w" table)
    (modify-syntax-entry ?%  "w" table)
    (modify-syntax-entry ?^  "w" table)
    (modify-syntax-entry ?&  "w" table)
    (modify-syntax-entry ?\? "w" table)
    (modify-syntax-entry ?-  "w" table)
    (modify-syntax-entry ?_  "w" table)
    (modify-syntax-entry ?=  "w" table)
    (modify-syntax-entry ?+  "w" table)
    (modify-syntax-entry ?*  "w" table)
    (modify-syntax-entry ?/  "w" table)
    (modify-syntax-entry ?<  "w" table)
    (modify-syntax-entry ?>  "w" table)
    (modify-syntax-entry ?|  "w" table)
    (modify-syntax-entry ?\n ">" table)
    table)
  "The SysRPL syntax table.")

(defalias 'sysrpl-mode-syntax-propertize
  (syntax-propertize-rules
   ("^*" (0 "<"))
   ("\\((\\)\s" (1 "<1b"))))

(defvar sysrpl-rplcomp-keywords '("LAM" "ID" "TAG" "CHR" "CODE" "CODEM" "ENDCODE" "PTR"
                                  "ROMPTR" "FLASHPTR" "ZINT" "ARRY" "LNKARRY" "HXS" "GROB"
                                  "::" ";" "BEGIN" "AGAIN" "UNTIL" "WHILE" "REPEAT" "DO"
                                  "LOOP" "+LOOP" "IF" "ELSE" "THEN" "FCN" "ENDFCN" "{" "}"
                                  "ASSEMBLE" "RPL" "ASSEMBLEM" "!RPL" "ROMID" "xROMID"
                                  "NAME" "NULLNAME" "xNAME" "sNAME" "tNAME"
                                  "NAMELESS" "LABEL" "LOCALNAME" "LOCALLABEL"
                                  "EXTERNAL" "LOCAL" "FEXTERNAL" "DEFINE" "INCLUDE"
                                  "TITLE" "STITLE" "EJECT")
  "Keywords used by the RPLCOMP SysRPL compiler.")

(defun sysrpl-font-lock-compile-keywords (names)
  "Construct a list of keyword matcher clauses suitable for `font-lock-keywords'."
  `((,(concat "\\<" (regexp-opt sysrpl-rplcomp-keywords) "\\>")
     (0 sysrpl-font-lock-keyword-face))
    ,@(mapcar (lambda (str) (list (concat "\\<" (regexp-quote str) "\\>")
                                  (list 0 'sysrpl-font-lock-name-face)))
              names)))

(defvar sysrpl-font-lock-keywords
  (sysrpl-font-lock-compile-keywords (rpl-edb-all-names (sysrpl-edb-calculator sysrpl-default-calculator))))

(defvar sysrpl-selected-calculator sysrpl-default-calculator
  "Currently selected calculator model.")

(defun sysrpl-select (model)
  "Set the currently selected calculator model to be the HP50G."
  (interactive (list (intern
                      (concat ":" (completing-read "Model: "
                                                   '("HP48G" "HP49G" "HP50G"))))))
  (setq sysrpl-selected-calculator model)
  (setq sysrpl-font-lock-keywords
        (sysrpl-font-lock-compile-keywords (rpl-edb-all-names (sysrpl-edb-calculator model))))
  (sysrpl-mode))

(defun sysrpl-get-eldoc-message ()
  (interactive)
  (rpl-edb-get-stack-effect (sysrpl-edb-calculator sysrpl-selected-calculator)
                            (thing-at-point 'word)))

(defun sysrpl-apropos-thing-at-point (name)
  "Show information about NAME in a popup buffer.
When called interactively NAME defaults to the word around
point."
  (interactive (list (completing-read "Apropos: " (rpl-edb-all-names (sysrpl-edb-calculator sysrpl-selected-calculator))
                                      nil nil (thing-at-point 'word))))
  (let ((bufname (format "*SysRPL: %s*" name)))
    (with-current-buffer (get-buffer-create bufname)
      (setq buffer-read-only nil)
      (erase-buffer)
      (insert (rpl-edb-get-stack-effect (sysrpl-edb-calculator sysrpl-selected-calculator) name))
      (newline)
      (insert (rpl-edb-get-description (sysrpl-edb-calculator sysrpl-selected-calculator) name))
      (newline)
      (insert (format "Address: %s" (rpl-edb-get-address (sysrpl-edb-calculator sysrpl-selected-calculator) name)))
      (newline)
      (insert (format "Flags: %s" (rpl-edb-get-flags (sysrpl-edb-calculator sysrpl-selected-calculator) name)))
      (newline)
      (goto-char (point-max))
      (help-mode)
      (set-buffer-modified-p nil)
      (setq buffer-read-only t))
    (fit-window-to-buffer (display-buffer bufname))))

(defun sysrpl-compile-buffer ()
  "Compile the current buffer."
  (interactive)
  (let ((tmp-filename (make-temp-file "sysrpl" nil ".s"))
        (rtn-code 0))
    (write-region (point-min) (point-max) tmp-filename)
    (with-current-buffer (get-buffer-create sysrpl-compiler-output-bufname)
      (setq buffer-read-only nil)
      (erase-buffer)
      (setq rtn-code (call-process sysrpl-compiler-program tmp-filename t nil "-" "-")))
    (display-buffer sysrpl-compiler-output-bufname)
    (if (eql rtn-code 0)
        (message "Compilation complete")
      (message "*** Compiled with ERRORS ***"))))

(defvar sysrpl-mode-map
  (let ((map (make-sparse-keymap))
        (menu-map (make-sparse-keymap)))
    (set-keymap-parent map rpl-common-keymap)
    ;; Menu items
    (define-key map [menu-bar rpl-menu] (cons "RPL" menu-map))
    map)
  "The SysRPL mode local keymap.")

(defvar sysrpl-mode-hook nil
  "Hook for customizing SysRPL mode.")

;;;###autoload
(define-derived-mode sysrpl-mode prog-mode "SysRPL"
  "Major mode for the SysRPL language."
  :group 'rpl
  (setq-local eldoc-documentation-function 'sysrpl-get-eldoc-message)
  (setq font-lock-defaults (list 'sysrpl-font-lock-keywords))
  (setq-local comment-start "( "
              comment-end " )"
              comment-start-skip "\\(^*+\\|(\s\\)\s*"
              comment-end-skip "[\t\s]*\\(\\s>\\|\n\\|)\\)"
              syntax-propertize-function #'sysrpl-mode-syntax-propertize)
  (setq rpl-menu-compile-buffer-enable t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; End of file
;;
(provide 'sysrpl-mode)

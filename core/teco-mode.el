;;; teco-mode.el --- Emacs mode for TECO macro -*- lexical-binding: t; -*-

(defconst teco-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?! "! 12b" table)
    (modify-syntax-entry ?/ "|" table)

    (modify-syntax-entry ?\" "." table)
    (modify-syntax-entry ?*  "." table)
    (modify-syntax-entry ?&  "."  table)
    (modify-syntax-entry ?+  "."  table)
    (modify-syntax-entry ?-  "."  table)
    (modify-syntax-entry ?*  "."  table)
    (modify-syntax-entry ?.  "."  table)
    (modify-syntax-entry ?:  "."  table)
    (modify-syntax-entry ?=  "."  table)
    (modify-syntax-entry ?<  "."  table)
    (modify-syntax-entry ?>  "."  table)
    (modify-syntax-entry ?|  "."  table)
    (modify-syntax-entry ?\n "> b" table)
    table))

(defvar teco-font-lock-keywords
  '(("E[ABCFIKLNPQRWX]" (0 font-lock-builtin-face))
    ("\\^[@A-Za-z[]" (0 font-lock-constant-face))))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.te[sc]\\'"  . teco-mode))

(define-derived-mode teco-mode prog-mode "TECO"
  :syntax-table teco-mode-syntax-table
  (setq font-lock-defaults (list 'teco-font-lock-keywords))
  (setq-local comment-start "! "
              comment-end " !"))

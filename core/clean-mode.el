;;; clean-mode.el --- Emacs mode for Clean -*- lexical-binding: t; -*-

(require 'font-lock)

(defconst clean-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; ' is a string delimiter
    ;; (modify-syntax-entry ?' "\"" table)
    ;; " is a string delimiter too
    (modify-syntax-entry ?\" "\"" table)

    (modify-syntax-entry ?\[ "(]" table)
    (modify-syntax-entry ?\] ")[" table)
    (modify-syntax-entry ?* ". 23" table)
    (modify-syntax-entry ?&  "."  table)
    (modify-syntax-entry ?+  "."  table)
    (modify-syntax-entry ?-  "."  table)
    (modify-syntax-entry ?/  "."  table)
    (modify-syntax-entry ?*  "."  table)
    (modify-syntax-entry ?.  "."  table)
    (modify-syntax-entry ?:  "."  table)
    (modify-syntax-entry ?=  "."  table)
    (modify-syntax-entry ?<  "."  table)
    (modify-syntax-entry ?>  "."  table)
    (modify-syntax-entry ?|  "."  table)
    (modify-syntax-entry ?\\ "\\" table)
    ;; / is punctuation, but // is a comment tablearter
    (modify-syntax-entry ?/ ". 124b" table)
    ;; \n is a comment ender
    (modify-syntax-entry ?\n "> b" table)
    table))

;;;###autoload
(unless (assoc "\\.[id]cl\\'" auto-mode-alist)
  (add-to-list 'auto-mode-alist '("\\.[id]cl\\'" . clean-mode)))

(define-derived-mode clean-mode prog-mode "Clean"
  :syntax-table clean-mode-syntax-table
  (font-lock-fontify-buffer))

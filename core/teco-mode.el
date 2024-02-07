;;; teco-mode.el --- Emacs mode for TECO macro -*- lexical-binding: t; -*-
;; This teco mode mainly targets TECO-64 which has slightly different
;; implementation on how to read in commands compared to traditional
;; tecoes.
(defconst teco-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?! "! 12b" table)
    (modify-syntax-entry ?/ "|" table)

    (modify-syntax-entry ?\" "." table)
    (modify-syntax-entry ?\' "." table)
    (modify-syntax-entry ?\[ "." table) ; Q-reg push
    (modify-syntax-entry ?\] "." table) ; Q-reg pop
    (modify-syntax-entry ?\{ "." table) ; not teco command
    (modify-syntax-entry ?\} "." table)
    (modify-syntax-entry ?*  "." table)
    (modify-syntax-entry ?&  "."  table)
    (modify-syntax-entry ?#  "."  table)
    (modify-syntax-entry ?+  "."  table)
    (modify-syntax-entry ?-  "."  table)
    (modify-syntax-entry ?*  "."  table)
    (modify-syntax-entry ?.  "."  table)
    (modify-syntax-entry ?:  "."  table)
    (modify-syntax-entry ?@  "."  table)
    (modify-syntax-entry ?=  "."  table)
    (modify-syntax-entry ?<  "."  table)
    (modify-syntax-entry ?>  "."  table)
    (modify-syntax-entry ?|  "."  table)
    (modify-syntax-entry ?\n "> b" table)
    table))

(defvar teco-font-lock-keywords
  '(("[#&*+:@-]" (0 'font-lock-operator-face))
    ("F?['<>|]" (0 font-lock-keyword-face))
    ("\\^[][_\\@A-Za-z]" (0 font-lock-constant-face))
    ("\"[ACDEFGLNRSTUVW<>=]" (0 font-lock-keyword-face))
    ("F[BCDKNRS_]" (0 font-lock-builtin-face))
    ("E[ABCFGIJKLMNPQRWXYZ_]" (0 font-lock-builtin-face))
    ("E[DEHOSTUV]" (0 'font-lock-variable-name-face))))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.te[sc]\\'"  . teco-mode))

;;;###autoload
(define-derived-mode teco-mode prog-mode "TECO"
  :syntax-table teco-mode-syntax-table
  (setq font-lock-defaults (list 'teco-font-lock-keywords))
  (setq-local comment-start "! "
              comment-end " !"))

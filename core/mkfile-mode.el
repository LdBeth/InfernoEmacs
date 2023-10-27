;; -*- lexical-binding:t -*-

(require 'make-mode)
(eval-when-compile (require 'rx))

(defvar mkfile-mode-syntax-table
  (let ((st (make-syntax-table makefile-mode-syntax-table)))
    (modify-syntax-entry ?\` "." st)
    st))

(defvar mkfile-dependency-regex
  (rx bol (group graph (*? nonl) ":")
      (? (+? nonl) (group ":"))
      (or (regexp "[\t\s]") eol)))

(defvar mkfile-rule-action-regex
  "^[ \t]+\\(?:\\([-@]+\\)[ \t]*\\)\\(.*\\(?:\\\\\n.*\\)*\\)")

(defconst mkfile-font-lock-keywords
  `(("^[\s\t]*\\([^\s\n\t][^:#=\s\n\t]*\\)[\s\t]*="
     (1 font-lock-variable-name-face))
    (makefile-match-action
     (1 font-lock-type-face nil t)
     (2 'makefile-shell prepend)
     (3 font-lock-builtin-face prepend t))
    ("\\(^\\|[^$]\\)\\$\\(#?[-a-zA-Z0-9_]+\\|[@%<?^+*][FD]?\\)"
     2 font-lock-variable-name-face prepend)
    ("[^$]\\$\\([@%<?^+*_]\\|[a-zA-Z0-9]\\>\\)"
     1 font-lock-constant-face prepend)
    (,(rx (not "$")
          (group "$"
                 (regex (regexp-opt '("alltarget" "newprereq"
                                      "nproc" "pid" "prereq"
                                      "target" "stem")))))
     1 'makefile-targets prepend)
    ("^\\(?: [ 	]*\\)?\\(<|?\\)[\s\t]*\\([^:\s\t\n#]*\\)"
     (1 font-lock-keyword-face)
     (2 font-lock-variable-name-face))
    ("\\([`$]{\\)\\([^}]*\\)\\(}\\)"
     (1 font-lock-keyword-face)
     (2 'font-lock-string-face prepend t)
     (3 font-lock-keyword-face))
    (makefile-match-dependency
     (1 'makefile-targets prepend)
     (2 'makefile-targets prepend))))

;;;###autoload
(define-derived-mode mkfile-mode makefile-mode "Mkfile"
  "An adapted `makefile-mode' that knows about Mk."
  :syntax-table mkfile-mode-syntax-table
  (setq-local makefile-dependency-regex mkfile-dependency-regex
              makefile-rule-action-regex mkfile-rule-action-regex)
  (setq font-lock-defaults
	`(mkfile-font-lock-keywords ,@(cdr font-lock-defaults))))

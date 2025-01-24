;; mmix-mode.el --- major mode for editing MMIX code -*- lexical-binding: t; -*-

;; Author: Alexander Bauer <sirius.b@web.de>

;; Commentary:

;; This mode was written by Alexander Bauer, based on the nasm-mode 
;; of Per Lundberg <plundis@debian.org>.

;; This mode is based on text mode.  It defines a private abbrev table
;; that can be used to save abbrevs for assembler mnemonics.  It binds just
;; three keys:
;;
;;	TAB		tab to next tab stop
;;	C-j, C-m	newline and jump to this line's indentation level

;; This mode runs two hooks:
;;   1) An mmix-mode-set-comment-hook before the part of the initialization
;; depending on mmix-comment-char, and
;;   2) an mmix-mode-hook at the end of initialization.

;;; Code:


(defgroup mmix nil
  "MMIX programming"
  :group 'mmix)

(defcustom mmix-comment-char ? ;
  "*The comment-start character assumed by MMIX mode."
  :type 'sexp
  :group 'mmix)

(defvar mmix-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?\n "> b" st)
    (modify-syntax-entry ?/  ". 124b" st)
    (modify-syntax-entry ?*  ". 23" st)
    st))

(defvar mmix-mode-abbrev-table nil
  "Abbrev table used while in MMIX mode.")
(define-abbrev-table 'mmix-mode-abbrev-table ())

(defvar mmix-mode-map nil
  "Keymap for MMIX mode.")


(if mmix-mode-map
    nil
  (setq mmix-mode-map (make-sparse-keymap 'mmix-mode-map))
  (define-key mmix-mode-map "\C-c;"	'comment-region)
  (define-key mmix-mode-map "\C-i"	'tab-to-tab-stop)
  (define-key mmix-mode-map "\C-j"	'mmix-newline)
  (define-key mmix-mode-map "\C-m"	'mmix-newline)
  )


;; Keywords for Syntax-Highlighting
(defconst mmix-font-lock-keywords (purecopy
  (list
   '("\\;.*\\|\\*.*\\|?.*" 
         . font-lock-comment-face)
   '("\\<\\(local\\|is\\|greg\\|prefix\\|loc\\|byte\\|wyde\\|tetra\\|octa\\)"
         1 font-lock-keyword-face)
   '("\\#[0-9a-f]*\\|\\$\\([1-2][0-9][0-9]\\|[0-9][0-9]\\|[0-9]\\)" 
     . font-lock-variable-name-face)
   (cons (concat "^[0-9a-z:]+")
	 'font-lock-function-name-face)
;;   (cons (concat "\\<local\\|is\\|greg\\|prefix\\|loc\\|byte\\|wyde\\|tetra\\|octa")
;;         'font-lock-preprocessor-face)
   '("'.+'" 
         . font-lock-string-face)))
  "Additional expressions to highlight in MMIX mode.")

(put 'mmix-mode 'font-lock-defaults '(mmix-font-lock-keywords nil t))

(defvar mmix-code-level-empty-comment-pattern nil)
(defvar mmix-flush-left-empty-comment-pattern nil)
(defvar mmix-inline-empty-comment-pattern nil)


;;;###autoload
(define-derived-mode mmix-mode prog-mode "MMIX"
  "Major mode for editing MMIX code.
Features a private abbrev table and the following bindings:

\\[mmix-colon]\toutdent a preceding label, tab to next tab stop.
\\[tab-to-tab-stop]\ttab to next tab stop.
\\[mmix-newline]\tnewline, then tab to next tab stop.
\\[mmix-comment]\tsmart placement of assembler comments.

The character used for making comments is set by the variable
`mmix-comment-char' (which defaults to `; *').

Alternatively, you may set this variable in `mms-mode-set-comment-hook',
which is called near the beginning of mode initialization.

Turning on MMIX mode runs the hook `mmix-mode-hook' at the end of
 initialization.

Special commands:
\\{mmix-mode-map}
"
  :abbrev-table mmix-mode-abbrev-table
  :syntax-table mmix-mode-syntax-table
  ;; Font lock support
  (setq-local font-lock-defaults '(mmix-font-lock-keywords nil t)
              comment-end ""
              fill-prefix "\t"
              comment-column 32))

(defun mmix-newline ()
  "Insert LFD + fill-prefix, to bring us back to code-indent level."
  (interactive)
  (if (eolp) (delete-horizontal-space))
  (insert "\n")
  )

(defun mmix-line-matches (pattern &optional _withcomment)
  (save-excursion
    (beginning-of-line)
    (looking-at pattern)))

(defun mmix-pop-comment-level ()
  ;; Delete an empty comment ending current line.  Then set up for a new one,
  ;; on the current line if it was all comment, otherwise above it
  (end-of-line)
  (delete-horizontal-space)
  (while (= (preceding-char) mmix-comment-char)
    (delete-char -1))
  (delete-horizontal-space)
  (if (bolp)
      nil
    (beginning-of-line)
    (open-line 1))
  )

(defun mmix-comment ()
  "Convert an empty comment to a `larger' kind, or start a new one.
These are the known comment classes:

1 -- comment to the right of the code (at the comment-column)
2 -- comment on its own line, indented like code
3 -- comment on its own line, beginning at the left-most column.

Suggested usage:  while writing your code, trigger mmix-comment
repeatedly until you are satisfied with the kind of comment."
  (interactive)
  (cond

   ;; Flush-left comment present?  Just insert character.
   ((mmix-line-matches mmix-flush-left-empty-comment-pattern)
    (insert mmix-comment-char))

   ;; If all else fails, insert character
   (t
    (insert mmix-comment-char))

   ))

;;;###autoload
(setq auto-mode-alist (cons '("\\.mms$" . mmix-mode) auto-mode-alist))

;;; mmixy-mode.el ends here

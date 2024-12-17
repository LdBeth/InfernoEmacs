;;; txl-mode.el -- major mode for editing TXL programs and grammars.
;; Markus Stoy (mstoy@gmx.de), Rostock (Germany), November/December 2003.

;; Installation (only tested under XEmacs-21.4 for Linux and WindowsXP):
;  - put this file into directory where Emacs can find it (within load-path)
;  - add following lines to Emacs init file (.emacs or init.el or maybe something else)
;  (require 'txl-mode)
;  (add-to-list 'auto-mode-alist '("\\.\\([tT]xl\\|[gG]rm\\|[gG]rammar\\|[rR]ul\\(es\\)?\\|[mM]od\\(ule\\)?\\)$" . txl-mode))

;; Features:
;  - syntax highlighting (with font-lock-mode)
;  - automatic indentation according to TXL style guide (perhaps stil buggy...)
;  - compile/debug/run TXL program from within Emacs
;  - comment/uncomment regions
;  - insert skeletion rules/functions/defines, find and insert matching end's
;  - abbreviations for keywords (with abbrev-mode; scroll down to see a list)
;  - TXL submenu which contains all new functions and their keyboard shortcuts

;; Wish list:
;  - navigation (jump to nonterminal/function/rule under cursor, next/previous nonterminal/function/rule, ...)
;  - use comint for run/debug/compile instead of simple shell-command? (which looks ugly under Windows)

;; Known bugs:
;  - 'x% is highlighted as comment
;  - compile and debug don't work under Windows

;; Oct 16 2008, Ivan N. Veselov <veselov@gmail.com>
;; - added compatibility with Emacs (fixed GNU Emacs/XEmacs compatibility issues
;;   with font-lock-defaults and set-keymap-name).
;;   Tested with GNU Emacs 22.3.1.

;;; Code: ----------------------------------------------------------------------

(defvar txl-mode-hook nil "Normal hook run when entering TXL mode.")

; syntax table -----------------------------------------------------------------
(defvar txl-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?'  "'" table) ; apostrophe quotes
    ;; % participates in all 3 comment styles %...\n %(...)% %{...}%
    (modify-syntax-entry ?%  "< 14" table)
    (modify-syntax-entry ?\n ">" table)
    (modify-syntax-entry ?\( "()2b" table)
    (modify-syntax-entry ?\) ")(3b" table)
    (modify-syntax-entry ?\{ "(}2c" table)
    (modify-syntax-entry ?\} "){3c" table)
    table)
  "Syntax table used while in TXL mode.")

(defvar txl-mode-font-lock-syntax-alist
  '((?' . "/")                       ; ' escapes keywords and comments
    (?_ . "w"))                      ; don't highlight keyword_foo
  "Syntax used for highlighting TXL")

(defvar txl-mode-font-lock-syntactic-keywords
  '(("\"[^\"]*\\('\\)\"" 1 ".")))    ; ' doesn't escape inside strings

; syntax highlighting ----------------------------------------------------------
(defvar txl-mode-keywords
  `(
    ;; preprocessor directives
    ("^[[:space:]]*#[a-z]+" 0 font-lock-preprocessor-face)
    ;; quoted literal symbol
    ("'[^]\t\n ]+" 0 font-lock-constant-face)
    ;; builtin rules (with parameters)
    (,(concat "\\[\\(\\(?:[\\+-\\*/:#_\\.^,=><\\$]\\|"
              (regexp-opt
               '("div" "rem" "index" "length" "select" "head" "tail" "~=" ">="
                 "<=" "grep" "quote" "unquote" "parse" "unparse" "reparse"
                 "read" "write" "fget" "getp" "fput" "putp" "fputp" "fclose"
                 "message" "pragma" "quit" "system" "pipe"))
              "\\)\\)[[:space:]]+")
     1 font-lock-builtin-face)
    ;; builtin rules (without parameters) and predefined nonterminal types
    (,(concat "\\["
              (regexp-opt
               '("!" "get" "put" "print" "printattr" "debug" "breakpoint" "id"
                 "number" "stringlit" "charlit" "comment" "space" "newline"
                 "upperlowerid" "upperid" "lowerupperid" "lowerid"
                 "floatnumber" "decimalnumber" "integernumber"
                 "empty" "key" "token" "any") t))
     1 font-lock-builtin-face)
    ;; formatting tokens (without number)
    (,(concat "\\[\\(?:"
              (regexp-opt '("NL" "IN" "EX" "TAB" "SP" "SPOFF" "SPON" "KEEP"))
              "\\)\\]")
     0 font-lock-comment-face)
    ;; formatting tokens (with number)
    ("\\[\\(IN\\|EX\\|TAB\\|SP\\)_[1-9][0-9]*\\]" 0 font-lock-comment-face)
    ;; type keywords
    (,(regexp-opt '("attr" "list" "opt" "repeat" "see") 'words)
     1 font-lock-type-face)
    ;; other keywords
    (,(regexp-opt '("all" "assert" "by" "comments" "compounds" "construct"
                    "deconstruct" "define" "each" "end" "export" "external"
                    "function" "import" "include" "keys" "match" "not"
                    "redefine" "replace" "rule" "skipping" "tokens" "where")
                  'words)
     1 font-lock-keyword-face)
    ;; number
    ("\\<[0-9]+\\([.][0-9]+\\)?\\([eE][-+]?[0-9]+\\)?\\>" 0 font-lock-constant-face))
  "Keywords for font-lock-mode used while in TXL mode.")

; abbreviations ----------------------------------------------------------------
(defvar txl-mode-abbrev-table
  (let ((table (make-abbrev-table)))
    (define-abbrev table "ass" "assert" nil)
    (define-abbrev table "com" "comments" nil)
    (define-abbrev table "cmp" "compounds" nil)
    (define-abbrev table "con" "construct" nil)
    (define-abbrev table "dec" "deconstruct" nil)
    (define-abbrev table "def" "define" nil)
    (define-abbrev table "exp" "export" nil)
    (define-abbrev table "ext" "external" nil)
    (define-abbrev table "fun" "function" nil)
    (define-abbrev table "imp" "import" nil)
    (define-abbrev table "inc" "include" nil)
    (define-abbrev table "red" "redefine" nil)
    (define-abbrev table "rpt" "repeat" nil)
    (define-abbrev table "rep" "replace" nil)
    (define-abbrev table "ski" "skipping" nil)
    (define-abbrev table "tok" "tokens" nil)
    table)
  "Abbrev table used while in TXL mode.")

; keyboard shortcuts -----------------------------------------------------------
(defvar txl-mode-map
  (let ((map (make-sparse-keymap)))
    (if (functionp 'set-keymap-name)
        (set-keymap-name map 'txl-mode-map))
    (define-key map "\C-cc" 'comment-region)
    (define-key map "\C-cu" 'txl-mode-uncomment-region)
    (define-key map "\C-cd" 'txl-mode-insert-define)
    (define-key map "\C-cf" 'txl-mode-insert-function)
    (define-key map "\C-cr" 'txl-mode-insert-rule)
    (define-key map "\C-c\C-e" 'txl-mode-insert-end)
    (define-key map "\C-c\C-c" 'txl-mode-compile)
    (define-key map "\C-c\C-d" 'txl-mode-debug)
    (define-key map "\C-c\C-r" 'txl-mode-run)
    (define-key map "\C-i" 'txl-mode-indent-line)
    (define-key map "\C-C\C-i" 'indent-region)
    map)
  "Keymap for TXL mode.")

; menubar ----------------------------------------------------------------------
(defvar txl-mode-menubar-menu
  '("T%_XL"
    ["Ru%_n " txl-mode-run :suffix (concat (txl-mode-get-name nil) "...")]
    ["De%_bug " txl-mode-debug :suffix (concat (txl-mode-get-name nil) "...")]
    ["Com%_pile " txl-mode-compile :suffix (txl-mode-get-name nil)]
    "--"
    ["%_Indent Region" indent-region :active (region-exists-p)]
    ["%_Comment Region"    comment-region :active (region-exists-p)]
    ["%_Uncomment Region"  txl-mode-uncomment-region (region-exists-p)]
    "--"
    ["Insert %_Define" txl-mode-insert-define]
    ["Insert %_Function" txl-mode-insert-function]
    ["Insert %_Rule" txl-mode-insert-rule]
    ["%_End Block" txl-mode-insert-end :active (txl-mode-block)]
    "--"
    ["Use %_Abbreviations" (setq abbrev-mode (not abbrev-mode))
     :style toggle :selected abbrev-mode])
  "TXL menu.")

(defvar txl-mode-input-file '()
  "The last input file used for `txl-mode-run' and `txl-mode-debug'")
(defvar txl-mode-options nil "The last options used for `txl-mode-run'")


(defun txl-mode () ; -----------------------------------------------------------
  "Major mode for editing TXL programs and grammars.
\\{txl-mode-map}
Turning on TXL mode runs the normal hook `txl-mode-hook'."
  (interactive)
  (kill-all-local-variables)
  (make-local-variable 'txl-mode-input-file)
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'txl-mode-indent-line)
  (make-local-variable 'comment-start)
  (setq comment-start "%")
  (make-local-variable 'comment-end)
  (setq comment-end "")
  (set-syntax-table txl-mode-syntax-table)
  (if (featurep 'xemacs)
      (setq font-lock-keywords txl-mode-keywords) ;; XEmacs
    (setq font-lock-defaults                      ;; Emacs
          `(txl-mode-keywords
            nil nil
            ,txl-mode-font-lock-syntax-alist
            nil
            (font-lock-syntactic-keywords
             . ,txl-mode-font-lock-syntactic-keywords))))
  (setq local-abbrev-table txl-mode-abbrev-table)
  (setq abbrev-mode t)
  (use-local-map txl-mode-map)
  (setq major-mode 'txl-mode
	mode-name "TXL")
  (if (and (featurep 'menubar)
           current-menubar)
      (progn
	(set-buffer-menubar current-menubar)
	(add-submenu nil txl-mode-menubar-menu)))
  (run-hooks 'txl-mode-hook))

; code templates ---------------------------------------------------------------

(defun txl-mode-insert-define ()
  "Insert an empty nonterminal definition."
  (interactive)
  (insert "\ndefine \nend define")
  (end-of-line 0))

(defun txl-mode-insert-function ()
  "Insert an empty function."
  (interactive)
  (insert "\nfunction \n    replace\n    by\nend function")
  (end-of-line -2))

(defun txl-mode-insert-rule ()
  "Insert an empty rule."
  (interactive)
  (insert "\nrule \n    replace\n    by\nend rule")
  (end-of-line -2))

(defun txl-mode-insert-end ()
  "Insert matching end for define, rule, function etc."
  (interactive)
  (let ((current-block (txl-mode-block)))
    (if current-block
	(insert (concat "end " current-block "\n\n"))
      (message "Not inside TXL block."))))

(defun txl-mode-uncomment-region ()
  "Uncomment region."
  (interactive)
  (comment-region (region-beginning) (region-end) -1))

; compile, debug and run TXL programs ------------------------------------------

(defun txl-mode-compile ()
  "Compile TXL program."
  (interactive)
  (shell-command (concat "txlc " (txl-mode-get-name t)) "*TXL Compilation*"))

(defun txl-mode-debug (input-file)
  "Ask input file from user and debug TXL program."
  (interactive (list (read-file-name "Input file: " nil txl-mode-input-file t)))
  (setq txl-mode-input-file input-file)
  (shell-command (concat "txldb " input-file " " (txl-mode-get-name t) " &") "*TXL Debug*")
  (other-window 1)
  (end-of-buffer))

(defun txl-mode-run (input-file &optional options)
  "Ask input file from user and run TXL program. With prefix arg
ask for extra options as well."
  (interactive (list (read-file-name "Input file: " nil txl-mode-input-file t)
                     (when current-prefix-arg
                       (read-string (format "txl options (%s): " txl-mode-options)
                                    nil nil txl-mode-options))))
  (setq txl-mode-input-file input-file)
  (if options (setq txl-mode-options options)
    (setq options ""))
  (shell-command (concat "txl " options " " input-file " " (txl-mode-get-name t)) "*TXL Output*"))

(defvar txl-mode-extension-regexp
  "[tT]xl\\|[gG]rm\\|[gG]rammar\\|[rR]ul\\(es\\)?\\|[mM]od\\(ule\\)?"
  "A regexp that matches filename extensions used by TXL")

(defun txl-mode-get-name (full)
  "If buffer file name has ending used by TXL, return base name
and ending `.Txl', otherwise return unchanged. The argument controls whether full path
is included or not."
  (let ((ext (file-name-extension buffer-file-name))
        (base (file-name-sans-extension (file-name-nondirectory buffer-file-name))))
    (concat (if full
                (file-name-directory buffer-file-name) "")
            base
            (if (string-match txl-mode-extension-regexp ext)
                ".Txl" ext))))

; automatic indentation --------------------------------------------------------

(defun txl-mode-indent-line (&optional whole-exp)
  "Indent current line as TXL code.
With argument, indent any additional lines of the same clause
rigidly along with this one (not yet)."
  (interactive "p")
  (let ((indent (txl-mode-indent-level))
        (pos (point-marker))
        (beg (line-beginning-position)))
    (back-to-indentation)
    (unless (zerop (- indent (current-column)))
      (delete-region beg (point))
      (indent-to indent))
    ;; special case for grammar alternative indentation
    (when (looking-at "|")
        (forward-char)
        (just-one-space 3))
    (when (> (marker-position pos) (point))
      (goto-char pos))
    (set-marker pos nil)))

(defun txl-mode-indent-level ()
  "Compute TXL indentation level."
  (save-excursion
    (beginning-of-line)
    (skip-chars-forward " \t")
    (cond
     ;; beginning of buffer: do not indent
     ((bobp) 0)
     ;; block start/end delimiters: do not indent
     ((looking-at "\\(define\\|redefine\\|keys\\|compounds\\|comments\\|tokens\\|function\\|rule\\|end\\)\\>") 0)
     ;; block-keywords: indent 4
     ((looking-at "\\(import\\|export\\|replace\\|by\\|match\\|deconstruct\\|construct\\|where\\|assert\\|skipping\\)\\>") 4)
     ;; alternative-sperator: indent 4
     ((looking-at "|") 4)
     ;; comments: within blocks indent 4, outside do not indent
     ((looking-at "%") (if (txl-mode-block) 4 0))
     ;; function calls: line up with previous
     ((looking-at "\\[") (txl-mode-indent-fun-level))
     ;; all other stuff: within blocks indent 8, outside do not indent
     (t (if (txl-mode-block) 8 0)))))

(defun txl-mode-block ()
  "If point is currently inside TXL block, return its name (define, rule, function, etc),
otherwise return nil."
  (interactive)
  (save-excursion
    (let ((searching t) (within-block nil))
      (while searching
	(beginning-of-line 0)
	(if (bobp)
	    (setq searching nil))
	(skip-chars-forward " \t")
	(if (looking-at "\\(define\\|redefine\\|keys\\|compounds\\|comments\\|tokens\\|function\\|rule\\)\\>")
	    (setq searching nil within-block (match-string 1)))
	(if (looking-at "end\\>")
	    (setq searching nil within-block nil)))
      within-block)))

(defun txl-mode-indent-fun-level ()
  "Compute indentation level of first function call on previous line.
If there is none, return 8 or 0, depending whether currently inside block."
  (save-excursion
    (end-of-line 0)
    (let ((eol (point)))
      (beginning-of-line)
      (skip-chars-forward "^\[" eol)
      (if (eolp)
	  (progn
	    (end-of-line 2)
	    (if (txl-mode-block) 8 0))
	(current-column)))))

(provide 'txl-mode)
;;; txl-mode.el ends here

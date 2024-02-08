;;; teco-mode.el --- Emacs mode for TECO macro -*- lexical-binding: t; -*-
;; Copyright (C) 2024 LdBeth
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; If not, see <https://www.gnu.org/licenses/>.
;;
;; This teco mode mainly targets TECO-64 which has slightly different
;; implementation on how to read in commands compared to traditional
;; tecoes.
(eval-when-compile
  (require 'rx))

(defgroup teco nil
  "A mode for editing TECO macros"
  :group 'languages
  :prefix "teco-")

(defcustom teco-atsign-use-braces 'ignore
  "Controls if @ delimiters use special treatment for braces.
 This is for TECO-64 when E1&4 is set. If set to `ignore', braces
 enclosed string are not font-locking. Set to `nil' to follow
 traditional TECO convention. When the value is true, whenever
 thedelimiter after @ modified command is brace the other
 delimiter must be paired."
  :type 'symbol
  :group 'teco)
(put 'teco-atsign-use-braces 'safe-local-variable #'symbolp)

(defcustom teco-atsign-ignore-spaces t
  "Controls if @ delimiters ignores whitespaces.
TECO-64 looks for non whitespaces."
  :type 'boolean
  :group 'teco)
(put 'teco-atsign-ignore-spaces 'safe-local-variable #'booleanp)

(defconst teco-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?! "! 12b" table)

    (modify-syntax-entry ?\C-a "\"" table)
    (modify-syntax-entry ?\" "." table)
    (modify-syntax-entry ?\' "." table)
    (modify-syntax-entry ?\[ "." table) ; Q-reg push
    (modify-syntax-entry ?\] "." table) ; Q-reg pop
    (modify-syntax-entry ?*  "." table)
    (modify-syntax-entry ?/  "." table)
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
  '(("\33\\|\\^\\["
     (0 (progn (compose-region (match-beginning 0) (match-end 0)
                               ?$)
               font-lock-keyword-face)))
    ("[=<>]=\\|<>\\|//\\|<<\\|>>\\|\\^_\\|\\\\/" (0 'font-lock-operator-face))
    ("[#&*+/!~:@-]" (0 'font-lock-operator-face))
    ("F?['<>|]" (0 font-lock-keyword-face))
    ("\\^[][_\\@A-Za-z]" (0 font-lock-constant-face))
    ("\\(?:^U\\|[]UXQGM*%\C-u[]\\)\\(\\.?[a-zA-Z0-9]\\)"
     (1 font-lock-function-name-face))
    ("\"[ACDEFGLNRSTUVW<>=]" (0 font-lock-keyword-face))
    ("F[1-4BCDKMNRS_]" (0 font-lock-builtin-face))
    ("E[ABCFGIJKLMNPQRWXYZ%_]" (0 font-lock-builtin-face))
    ("E[1-4DEHOSTUV]" (0 'font-lock-variable-name-face))
    ("F?[HZ]\\|[B.]\\|F0" (0 'font-lock-variable-name-face))))

(eval-when-compile
  (rx-define teco-rx-regiser
    (seq (? ".") (any "A-Z0-9")))
  (rx-define teco-rx-atsign-1-arg
    (or (seq "E" (or (any "%BGILNRW_")
                     (seq (any "QM") teco-rx-regiser)))
        (regex "F[BDMKR]")
        "^A"
        (any "\C-a=!INOS_")
        (seq (or "^U" "\C-u") teco-rx-regiser)))
  (rx-define teco-rx-atsign-2-arg
    (seq "F" (any "1-4CNS_"))))

(defun teco-mode-syntax-propertize (start end)
  (goto-char start)
  (let* ((ppss (syntax-ppss))
         (string-start (and (eq t (nth 3 ppss)) (nth 8 ppss))))
    (when string-start
      (goto-char string-start)
      (search-backward "@" nil t)))
  (while (and (< (point) end)
              (re-search-forward (rx (or (seq (regex "@[\s\t\n\r,:0-9]*")
                                              (group teco-rx-atsign-1-arg))
                                         (seq (regex "@[\s\t\n\r,:0-9]*")
                                              (group teco-rx-atsign-2-arg))))
                                 end t))
    (let* ((cmd (match-beginning 1))
           is-comment
           (delimiter
            (if teco-atsign-ignore-spaces
                (prog1 (and (looking-at "[\s\t\n\r]*\\([^\C-@]\\)")
                            (char-after (match-beginning 1)))
                  (goto-char (match-beginning 1)))
              (char-after (point))))
           (string-start (point)))
      (when (and cmd (eql (char-after cmd) ?!))
        ;; Cancel syntax class of `!'
        (put-text-property cmd (1+ cmd) 'syntax-table '(1))
        (setq is-comment t))
      (if (and (eq teco-atsign-use-braces 'ignore)
               (eql delimiter ?{))
          (setq delimiter nil))
      (let ((ppss (syntax-ppss)))
        (unless (eq t (nth 3 ppss))
          (setq cmd (if cmd 1 2))
          (when delimiter
            (put-text-property string-start (1+ string-start) 'syntax-table
                               (if is-comment '(14) '(15)))
            (let ((search
                   (cond
                    ((and teco-atsign-use-braces (eql delimiter ?\[))
                     "]")
                    ((and teco-atsign-use-braces (eql delimiter ?{))
                     "}")
                    (t
                     (string delimiter)))))
              (forward-char)
              (when (search-forward search end t cmd)
                (put-text-property (1- (point)) (point) 'syntax-table
                                   (if is-comment '(14) '(15)))))))))))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.te[sc]\\'"  . teco-mode))

;;;###autoload
(define-derived-mode teco-mode prog-mode "TECO"
  :syntax-table teco-mode-syntax-table
  (setq font-lock-defaults (list 'teco-font-lock-keywords nil t)
        font-lock-multiline t)
  (setq-local comment-start "! "
              comment-end " !"
              syntax-propertize-function #'teco-mode-syntax-propertize))

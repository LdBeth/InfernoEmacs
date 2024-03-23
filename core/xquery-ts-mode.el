;;; rnc-ts-mode.el --- Emacs mode to edit Relax-NG Compact files  -*- lexical-binding:t -*-

;; Copyright (C) 2023 LdBeth

;; Author: LdBeth <ldbeth@sdf.org>
;; Keywords: xml relaxng
;; Version: 0.5

;; This file is not part of GNU Emacs.

;; rnc-ts-mode is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; rnc-ts-mode is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.


;;; Commentary:

(require 'treesit)
(eval-when-compile
  (require 'rx))

;;; Code:

;;;###autoload
(add-to-list 'auto-mode-alist '(".xq[myl]?\\'" . xquery-ts-mode))

(defconst xquery-mode-syntax-table
  (let ((st (make-syntax-table)))
    ;; single-quotes are equivalent to double-quotes
    (modify-syntax-entry ?' "\"" st)
    ;; treat underscores as punctuation
    (modify-syntax-entry ?\_ "." st)
    ;; treat hypens as punctuation
    (modify-syntax-entry ?\- "." st)
    ;; colons are both punctuation and comments
    ;; the space after '.' indicates an unused matching character slot
    (modify-syntax-entry ?\: ". 23" st)
    ;; XPath step separator / is punctuation
    (modify-syntax-entry ?/ "." st)
    ;; xquery doesn't use backslash-escaping, so \ is punctuation
    (modify-syntax-entry ?\\ "." st)
    ;; set-up the syntax table correctly for all the different braces
    (modify-syntax-entry ?\{ "(}" st)
    (modify-syntax-entry ?\} "){" st)
    (modify-syntax-entry ?\[ "(]" st)
    (modify-syntax-entry ?\] ")]" st)
    ;; parens may indicate a comment, or may be a sequence
    (modify-syntax-entry ?\( "()1n" st)
    (modify-syntax-entry ?\) ")(4n" st)
    st))

(defconst xquery-mode-keywords
  '(
    ;; FLWOR
    ;;"let" "for"
    "at" "in"
    "where"
    "stable order by" "order by"
    "ascending" "descending" "empty" "greatest" "least" "collation"
    "return"
    ;; XPath axes
    "self" "child" "descendant" "descendant-or-self"
    "parent" "ancestor" "ancestor-or-self"
    "following" "following-sibling"
    "preceding" "preceding-sibling"
    ;; conditionals
    "if" "then" "else"
    "typeswitch" ;"case" "default"
    ;; quantified expressions
    "some" "every" "construction" "satisfies"
    ;; schema
    "schema-element" "schema-attribute" "validate"
    ;; operators
    "intersect" "union" "except" "to"
    "is" "eq" "ne" "gt" "ge" "lt" "le"
    "or" "and"
    "div" "idiv" "mod"))

(defcustom xquery-mode-indent-width 2
  "Indent width for `xquery-mode'."
  :group 'xquery-mode
  :type 'integer)

(defvar xquery--treesit-font-lock-settings
  (treesit-font-lock-rules

   :language 'xquery
   :feature 'comment
   '((comment) @font-lock-comment-face)

   :language 'xquery
   :feature 'keyword
   `([,@xquery-mode-keywords] @font-lock-keyword-face)))

;;;###autoload
(define-derived-mode xquery-ts-mode prog-mode "XQuery"
  "A major mode for W3C XQuery 1.0"
  :syntax-table xquery-mode-syntax-table
  (when (treesit-ready-p 'xquery)
    (setq-local comment-start "#")
    (treesit-parser-create 'xquery)
    (setq-local treesit-font-lock-settings rnc--treesit-font-lock-settings)
    (setq-local treesit-font-lock-feature-list
                '((comment definition)
                  (keyword string)
                  (delimiter docstring namespace)))
    (setq-local treesit-defun-type-regexp (rx bos (or "define" "declare") eos))
    (setq-local treesit-defun-name-function #'rnc--treesit-defun-name)
    (setq-local treesit-simple-indent-rules rnc--treesit-indent-rules)
    (setq-local treesit-simple-imenu-settings
                `(("Definition" ,(rx bos "define" eos) nil nil)
                  ("Namespace" ,(rx bos "declare" eos)
                   nil nil)))
    (treesit-major-mode-setup)))


(provide 'xquery-ts-mode)
;;; rnc-ts-mode.el ends here

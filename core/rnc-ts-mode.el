;;; rnc-ts-mode.el --- Emacs mode to edit Relax-NG Compact files  -*- lexical-binding:t -*-

;; Copyright (C) 2023 LdBeth

;; Author: LdBeth <ldbeth@sdf.org>
;; Keywords: xml relaxng
;; Version: 0.3

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

(require 'nxml-mode)
(require 'treesit)
(eval-when-compile
  (require 'rx))

;;; Code:

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.rnc\\'" . rnc-ts-mode))

(defconst rnc-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?# "<" st)
    (modify-syntax-entry ?\n ">" st)
    (modify-syntax-entry ?\" "\"" st)
    (modify-syntax-entry ?\' "\"" st)
    (modify-syntax-entry ?- "_" st)
    (modify-syntax-entry ?. "_" st)
    (modify-syntax-entry ?: "_" st)
    (modify-syntax-entry ?_ "_" st)
    st))

(defconst rnc--keywords
  ;; Taken from the grammar in http://relaxng.org/compact-20021121.html,
  ;; by order of appearance.
  '("namespace" "default" "datatypes" "element" "attribute"
    "list" "mixed" "parent" "empty" "text" "notAllowed" "external"
    "grammar" "div" "include" ;; "start"
    "string" "token" "inherit"))

(defconst rnc--operators
  '("=" "&=" "|=" "*" "?" "+" "-" "~"))

(defconst rnc--delimiters '("&" "," "|")) 

(defvar rnc-indent-level 2)

(defvar rnc--treesit-font-lock-settings
  (treesit-font-lock-rules

   :language 'rnc
   :feature 'comment
   '((comment) @font-lock-comment-face)

   :language 'rnc
   :feature 'keyword
   `([,@rnc--keywords] @font-lock-keyword-face)

   :language 'rnc
   :feature 'string
   '((literal_segment) @font-lock-string-face)

   :language 'rnc
   :feature 'definition
   '((define
      name: (identifier) @font-lock-function-name-face)

     (param
      name: (identifier) @font-lock-variable-name-face))

   :language 'rnc
   :feature 'namespace
   '((declare
      name: (identifier) @font-lock-constant-face)
     (name
      ns: (prefix) @font-lock-constant-face)
     (datatype_name
      ns: (prefix) @font-lock-constant-face))

   :language 'rnc
   :feature 'docstring
   '((documentations) @font-lock-doc-face)

   :language 'rnc
   :feature 'operator
   `([,@rnc--operators] @font-lock-operator-face)

   :language 'rnc
   :feature 'bracket
   '((["(" ")" "[" "]" "{" "}"]) @font-lock-bracket-face)

   :language 'rnc
   :feature 'delimiter
   `(([,@rnc--delimiters]) @font-lock-delimiter-face)))

(defvar rnc--treesit-indent-rules
  `((rnc
     ((parent-is "declare") parent-bol 0)
     ((node-is "}") parent-bol 0)
     ((node-is ")") parent-bol 0)
     ((node-is "]") parent-bol 0)
     ((node-is "literal_segment") parent-bol 0)
     ((parent-is "comment") prev-adaptive-prefix 0)
     ((parent-is ,(rx (seq (one-or-more alpha) "_pattern"))) first-sibling 0)
     ((parent-is ,(rx (seq (one-or-more alpha) "_block"))) parent-bol rnc-indent-level)
     ((parent-is "param") great-grand-parent rnc-indent-level)
     ((field-is "body") parent-bol rnc-indent-level)
     )))

(defun rnc--treesit-defun-name (node)
  "Return the defun name of NODE.
Return nil if there is no name or if NODE is not a defun node."
  (treesit-node-text
   (treesit-node-child-by-field-name
    node
    "name")
   t))

;;;###autoload
(define-derived-mode rnc-ts-mode prog-mode "RNC"
  "Major mode to edit Relax-NG Compact files."
  (when (treesit-ready-p 'rnc)
    (setq-local comment-start "#")
    (treesit-parser-create 'rnc)
    (setq-local treesit-font-lock-settings rnc--treesit-font-lock-settings)
    (setq-local treesit-font-lock-feature-list
                '((comment definition)
                  (keyword string)
                  (bracket delimiter operator docstring namespace)))
    (setq-local reesit-defun-name-function #'rnc--treesit-defun-name)
    (setq-local treesit-simple-indent-rules rnc--treesit-indent-rules)
    (treesit-major-mode-setup)))


(provide 'rnc-ts-mode)
;;; rnc-ts-mode.el ends here

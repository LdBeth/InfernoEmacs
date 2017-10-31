;;; core-lib.el --- Inferno Emacs Core Library       -*- lexical-binding: t; -*-

;; Copyright (C) 2017  LdBeth

;; Author: Jair Wang <ldbeth@Costume-Party.local>
;; Keywords: lisp

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:

(defmacro label (name symbol)
  "Set NAME to SYMBOL's function definetion."
  `(fset (quote ,name) ,(if (functionp symbol)
                            (symbol-function symbol)
                          (let ((def (eval symbol)))
                            (if (eq (car def) 'lambda)
                                def)))))


(defconst exclamation-macro-mark '\$
  "Symbol used to represent a macro in `exclamation.'")

(defmacro exclamation (&rest structure)
  "Expand the $ structure as inline macros at compiling time
 to optimze byte-code."
  (cl-labels ((process
               (s)
               (if (listp s)
                   (cl-loop for i in s
                            collect
                            (if (and
                                 (listp i)
                                 (eq
                                  (car i)
                                  exclamation-macro-mark))
                                (let* ((lst (cdr i))
                                       (war (check lst)))
                                  (if war lst
                                    (eval lst)))
                              (process i)))
                 s))
              (check
               (sexp)
               ;; A example of using `catch' and `throw'.
               (catch 'unbound
                 (cl-loop for x in sexp do ; do form in `loop' always returns nil.
                          (if (listp x)
                              (let ((head (car x)))
                                (unless (fboundp head)
                                  (throw 'unbound t)
                                  (warn
                                   "symbol's function definition void: `%S' "
                                   (car x)))
                                (unless (or
                                         (macrop head)
                                         (special-form-p head))
                                  check x))
                            (or
                             (not (symbolp x))
                             (boundp x)
                             (fboundp x)
                             (progn
                               (throw 'unbound t)
                               (warn
                                "symbol's definition void: `%S'"
                                x))))))))
	     (cons 'progn (process structure))))

(defalias 'excl (symbol-function 'exclamation))



(defconst anomyous-arg-mark (rx bos "%" num)
  "String used to represent an argument in `anomyous'.")

(defmacro anomyous (&optional docstring &rest body)
  "Shortcut for `lambda'.

Inside BODY symbols in the form %N where N is a positive
number are to stand for positional arguments to the generated
lambda.

BODY can be either wrapped in a list or not.

e.g.

anomyous + %1 %2 is equivalent to  and."
  (declare (doc-string 1))
  (let* ((dstrp (stringp docstring))
         (form (if dstrp body
                 (if (and body docstring)
                     (cons docstring body)
                   (or
                    body
                    (list docstring))))))
    `(lambda
       ,(cl-labels ((collect-vars
                     (&rest forms)
                     (cl-loop
                      for form in forms
                      append
                      (cl-loop
                       for atom in form
                       if (and (symbolp atom)
                               (string-match anomyous-arg-mark
                                             (symbol-name atom)))
                       collect atom
                       else if (consp form)
                       append (collect-vars atom)))))
          (cl-sort
           (collect-vars form)
           #'string<
           :key #'symbol-name))
       ,@(when dstrp (list docstring))
       ,@form)))

;; (require 'dash)

;; (defun anomyous-find-args (sexp)
;;   "Find and return anoymous args from SEXP."
;;   (seq-sort
;;    (lambda (sym1 sym2)
;;      (< (string-to-number (substring (symbol-name sym1) 1))
;;         (string-to-number (substring (symbol-name sym2) 1))))
;;    (seq-filter
;;     (lambda (x)
;;       (and (symbolp x) (equal 0 (string-match "\\%[0-9]+" (symbol-name x)))))
;;     (-flatten sexp))))

;; (defmacro anomyous (&rest body)
;;   "Dynamic scoping version of `anoymous'"
;;   (let* ((head (car body))
;;          (argp (vectorp head))
;;          (form (if argp
;;                    (cdr body)
;;                  body)))
;;     `(lambda ,(if argp
;;                   (seq-into head 'list)
;;                 (anoymous-find-args body))
;;        ,@(if (cdr form)
;;              (list form)
;;            form))))


(defalias 'amys (symbol-function 'anoymous))


(defmacro repeat-declare-syntax (mark form &rest args)
  "Repeat MARK with coresponding FORM on each of ARGS as arguments.
If FORM is a function, it is only applied to the secound element of ARGS"
  (declare (indent 2))
  (cons 'progn
        (cl-loop for i in args collect
                 `(,mark
                   ,(if (and
                         (listp i)
                         (not (eq (car i) 'quote)))
                        (car i)
                      i)
                   ,@(cond
                      ((or
                        (not (listp i))
                        (eq (car i) 'quote))
                       nil)
                      ((functionp form)
                       (list
                        (funcall form
                                 (cadr i))
                        (cl-caddr i)))
                      ((not form)
                       (cdr i))
                      (t
                       (cl-loop for x in form
                                for y in (cdr i)
                                collect
                                (funcall x y))))))))


(defmacro progress (&optional msg &rest body)
  "Excute BODY with MSG for each success excution."
  (declare (indent 1))
  (cons 'progn
        (let (var)
          (cl-loop for i in body
                   do (progn
                        (push i var)
                        (push msg var)))
          (nreverse var))))



(defmacro defn (fname args &optional docstring &rest body)
  "Macro to define a function with improved argument documentation.
FNAME is a symbol for the function name.
ARGS is a list of arguments. Each argument can be either a symbol
or a list of the form (arg-symbol arg-docstring options) where
options is a plist style of options that include:
:default value
:validate function (the function takes one argument, and should
return t if the argument is valid.
:rest (this indicates the rest of the arguments go into this
variable, and it has to be last)
The function docstring is built up from that information.
Default values will automatically be set in the body, and
validation code will be automatically generated if the option is
present.
DOCSTRING is an optional string for the overall purpose of the
function. The argument docstrings will be appended onto this.
BODY is a form for the function."
  (declare (doc-string 3) (indent defun))
  (if
      (not (stringp docstring))
      (setq body docstring
            docstring "No documentation provided."))
  (let* (_ds
         arg-options
         ;; build up the docstring.
         (ds (concat
              (or docstring "No docstring defined.")
              "\n"
              (mapconcat
               'identity
               (cl-loop for arg in args
                        collect
                        (cond
                         ((listp arg)
                          (setq arg-options
                                (if (stringp (nth 1 arg))
                                    (cddr arg)
                                  (cdr arg)))
                          (format "%s : %s%s%s"
                                  (upcase (symbol-name (car arg)))
                                  (if (stringp (nth 1 arg))
                                      (nth 1 arg)
                                    "No documentation")
                                  (if (plist-get arg-options :default)
                                      (format " (default = %s)"
                                              (plist-get arg-options :default))
                                    "")
                                  (if (plist-get arg-options :validate)
                                      (format " (valid = %s)"
                                              (plist-get arg-options :validate))
                                    "")))
                         ;; this is a standalone symbol
                         (t
                          (format "%s : No documentation"
                                  (upcase (symbol-name arg))))))
               "\n")
              "\n"))
         ;; These are the args to go in the function definition
         (newargs (cl-loop for arg in args
                           append
                           (cond
                            ((listp arg)
                             (cond
                              ((plist-get (cddr arg) :default)
                               `(&optional ,(car arg)))
                              ((member  :rest arg)
                               `(&rest ,(car arg)))
                              (t
                               (list (car arg)))))
                            (t
                             (list arg)))))
         ;; This is the code to set default values
         (defaults
           (delq nil
                 (cl-loop for arg in args
                          collect
                          (when
                              (and
                               (listp arg)
                               (plist-get (cddr arg) :default))
                            `(when (null ,(car arg))
                               (setq ,(car arg)
                                     ,(plist-get (cddr arg) :default)))))))
         ;; This is the code to validate arguments
         (validate
          (delq nil
                (cl-loop for i from 0 for arg in args
                         collect
                         (when
                             (and
                              (listp arg)
                              (plist-get
                               (delq :rest (cddr arg))
                               :validate))
                           `(unless (funcall
                                     ',(plist-get
                                        (delq :rest (cddr arg))
                                        :validate)
                                     ,(car arg))
                              (error "In (%s %s) Expected %s to pass %S. Got %S"
                                     ,(symbol-name fname) ,(format "%s" newargs)
                                     ,(symbol-name (car arg))
                                     ',(plist-get (delq :rest (cddr arg)) :validate)
                                     ,(car arg)))))))
         (f `(defun ,fname (,@newargs)
               ,(or ds "No docstring defined ;(."))))
    (when defaults
      (setq f (append f `((progn ,@defaults)))))
    (when validate
      (setq f (append f `((progn ,@validate)))))
    (setq f (append f `,@body))))


(defmacro self-byte-compile (&optional arg)
  "Auto byte compile current loading file. If AEG, force update compiled file."
  (cond ((featurep 'wizard 'install)
         `(eval-when 'eval
            (byte-compile-file ,load-file-name)))
        ((featurep 'inferno 'autocomp)
         `(eval-when 'eval
            (byte-recompile-file ,load-file-name ,arg 0)))))

;; Enable auto byte compile.
(self-byte-compile t)

(provide 'core-lib)
;;; core-lib.el ends here

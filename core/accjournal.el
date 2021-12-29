;;; accjournal.el --- double entry journal accounting -*- lexical-binding:t -*-

;; Copyright 2006, 2007, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017 Kevin Ryde

;; Author: Kevin Ryde <user42_kevin@yahoo.com.au>
;; Version: 9
;; Keywords: data, accounting
;; URL: http://user42.tuxfamily.org/accjournal/index.html

;; accjournal.el is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; accjournal.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General
;; Public License for more details.
;;
;; You can get a copy of the GNU General Public License online at
;; <http://www.gnu.org/licenses/>.


;;; Commentary:

;; This is a minimal double-entry accounting system using a journal of
;; transactions in a text file.  The file format is a bit Spartan, but it's
;; quite effective for keeping track of personal finances.
;;
;; Each line in the file is a transaction debiting one account and crediting
;; another.
;;
;;    30 Apr 10  400.00   Salary 09/10 -> Bank
;;
;; Accumulated results are display by account and a summary of open account
;; balances at the end.
;;
;; "Double-entry" means amounts are only ever moved from one account to
;; another.  So if you record salary as "credit bank, debit taxable income"
;; then you can be confident all amounts deposited in the bank have gone to
;; taxable income (or wherever).  It's possible to put a wrong figure, but
;; an amount can't be dropped on the floor.
;;
;; See the `accjournal-mode' docstring below for more (including other
;; similar Lisp packages).  See the examples directory in the source .tar
;; for some complete input files.

;;; Emacsen:

;; Designed for Emacs 21 and up, works in XEmacs 21.
;; Doesn't work in Emacs 20 due to regexp "?:" and "+?" bits.

;;; Install:

;; To make M-x accjournal-mode available put accjournal.el in one of your
;; `load-path' directories and the following in your .emacs
;;
;;     (autoload 'accjournal-mode "accjournal" nil t)
;;
;; There's an autoload cookie for this if you know how to use
;; `update-file-autoloads' and friends.
;;
;; Byte-compiling is recommended for speed of report processing.

;;; History:

;; Version 1 - the first version
;; Version 2 - avoid `loop' losing last account
;; Version 3 - new include directive
;; Version 4 - better window position preserving
;; Version 5 - use define-derived-mode for hooks running conventions
;; Version 6 - correction to auto-fill-inhibit-regexp
;; Version 7 - catch integer overflows
;; Version 8 - ignore trailing whitespace for to-account name
;; Version 9 - hash table for completion construction

;;; Code:

(eval-when-compile
  (unless (and (fboundp 'dolist)
               (fboundp 'push)
               (fboundp 'declare))
    (require 'cl))) ;; for macros in emacs20 (though it would need puthash)

;; quieten byte compiler
(defvar align-mode-rules-list)    ;; in align.el
(defvar align-open-comment-modes) ;; in align.el
(defvar font-lock-defaults)       ;; not pre-loaded in xemacs21


;;-----------------------------------------------------------------------------
;; customizations

;;;###autoload
(defgroup accjournal nil "AccJournal."
  :group  'applications
  :link   '(url-link :tag "accjournal.el home page"
                     "http://user42.tuxfamily.org/accjournal/index.html"))

(defcustom accjournal-mandatory-open nil
  "If non-nil then a \"!\" to open each account is mandatory.
If nil (the default) then the first use of an account opens it.
If t then all accounts must be opened with a \"!\" marker on each
first use."
  :type  'boolean
  :group 'accjournal)
;;;###autoload
(put 'accjournal-mandatory-open 'safe-local-variable 'booleanp)


;;-----------------------------------------------------------------------------
;; emacs22 new stuff

(if (eval-when-compile (fboundp 'complete-with-action))
    ;; emacs22 (and in emacs23 recognising the "boundaries" thing)
    (eval-and-compile
      (defalias 'accjournal--complete-with-action
        'complete-with-action))

  ;; emacs21,xemacs21
  (defun accjournal--complete-with-action (action table string pred)
    (cond ((null action)
           (try-completion string table pred))
          ((eq action t)
           (all-completions string table pred))
          (t
           (eq t (try-completion string table pred))))))


;;-----------------------------------------------------------------------------
;; vaguely generic
(defconst accjournal-Mmm-hash
  (let ((h (make-hash-table :test 'equal))
        (i 1))
    (dolist (str '("Jan" "Feb" "Mar" "Apr" "May" "Jun"
                   "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))
      (puthash str i h)
      (puthash (upcase str) i h)
      (puthash (downcase str) i h)
      (setq i (1+ i)))
    h)
  "An internal part of accjournal.
A hash table mapping month name like \"Oct\" to number like 10.
Capitalizations \"Oct\", \"OCT\" and \"oct\" are all in the
table.")

(defsubst accjournal-Mmm-to-month (str)
  "An internal part of accjournal.el.
Convert STR like \"Oct\" to a number like 10.
If STR is not a valid month then throw an error.  Capitalizations
\"Oct\", \"OCT\" and \"oct\" are recognised per
`accjournal-Mmm-hash'."
  (or (gethash str accjournal-Mmm-hash)
      (error "Unrecognised month: %S" str)))

(defvar accjournal-year-base
  (- (nth 5 (decode-time)) ;; current year, like 2010
     60)
  "Base year for 2-digit year windowing.
A 2-digit year like 85 is taken to be between
accjournal-year-base and accjournal-year-base + 99.  For example
if accjournal-year-base is 1960 then 2-digit years are taken to
be from 1960 to 2059.  The default is today-60.

\(4-digit years can be used to avoid ambiguity if this type of
windowing doesn't suit.)")
;;;###autoload
(put 'accjournal-year-base 'safe-local-variable 'integerp)

(defun accjournal-year (str)
  "An internal part of accjournal.el.
Convert STR like \"95\" or \"1995\" to integer 1995.
2-digit years are expanded to the nearest +40/-60 years as per
`accjournal-year-base'."
  (let ((year (string-to-number str)))
    (if (>= year 1000)
        year
      (+ accjournal-year-base
         (mod (- year accjournal-year-base) 100)))))

(defun accjournal-date-to-N (str)
  "An internal part of accjournal.el.
Convert STR like \" 1 Apr 10\" or \"2010-04-01\" to integer 20100401.
2-digit years are interpreted per `accjournal-year'."
  (cond ((string-match "\\` ?\\([0-9][0-9]?\\) +\\([A-Z][a-z][a-z]\\) +\\([0-9][0-9]\\([0-9][0-9]\\)?\\)\\'" str)
         (+ (string-to-number (match-string 1 str))
            (* 100 (accjournal-Mmm-to-month (match-string 2 str)))
            (* 10000 (accjournal-year (match-string 3 str)))))

        ((string-match "\\`\\([0-9][0-9][0-9][0-9]\\)-\\([0-9][0-9]\\)-\\([0-9][0-9]\\)\\'" str)
         (string-to-number (concat (match-string 1 str)
                                   (match-string 2 str)
                                   (match-string 3 str))))

        (t
         (error "Unrecognised date: %S" str))))

(eval-when-compile
  (defmacro accjournal-push-mark-on-error (&rest body)
    "An internal part of accjournal.
This macro doesn't exist when running byte compiled.

Run BODY forms with point saved and a `push-mark' on error.
Point is saved and if the BODY forms run to completion then point
is restored.  If BODY throws an error then point if left at the
offending position and `push-mark' used to put the mark at the
original position.  There's no return value."
    (declare (debug t))
    `(let ((accjournal-old-point  (point))
           (accjournal-old-buffer (current-buffer)))
       (condition-case err
           (progn ,@body)

         (error
          (if (buffer-live-p accjournal-old-buffer)
              (with-current-buffer accjournal-old-buffer
                (push-mark accjournal-old-point)))
          (signal (car err) (cdr err))))

       (if (buffer-live-p accjournal-old-buffer)
           (with-current-buffer accjournal-old-buffer
             (goto-char accjournal-old-point))))))

(eval-when-compile
  (defmacro accjournal-with-file (filename &rest body)
    ;; checkdoc-params: (filename body)
    "An internal part of accjournal.
This macro doesn't exist when running byte compiled.

Evaluate BODY in a temporary `find-file' of FILENAME."

    (declare (indent 1) (debug t))
    `(let* ((accjournal-with-file--oldbuf  (current-buffer))
            (accjournal-with-file--filename ,filename)
            (accjournal-with-file--existing (find-buffer-visiting
                                             accjournal-with-file--filename)))
       (switch-to-buffer
        (or accjournal-with-file--existing
            (progn
              (unless (file-exists-p accjournal-with-file--filename)
                (error "No such file: %s" accjournal-with-file--filename))
              (find-file-noselect accjournal-with-file--filename))))
       ,@body
       (if (not accjournal-with-file--existing)
           (kill-buffer accjournal-with-file--existing))
       (switch-to-buffer accjournal-with-file--oldbuf))))

;;-----------------------------------------------------------------------------
;; arithmetic

(defun accjournal-add (x y)
  "An internal part of accjournal.el.
Return the sum (+ X Y), but throw an error for integer overflow."
  (let ((sum (+ x y)))
    (if (if (< y 0) (> sum x) (< sum x)) ;; wraparound
        (error "Integer overflow (maximum %s)" most-positive-fixnum))
    sum))

(defun accjournal-sub (x y)
  "An internal part of accjournal.el.
Return subtraction (- X Y), but throw an error for integer
overflow."
  (let ((sum (- x y)))
    (if (if (< y 0) (< sum x) (> sum x)) ;; wraparound
        (error "Integer overflow (maximum %s)" most-positive-fixnum))
    sum))


;;-----------------------------------------------------------------------------
;; output buffer position

(eval-when-compile
  (defmacro accjournal-save-window-start (buffer &rest body)
    ;; checkdoc-params: (buffer body)
    "An internal part of accjournal.
This macro doesn't exist when running byte compiled.

Run BODY with window line/columns saved in any showing BUFFER."
    (declare (debug t))
    `(let ((accjournal-save-window-start--buffer ,buffer)
           accjournal-save-window-start--wlist)
       (with-current-buffer accjournal-save-window-start--buffer
         (dolist (frame (frame-list))
           (dolist (window (window-list frame))
             (when (eq accjournal-save-window-start--buffer
                       (window-buffer window))
               (let* ((window-point (window-point window))
                      (window-start (window-start window))
                      (point-line   (count-lines (point-min) window-point))
                      (point-column (save-excursion
                                      (goto-char window-point)
                                      (current-column)))
                      (start-line   (count-lines (point-min) window-start))
                      (start-column (save-excursion
                                      (goto-char window-start)
                                      (current-column)))
                      account-name
                      account-start
                      account-point)
                 (save-excursion
                   (forward-line 1)
                   (when (search-backward accjournal-separator nil t)
                     (forward-line 1)
                     (setq account-name (buffer-substring (point)
                                                          (line-end-position)))
                     (setq account-start (count-lines window-start (point)))
                     (if (> (window-start) (point))
                         (setq account-start (- account-start)))
                     (setq account-point (count-lines window-point (point)))
                     (if (> (window-point) (point))
                         (setq account-point (- account-point)))))
                 ;; (message "stl=%S %S,%S,%S %S %S"
                 ;;          start-line
                 ;;          (window-start)
                 ;;          (window-point)
                 ;;          (point)
                 ;;          account-start account-point)

                 (push (list window
                             point-line
                             point-column
                             start-line
                             start-column
                             account-name
                             account-start
                             account-point)
                       accjournal-save-window-start--wlist))))))

       (progn ,@body)

       (with-current-buffer accjournal-save-window-start--buffer
         (save-excursion
           (dolist (elem accjournal-save-window-start--wlist)
             (let ((window        (nth 0 elem))
                   (point-line    (nth 1 elem))
                   (point-column  (nth 2 elem))
                   (start-line    (nth 3 elem))
                   (start-column  (nth 4 elem))
                   (account-name  (nth 5 elem))
                   (account-start (nth 6 elem))
                   (account-point (nth 7 elem)))

               (cond ((and account-name
                           (search-forward (concat accjournal-separator
                                                   account-name) nil t))
                      (save-excursion
                        (forward-line account-start)
                        (move-to-column point-column)
                        (set-window-start window (point)))

                      (forward-line account-point)
                      (move-to-column start-column)
                      (set-window-point window (point)))
                     (t
                      (goto-char (point-min))
                      (forward-line point-line)
                      (move-to-column point-column)
                      (set-window-start window (point))

                      (goto-char (point-min))
                      (forward-line start-line)
                      (move-to-column start-column)
                      (set-window-point window (point)))))))))))


;;-----------------------------------------------------------------------------

(defconst accjournal-buffer-name "*accjournal*"
  "Name of the accjournal report output buffer.")

(defvar accjournal-accounts-alist nil
  "An internal part of accjournal.
An alist of account name to acc record.
Accounts are pushed as they're opened and at the end reversed so
the list is in the order opened.  This is only used during report
crunching.")

(defvar accjournal-accounts-hash (make-hash-table :test 'equal)
  "An internal part of accjournal.
A hash table of account names to acc records.")

(defvar accjournal-decimal-point nil
  "An internal part of accjournal.
The decimal point string in accjournal processing.
This is set from the input and only used during report
crunching.")

(defvar accjournal-decimal-places nil
  "An internal part of accjournal.
The number of decimal places on values in accjournal processing.
This is set from the input and only used during report
crunching.")

(defconst accjournal-amount-width 10
  "An internal part of accjournal.
Width to display a dollar amount.
The default 10 allows amounts less than $1 million with 2 decimal
points and either positive or negative.

This only affects the output.  Amounts in the input file can be
have as many spaces as needed or desired after the date.

It may be better to drop this and instead track the biggest value
posted or accumulated, maybe in each account separately or maybe
globally.")

(defvar accjournal-amount-format nil
  "An internal part of accjournal.
A `format' string for money amounts.
This is made from `accjournal-amount-width' and only used during
report crunching.")

(defvar accjournal-post-format nil
  "An internal part of accjournal.
A `format' string for posting entries to accounts.
This is made from `accjournal-amount-format' and only used during
report crunching.")

(eval-and-compile ;; eval at compile-time too for use in align-rules below

  (defconst accjournal-date-regexp
    " ?[0-9]+ [A-Za-z][A-Za-z][A-Za-z] [0-9][0-9]\\(?:[0-9][0-9]\\)?\
\\|[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]"
    "An internal part of accjournal.
Regexp matching a date like \"2010-05-17\" or \" 9 May 2010\".
This is used for matching accjournal lines.  It's a tight match
so the `thing-at-point' handler and `align' rules will only act
on relevant lines.  There are no \\( \\) groups.")

  (defconst accjournal-amount-regexp
    (concat "-?"
            "\\(?:"  "[0-9]+\\(?:[.,][0-9]*\\)?" ;; digit before, opt dec point
            "\\|"    "[.,][0-9]+"  ;; no digits before, mandatory dec point
            "\\)")
    "An internal part of accjournal.
Regexp matching an amount like 1.23 or -0,25.
The decimal point can be either . or , and there must be at least
one digit.  There are no \\( \\) groups."))

(defun accjournal-cents-to-string (n)
  "An internal part of accjournal.
Convert cents N like 12345 to a string like \"123.45\".
This is used only during report crunching with
`accjournal-decimal-point' and `accjournal-decimal-places' giving
the formatting."

  (if (zerop accjournal-decimal-places)
      (number-to-string n)

    (let ((str (if (> (length (number-to-string (abs n)))
                      accjournal-decimal-places)
                   (number-to-string n)
                 ;; pad with zeros to have the desired decimal places
                 (format (format "%%0%dd" (+ accjournal-decimal-places
                                             (if (>= n 0) 1 2)))
                         n))))
      ;; insert decimal point
      (concat (substring str 0 (- accjournal-decimal-places))
              accjournal-decimal-point
              (substring str (- accjournal-decimal-places))))))

(defconst accjournal-line-regexp
  (eval-when-compile
    (concat "^\\(" accjournal-date-regexp "\\)"  ;; 1 date
            "[ \t]+"
            "\\("              ;; 2 amount
            "\\(-?[0-9]*\\)"   ;; 3 dollars
            "\\(?:\\([.,]\\)"  ;; 4 decimal point (optional)
            "\\([0-9]*\\)\\)?" ;; 5 cents (optional)
            "\\)"
            "[ \t]+"
            "\\(!\\)?"         ;; 6  from open
            "\\([^@\t\n]+?\\)" ;; 7  from name
            "\\(@\\)?"         ;; 8  from close
            "[ \t]+->[ \t]+"   ;; -> arrow
            "\\(!\\)?"         ;; 9  to open
            "\\([^@\t\n]+?\\)" ;; 10 to name
            "\\(@\\)?"         ;; 11 to close
            "[ \t]*"
            "\\(?:|[ \t]*"     ;; | note separator
            "\\(.*\\)\\)?$"))  ;; 12 note (optional)
  "An internal part of accjournal.
Regexp matching an accjournal input line.
There are many \\=\\( \\=\\) groups for the parts of the line.")

(defconst accjournal-include-regexp
  ;; (match-string 2) filename
  ;; "" quotes optional
  "include[ \t]\\(\"?\\)\\(.*\\)\\1"
  "An internal part of accjournal.
Regexp matching an accjournal include line.")


;;-----------------------------------------------------------------------------
;; `thing-at-point' setups
;;
;; (thing-at-point 'accjournal-account) returns an account name at point.

(defun accjournal-beginning-of-account ()
  "An internal part of accjournal.
Move point to the start of an account name."

  (if (re-search-backward
       (concat "\\(\\(" accjournal-date-regexp
               "\\)[ \t]+" accjournal-amount-regexp "[ \t]*!?"
               "\\|->[ \t]*!?"
               "\\|" comment-start-skip
               "\\)")
       nil t)
      (goto-char (match-end 0)))
  (skip-chars-forward " \t"))

(put 'accjournal-account 'beginning-op
     'accjournal-beginning-of-account)

(put 'accjournal-account 'end-op
     (lambda ()
       (if (re-search-forward "\\(->\\||\\|\n\\|$\\)" nil t)
           (goto-char (match-beginning 0)))
       (skip-chars-backward " \t@")))


;;-----------------------------------------------------------------------------
;; account name user input

(defvar accjournal-account-history nil
  "Interactive history list of account names.")

(defun accjournal-completion-accounts ()
  "An internal part of accjournal.el.
Return an alist of the account names in the current buffer."
  (let ((h (make-hash-table :test 'equal)))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (when (looking-at accjournal-line-regexp)
          (puthash (match-string 7)  nil h)
          (puthash (match-string 10) nil h))
        (forward-line)))
    (mapcar 'list (sort (hash-table-keys h) 'string-lessp))))

(defun accjournal-completing-read-account (&optional default)
  "An internal part of accjournal.
Read an account name using `completing-read'.
DEFAULT (a string) is offered as the default return.
Completions are available from account names in the current
buffer."
  (if (equal "" default) ;; allow for empty from thing-at-point
      (setq default nil))
  (let ((completion-ignore-case t)
        (accjournal-buffer (current-buffer))
        accjournal-completion-accounts
        accjournal-completion-accounts-done)
    (completing-read (if default
                         (format "Account (%s): " default)
                       "Account: ")
                     (lambda (str pred action)
                       (unless accjournal-completion-accounts-done
                         (setq accjournal-completion-accounts
                               (with-current-buffer accjournal-buffer
                                 (accjournal-completion-accounts)))
                         (setq accjournal-completion-accounts-done t))
                       (accjournal--complete-with-action
                        action accjournal-completion-accounts str pred))
                     nil  ;; pred
                     nil  ;; require-match
                     nil  ;; initial-input
                     'accjournal-account-history
                     default)))

;;-----------------------------------------------------------------------------

(defun accjournal-acc-create ()
  "An internal part of accjournal.el.
Return a new account object."
  (vector 0     ;; balance, integer cents
          nil   ;; translist, elements (DATEN ...)
          nil   ;; closedp, boolean
          ))
(defconst accjournal-acc-balance   0)
(defconst accjournal-acc-translist 1)
(defconst accjournal-acc-closedp   2)

(defun accjournal-open (name)
  "An internal part of accjournal.el.
Open an account NAME and return the new account object for it.
An error is thrown if NAME already exists."

  (if (gethash name accjournal-accounts-hash)
      (error "Account already exists: %S" name))
  (let ((account (accjournal-acc-create)))
    (setq accjournal-accounts-alist
          (push (cons name account) accjournal-accounts-alist))
    (puthash name account accjournal-accounts-hash)
    account))

(defun accjournal-account-find (name)
  "An internal part of accjournal.el.
Return an account object called NAME.

If NAME doesn't exist then it's opened automatically, or if
`accjournal-mandatory-open' says accounts must be explicitly
opened then it's an error if NAME doesn't already exist."
  (or (gethash name accjournal-accounts-hash)
      (progn
        (if accjournal-mandatory-open
            (error "Account not opened: %S" name))
        (accjournal-open name))))

(defun accjournal-account-find-open (name)
  "An internal part of accjournal.el.
Return an account object called NAME.
NAME must be already open."
  (let ((acc (accjournal-account-find name)))
    (if (aref acc accjournal-acc-closedp)
        (error "Account closed: %s" name))
    acc))

(defun accjournal-close (acc name)
  "An internal part of accjournal.el.
Close account object ACC.
The balance in ACC must be zero.  NAME is used in the error message."
  (let ((balance (aref acc accjournal-acc-balance)))
    (or (zerop balance)
        (error "Closing account with non-zero balance (%s): %s" balance name)))
  (aset acc accjournal-acc-closedp t))

(defun accjournal-make-alist-buf ()
  "An internal part of accjournal."

  (let ((skip-regexp (concat (and comment-start-skip ;; nil in text-mode
                                  (concat "\\(" comment-start-skip "\\)\\|"))
                             "[ \t]*$")))
    (accjournal-push-mark-on-error
     (goto-char (point-min))
     (while (not (eobp))
       (cond
        ((looking-at skip-regexp)
         ;; comment or blank line
         )

        ((looking-at accjournal-include-regexp)
         ;; recurse for an "include"
         (accjournal-with-file (match-string 2)
           (accjournal-make-alist-buf)))

        ((looking-at accjournal-line-regexp)
         (let ((date           (match-string 1))

               (amount-pos     (match-beginning  2))
               (amount         (match-string     2))
               (amount-dollars (match-string     3))
               (amount-decimal (match-string     4))
               (amount-cents   (or (match-string 5) ""))

               (from-pos       (or (match-beginning 6)
                                   (match-beginning 7)))
               (from-open      (match-beginning 6))
               (from-name      (match-string    7))
               (from-close     (match-beginning 8))

               (to-pos         (or (match-beginning 9)
                                   (match-beginning 10)))
               (to-open        (match-beginning 9))
               (to-name        (match-string 10))
               (to-close       (match-beginning 11))

               (note           (match-string 12))

               date-N from-acc to-acc)

           (goto-char (match-beginning 1))
           (setq date-N (accjournal-date-to-N date))

           (goto-char amount-pos)
           (setq accjournal-decimal-point amount-decimal)
           (or (equal amount-decimal accjournal-decimal-point)
               (error "Different decimal point %S" amount))

           (unless accjournal-decimal-places
             (setq accjournal-decimal-places (length amount-cents)))
           (unless (= (length amount-cents) accjournal-decimal-places)
             (error "Different decimal places %S" amount))
           (setq amount-cents (string-to-number (concat amount-dollars
                                                        amount-cents)))
           (if (floatp amount-cents)
               (error "Integer overflow (maximum %s)" most-positive-fixnum))

           (if from-open
               (progn
                 (goto-char from-pos)
                 (accjournal-open from-name)))
           (if to-open
               (progn
                 (goto-char to-pos)
                 (accjournal-open to-name)))

           (goto-char from-pos)
           (setq from-acc (accjournal-account-find-open from-name))
           (goto-char to-pos)
           (setq to-acc   (accjournal-account-find-open to-name))

           (setq note (if note (concat " | " note) ""))

           ;; from
           (aset from-acc accjournal-acc-balance
                 (accjournal-sub (aref from-acc accjournal-acc-balance)
                                 amount-cents))
           (aset from-acc accjournal-acc-translist  ;; push onto elem
                 (cons (list date-N
                             (- amount-cents)
                             date
                             amount  ;; debit
                             ""      ;; credit
                             nil     ;; balance
                             to-name
                             note)
                       (aref from-acc accjournal-acc-translist)))

           ;; to
           (aset to-acc accjournal-acc-balance
                 (accjournal-add (aref to-acc accjournal-acc-balance)
                                 amount-cents))
           (aset to-acc accjournal-acc-translist  ;; push onto elem
                 (cons (list date-N
                             amount-cents
                             date
                             ""     ;; debit
                             amount ;; credit
                             nil    ;; balance
                             from-name
                             note)
                       (aref to-acc accjournal-acc-translist)))

           (if from-close
               (progn
                 (goto-char from-pos)
                 (accjournal-close from-acc from-name)))
           (if to-close
               (progn
                 (goto-char to-pos)
                 (accjournal-close to-acc to-name)))))

        (t
         (error "Bad line")))
       (forward-line)))))

(defun accjournal-make-alist ()
  "An internal part of accjournal.el.
Make account data in `accjournal-alist'."

  (clrhash accjournal-accounts-hash)
  (setq accjournal-accounts-alist nil)
  (setq accjournal-decimal-point nil)
  (setq accjournal-decimal-places nil)
  (setq accjournal-amount-format
        (format "%%%ds" accjournal-amount-width))
  (setq accjournal-post-format
        (concat "%s " accjournal-amount-format
                " " accjournal-amount-format
                " " accjournal-amount-format
                "  %s%s\n"))

  (accjournal-make-alist-buf)
  (setq accjournal-accounts-alist (nreverse accjournal-accounts-alist))

  ;; sort accjournal-acc-translist by date and set the balance in each entry
  ;;
  (dolist (pair accjournal-accounts-alist)
    (let ((acc (cdr pair)))
      (aset acc accjournal-acc-translist
            (sort
             ;; nreverse so buffer order for equal dates
             (nreverse (aref acc accjournal-acc-translist))
             (lambda (x y)
               (< (car x) (car y)))))  ;; compare by date-N

      (let ((balance 0))
        (dolist (elem (aref acc accjournal-acc-translist))
          (setq balance (accjournal-add balance
                                        (cadr elem))) ;; amount-cents
          (setcar (nthcdr 5 elem)
                  (accjournal-cents-to-string balance)))))))

(defconst accjournal-separator
  "----------------------------------------------------------------------\n"
  "An internal part of accjournal.
The string used as a separator between accounts in the
*accjournal* output buffer.

This should be something long enough or distinctive enough that
it doesn't otherwise occur in the text, since the current
`accjournal-output-account-name' looks for this separator to find
the account name (in turn used to keep the window position the
same when re-running.")

(defun accjournal-make ()
  "An internal part of accjournal.el.
Make account output in `accjournal-buffer-name'."

  (accjournal-make-alist)
  (let ((filename buffer-file-name))
    (with-current-buffer (get-buffer-create accjournal-buffer-name)
      (setq buffer-read-only nil)
      (accjournal-save-window-start
       (current-buffer)

       (erase-buffer)
       (insert (format "AccJournal of %s\n\n" filename))

       (dolist (elem accjournal-accounts-alist)
         (let ((name (car elem))
               (acc  (cdr elem)))
           (insert accjournal-separator
                   name "\n"
                   "          " (format accjournal-amount-format "Debit")
                   " "          (format accjournal-amount-format "Credit")
                   " "          (format accjournal-amount-format "Balance")
                   "   From\n")

           (dolist (elem (aref acc accjournal-acc-translist))
             (insert (apply 'format accjournal-post-format (cddr elem))))

           (insert (if (aref acc accjournal-acc-closedp)
                       "[Closed]\n\n\n"
                     "[Open]\n\n\n"))))

       (insert "\n" accjournal-separator "Open Accounts:\n")
       (let ((open-format (format "%%%ds\t%%s\n" accjournal-amount-width)))
         (dolist (pair (sort (copy-sequence accjournal-accounts-alist)
                             (lambda (x y)
                               (string-lessp (car x) (car y)))))
           ;; accounts sorted by name
           (let ((name (car pair))
                 (acc  (cdr pair)))
             (unless (aref acc accjournal-acc-closedp)
               (insert (format open-format
                               (accjournal-cents-to-string
                                (aref acc accjournal-acc-balance))
                               name))))))

       (goto-char (point-min))
       (view-mode)))))

(defun accjournal-view ()
  "View full report of all account transactions and balances."
  (interactive)
  (accjournal-make)
  (switch-to-buffer accjournal-buffer-name))

(defun accjournal-output-account-name ()
  "An internal part of accjournal.
In a *accjournal* output buffer, return the name of the account
which is at point.  `accjournal-separator' separates each account
output and the first line is the account name."
  (let ((orig-point (point)))
    (forward-line 1)
    (if (search-backward accjournal-separator nil t)
        (progn
          (forward-line 1)
          (buffer-substring (point) (line-end-position)))
      (goto-char orig-point)
      nil)))

(defun accjournal-view-account (name)
  "View accumulated transactions for a given account.
Interactively the default account NAME is taken from point."
  (interactive (list (accjournal-completing-read-account
                      (thing-at-point 'accjournal-account))))
  (accjournal-make)
  (save-selected-window
    (switch-to-buffer-other-window accjournal-buffer-name)
    (with-current-buffer accjournal-buffer-name
      (cond ((equal name (accjournal-output-account-name))
             ;; already displaying NAME
             )
            ((progn
               (goto-char (point-min))
               (search-forward (concat accjournal-separator name "\n") nil t))
             ;; found NAME
            (forward-line -1)
            (set-window-start nil (point)))
            (t
             (message "Oops, account not found: %S" name))))))


;;-----------------------------------------------------------------------------
;; align.el

(defconst accjournal-align-rules
  (eval-when-compile
    (let ((fields-regexp
           (concat "^\\(" accjournal-date-regexp "\\)"
                   "\\([ \t]*" accjournal-amount-regexp "\\)" ;; 1
                   "\\([ \t]*\\) .+?" ;; from    2
                   "\\([ \t]*\\)->"   ;;         3
                   "\\([ \t]*\\)"     ;; to      4
                   )))

      `((accjournal-amount
         (regexp   . ,fields-regexp)
         (justify  . t)  ;; right-justified
         (group    . 2)
         (spacing  . 2)  ;; at least two spaces between date and amount
         (tab-stop . nil)
         (separate . entire)) ;; all numbers aligned

        ;; "from", "arrow", "to"
        (accjournal-fields
         (regexp   . ,fields-regexp)
         (group    . (3 4 5)))

        ;; "note" is only on some lines, must use a separate rule as cannot
        ;; have an optional matched group in an align regexp.  (Could use
        ;; the same regexp with a `valid-if' checking for a match though.)
        ;;
        (accjournal-note
         (regexp   . ,(concat "^\\(?:" accjournal-date-regexp "\\)"
                              "[ \t]+" accjournal-amount-regexp
                              "[ \t]+.+?"   ;; from
                              "[ \t]+->"    ;;
                              "[ \t]+.+?"   ;; to
                              "\\([ \t]*\\)?|"))
         (group    . 1)
         (spacing  . 2))))) ;; at least two spaces before "|"

  "An internal part of accjournal.
The `align-mode-rules-list' setting for an `accjournal-mode'
buffer to align accjournal fields.")

(eval-after-load "align"
  '(add-to-list 'align-open-comment-modes 'accjournal-mode))


;;-----------------------------------------------------------------------------
;; font-lock

(defun accjournal-font-lock-warning (limit)
  "Search for a bad accjournal line up to LIMIT.
This function is designed for use as a `font-lock-keywords'
matcher.

Return nil if there's no bad lines.  Return non-nil if there's a
bad line, setting match data 0 to encompass it and leaving point
at the start of the following line (ready for a further search)."

  (beginning-of-line)
  (let (ret)  ;; default return nil if reach limit
    (while (and (not ret)
                (< (point) limit))
      (if (or (looking-at "[ \t]*$")               ;; empty line good
              (looking-at comment-start-skip)      ;; comment lines good
              (looking-at accjournal-line-regexp)
              (looking-at accjournal-include-regexp))
          (forward-line 1)  ;; good line, continue
        ;; bad line
        (setq ret t)
        (looking-at ".*\n?") ;; whole line bad, including \n if present
        (goto-char (match-end 0))))  ;; next search begin at next line
    ret))

(defvar accjournal-mode-font-lock-defaults
  '(;; font-lock-keywords list
    ((accjournal-font-lock-warning . font-lock-warning-face))

    ;; KEYWORDS-ONLY default nil, so syntactic comment fonting
    ))


;;-----------------------------------------------------------------------------
;; auto-complete.el tie-in

(defun accjournal-accounts-at-point ()
  "An internal part of accjournal.el.
Return a list of open account names at point."

  (save-excursion
    (let ((orig-point (line-end-position))
          accounts       ;; list of account name strings
          (opened-after-hash
           (make-hash-table  ;; account names opened after point
            :test 'equal)))

      ;; account names on the current line not returned, by reckoning them
      ;; as if `opened-after-hash'
      (beginning-of-line)
      (when (looking-at accjournal-line-regexp)
        (puthash (match-string 7)  t opened-after-hash)    ;; from-name
        (puthash (match-string 10) t opened-after-hash))    ;; to-name

      (goto-char (point-min))
      (while (re-search-forward accjournal-line-regexp nil t)
        (dolist (n '(6 9))
          (let ((open  (match-beginning n))                 ;; bool
                (name  (match-string-no-properties (1+ n))) ;; str
                (close (match-beginning (+ 2 n))))          ;; bool
            (and open
                 (> (point) orig-point)
                 (puthash name t opened-after-hash))
            (and (not (gethash name opened-after-hash))
                 (not (member name accounts))
                 (push name accounts))
            (and close
                 (< (point) orig-point)
                 (setq accounts (delete name accounts))))))

      (nreverse accounts))))

(defun accjournal-beginning-of-account-position ()
  "An internal part of accjournal.el.
Return the position of the start of an account name at point."
  (save-excursion
    (accjournal-beginning-of-account)
    (point)))

(defvar accjournal-ac-source-accounts
  '((candidates . accjournal-accounts-at-point)
    (prefix     . accjournal-beginning-of-account-position))
  "auto-complete.el source for accjournal account names.
This source completes account names by open accounts at point.

Closed accounts are not completed.  If an account is closed by
\"@\" somewhere before point then it's not open.  If an account
is opened by \"!\" somewhere after point then it's not open at
point.  The two accounts on the current line are excluded.  One
is the partial name at point being completed, the other would
make a duplicate \"foo -> foo\".

There's no particularly easy config for this source yet.  The
suggestion is from the mode hook set `ac-sources' to
`accjournal-ac-source-accounts' and any other desired sources.

    (add-hook 'accjournal-mode-hook
              (lambda ()
                (set (make-local-variable 'ac-sources)
                     '(accjournal-ac-source-accounts))))

Then `M-x auto-complete-mode' for temporary use in the usual way,
or have it permanently by adding to `ac-modes' as described in
the auto-complete.el manual under \"Enable auto-complete-mode
automatically for specific modes\".

    (eval-after-load \"auto-complete\"
      '(add-to-list 'ac-modes 'accjournal-mode))")


;;-----------------------------------------------------------------------------
;; mode

(defvar accjournal-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m "\C-c\C-f"  'accjournal-view-account)
    (define-key m [f5]        'accjournal-view)
    (define-key m "\C-c\C-c"  'comment-region)
    m)
  "Keymap for `accjournal-mode'.")

(defvar accjournal-mode-syntax-table
  (let ((s (make-syntax-table)))
    (modify-syntax-entry ?#  "<"  s)
    (modify-syntax-entry ?\n "> " s)
    s)
  "Syntax table for `accjournal-mode'.")

;;;###autoload
(define-derived-mode accjournal-mode fundamental-mode "AccJournal"
  "Major mode for accounting journal.
This is a minimal double-entry accounting system.  It's good for
recording money movements for personal taxes etc or a little
analysis of income versus expenses.

There's no hierarchy of accounts, and there's no automatic
transactions so every amount has to be entered individually.
Manual entry is a little tedious but prevents a mistake in one
place quietly propagating.

\\{accjournal-mode-map}
`accjournal-mode-hook' is run after initializations are complete.


File Format
-----------

There's no specific filename extension or `auto-mode-alist' for
accjournal yet, so use any name and put it into `accjournal-mode'
with the following first-line cookie (see Info
node `(emacs)Specifying File Variables').  Buffers must be in
`accjournal-mode' to process.

    # -*- mode: accjournal -*-

Each line in the file is a transaction moving money from one
account to another.  For example withdrawing cash from the bank
might move from account \"Bank\" to account \"Wallet\",

# some comment as extra reminder
12 May 10  100.00  Bank -> Wallet   | cash machine

Comment lines begin with a \"#\".  Account names can include
spaces, digits and non-ASCII.  The \"|\" part is an optional
free-form note.  Set a file coding system in the usual Emacs ways
for non-ASCII \(see Info node `(emacs)International').

Dates can be any of the following forms.  Transactions don't have
to be in date order in the file, so you group things logically
and they're sorted in the output.

    12 May 10
    12 May 2010
    2010-05-12

The decimal point in values can be either \".\" or \",\".
Decimal places are arbitrary and can even be none to work in
whole dollars or a currency like Yen.  But any decimal places
must be the same throughout the file.

All calculations are made in Emacs integers.  An error is thrown
for integer overflow.  Emacs 24 on a 32-bit system uses signed
30-bit integers which means 8 digits or about +/- $5 million for
amounts and balance.  For more try building --with-wide-int for
62-bit integers even on a 32-bit system.

Columns don't have to line up, the \"->\" and \"|\" separators
are enough.  It's usually more readable to mostly align but you
might want long lines tigher.  There's rules for `M-x align' to
align groups of lines or a whole file (see align.el).

Bad lines provoke an error from `accjournal-view'.  There's also
`font-lock-mode' setups to show bad lines in warning face
\(`font-lock-warning-face') while editing.


Output
------

`accjournal-view' (\\[accjournal-view]) processes the input file
into an *accjournal* output buffer with the following for each
account,

    Bank
                  Debit    Credit    Balance
    ...                              700.00
    12 May 10    100.00              600.00  Wallet | cash machine
    [Open]

The balance shows what you've accumulated in the account and the
Debit/Credit/Balance columns can be checked against a bank
statement etc.

To see a particular account put point on its name and use
`accjournal-view-account' \(\\[accjournal-view-account]).  This is good when entering
transactions on a particular account and you want to see
how the balance is going.

What you record is a matter of what you want to verify or account
for, such as assets, taxable income, expenditures, etc.  The
level of detail is purely what's desired.  Tracking cash in and
out of your wallet is probably too much, but you might put all
cash withdrawals to a \"Personal Expenses\" account, or perhaps
an account for each year like \"Personal 2012\", or just a
\"Non-Deductible\" to separate them from tax deductibles.


Opening and Closing
-------------------

Accounts are automatically opened when first used, or you can
write a \"!\" before the name to explicitly show that.  For
example the following is a new personal loan going into your bank
account, and repayments back to the loan,

12 May 10  3500.00  !Pers Loan -> Bank       | new loan
# then later ...
30 Jun 10   150.00  Bank       -> Pers Loan  | repayment
30 Jul 10   150.00  Bank       -> Pers Loan  | repayment

The `accjournal-mandatory-open' variable can demand a \"!\" for
each new account.  This helps prevent a typo becoming a new
account but can be a little tedious at first so its default is
nil.  In all cases \"!\" means the account must not already exist
and therefore when used it should be on the first appearance of
the account.

Accounts are closed by writing an \"@\" after the name on its
last use.  The balance must be zero.

30 May 15   150.00  Bank -> Pers Loan@  | last repayment

Closing an account with a non-zero balance gives an error from
the view commands.  Usually the easiest way to fix this is remove
the \"@\" and re-run the view to see what you have left or which
transactions haven't added up.

After an account is closed it cannot be re-opened.  But of course
you can remove the \"@\" to use it further.


Initial Balances
----------------

There's nothing special for creating initial balances.  Use a
\"zInitial\" account or some such name and transfer from it to
begin accounts.

1 Jul 10   1234.00  zInitial -> !Bank	      | initial balance
1 Jul 10  -5100.00  zInitial -> !Credit Card  | initial balance

Making your Bank etc positive for credit balance and negative
when a debt is the easiest way to check it against bank
statements.  This will determine the sign positive or negative of
everything else too.

If you use positive for credit balance then accounts for income
sources like employer salary are negative because you transfer
\"from employer, to bank\".

An accountant will tell you assets increase on the debit side.
You can do that if you want.  AccJournal doesn't care what's
positive or what's negative, it just adds up.


Final Balances
--------------

An accumulation account like taxable income for a given year can
be sunk back to \"zInitial\" before closing.

1 Jul 10  15000.00   09/10 taxable@ -> zInitial  | sink

Or keep a zSink account or similar for such things.  The \"z\" in
the name is a suggestion to keep dummy accounts at the end of the
Open Accounts list in `accjournal-view'.  Use \"aa\" or similar
if you prefer them at the start.

If you sink back to zInitial then it changes from initial net
value to a new value inclusive of the years income (etc).  It's
not a \"net worth\" because assets are only held at cost price.
You could add explicit asset revaluation transactions if you
wanted.  If you're a share trader then annual mark-to-market
might be mandatory.  But generally there's other better software
for portfolio valuations, budgeting, etc, which would calculate a
net worth.


Sub-Files
---------

An \"include\" directive reads another file.

    include \"prev-year.txt\"

Each sub-file should contain a mode line like

    # -*- mode: accjournal -*-

\(see Info node `Specifying File Variables'), since this ensures
the right syntax and comment setups for `accjournal-view'
processing.

The `accjournal-mandatory-open' can be set by Emacs \"Local
Variables\" in the usual way.  If using multiple files then
remember to have it in each sub-file.

All sub-files are crunched in full and the contents all shown in
one big output.  Sub-files just help keep past year data etc
separate.  It may be worth only archiving the very oldest
transactions, so as not to have to search too many files to find
for instance the \"buy\" of an only moderately old asset.

For multiple past files it's convenient to chain them, so that
say tax2010.txt starts with include \"tax2009.txt\" and that file
in turn starts with include \"tax2008.txt\", etc.  This way a
past file can be viewed with `accjournal-view'.  If you have
instead a single single top-level master file then the view only
works from there.

Include lines are recognised by ffap-include-start.el which can
make `M-x ffap' work with point at the start of the line as well
as on the filename proper.


Side Calculations
-----------------

If you have an amount which is an aggregate of a few things then
a temporary account can record the breakdown and ensure it adds
up as expected.  Suppose you buy shares in FooCo and BarCo and
pay by a single cheque,

30 May 10   7000.00  Bank	     -> !Shares 30may10 | cheque
30 May 10   3000.00  Shares 30may10  -> FooCo		| buy
30 May 10   4000.00  Shares 30may10@ -> BarCo		| buy

This way \"Bank\" has a single 7000.00 amount the same as your
bank statement but you have a record of the breakdown of the
purchases and know that it adds up.

If settlement is a few days after the buy date then it could look
like the following, with the assets bought first from a temporary
pending settlement account which is paid for later.

30 May 10   3000.00  !Shares 30may10 -> FooCo		| buy
30 May 10   4000.00  Shares 30may10  -> BarCo		| buy
 5 Apr 10   7000.00  Bank	     -> Shares 30may10@	| settle

Each such little account must have a different name.  Use a date
or cheque number or similar to make it clear what is being split.
Because it's all just a text file you can always search and
replace to change the naming style later.


Capital Gains
-------------

A good use for accjournal is to track the cost base of assets
subject to capital gains tax.  A buy is a transfer of money from
your bank account (or wherever) to the asset account.  Further
expenditure on it likewise from the bank to the asset, increasing
its cost base.  Tax deductible depreciation goes out of the asset
to tax deductions of a given year, reducing the cost base.
A final sale is a transfer from the asset to your bank account
and the balance left is a gain or loss.  For example an
investment house,

 1 Jul 85   20000.00  Bank	-> !InvHouse	 | buy
30 May 86    5000.00  Bank	-> InvHouse	 | spent on renovation
30 Jun 86     800.00  InvHouse	-> 85/86 deducts | depreciation claimed

30 Nov 86   27000.00  InvHouse	-> Bank          | sell
30 Nov 86    2800.00  86/87 cap gains -> InvHouse@

A net gain leaves a negative amount in the asset account because
more money came out than went in.  A net loss conversely leaves a
positive since more went in than came out.  Either way it goes to
total taxable for the year (negative for income, positive for
deductions).


Other
-----

See the examples subdirectory in the accjournal source
distribution for some complete sample files and accumulation
ideas.

See balance.el and creditcard.el for similar personal accounting
in a tighter file format.

If you like a text file for good editing, undoing, etc, but
accjournal is too limited then try ledger at
URL `http://newartisans.com/software/ledger.html'.  It has more
reports and automation, and includes a ledger.el editing mode.

skeleton.el and similar could help entering frequent transactions
\(see Info node `(autotype)Top').  Cut and paste is often good
enough.

There's a tie-in for auto-complete.el to complete AccJournal
account names at point.  See `accjournal-ac-source-accounts' for
how to set that up.


--------
The accjournal home page is
URL `http://user42.tuxfamily.org/accjournal/index.html'"

  ;; No need to declare :group since `custom-group' defaults to major-mode
  ;; without the -mode suffix.
  ;; :group 'accjournal

  (setq font-lock-defaults accjournal-mode-font-lock-defaults)
  (set (make-local-variable 'align-mode-rules-list) accjournal-align-rules)

  ;; comments must be at the start of a line
  (set (make-local-variable 'comment-start-skip) "^#+[ \t]*")
  (set (make-local-variable 'comment-start)      "#")
  (set (make-local-variable 'comment-end)        "")
  (set (make-local-variable 'comment-style)      'plain)
  (setq indent-tabs-mode nil)

  ;; Transactions are line-by-line so they should not wrap.
  ;; But comment lines can wrap.
  (set (make-local-variable 'auto-fill-inhibit-regexp) "^[^#]"))

;; LocalWords: AccJournal accjournal Capitalizations oct docstring acc
;; LocalWords: initializations Pers zInitial zSink FooCo BarCo InvHouse
;; LocalWords: creditcard aa ascii filename txt subdirectory matcher

(provide 'accjournal)

;;; accjournal.el ends here

;;; -*- mode: emacs-lisp; lexical-binding: t -*-

;;; rpl-edb.el -- utilities to parse the entries database

;; Copyright (C) 2014 Paul Onions

;; Author: Paul Onions <paul.onions@acm.org>
;; Keywords: RPL, UserRPL, SysRPL, HP48, HP49, HP50

;; This file is free software, see the LICENCE file in this directory
;; for copying terms.

;;; Commentary:

;; Functions to parse the entries.db file and create accessible
;; databases of SysRPL information.

;;; Code:
(eval-when-compile (require 'cl-lib))
(require 'rpl-base)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Functions for parsing the EDB file

(defun rpl-edb-get-line ()
  "Get line that point is on from the current buffer.
Return a string containing the line, or nil if at end of buffer.
As a side-effect set point to the start of the next line."
  (cond ((eobp)
         nil)
        (t
         (beginning-of-line)
         (let ((start (point)))
           (end-of-line)
           (let ((line (buffer-substring-no-properties start (point))))
             (forward-char)
             line)))))

;;; Parsing identifier lines
;;;
(defun rpl-trim-stack-effect-lines (lines)
  "Trim leading and trailing fluff from strings in LINES list."
  (let ((left-edge 1000))
    (dolist (s lines)
      (string-match "[[:blank:]]*" s)
      (when (< (match-end 0) left-edge)
        (setq left-edge (match-end 0))))
    (mapcar (lambda (s)
              (if (string-match "\\([[:blank:]]*\\(\\\\\\)*[[:blank:]]*$\\)" s)
                  (substring s left-edge (max left-edge (match-beginning 1)))
                (substring s left-edge)))
            lines)))

(defun rpl-tidy-stack-effect-lines (lines)
  "Tidy-up stack-effect lines."
  (rpl-trim-stack-effect-lines
   (mapcar (lambda (ln)
             (replace-regexp-in-string "\\\\->" "-->" ln))
           lines)))

(defun rpl-edb-consume-ident-line ()
  "Consume an EDB identifier line.
Return a list of two strings: the identifier and its stack effect
description.  Move point to the start of the next line."
  (let ((line (rpl-edb-get-line)))
    (cond ((string-match "^[[:graph:]]+" line)
           (let* ((name (match-string 0 line))
                  (desc (list (concat (make-string (match-end 0) 32)
                                      (substring line (match-end 0))))))
             ;; Automatically consume continuation lines
             ;; (after line ends with a backslash)
             (while (and (> (length (car desc)) 0)
                         (string-match ".*\\\\[[:blank:]]*$" (car desc)))
               (setq desc (cons (rpl-edb-get-line) desc)))
             (list name (rpl-tidy-stack-effect-lines (reverse desc)))))
          (t
           (list nil nil)))))

;;; Parsing keyword lines
;;;
(defun rpl-edb-parse-keyword-line (line)
  "Parse the given EDB keyword line.
Return a list consisting of the EDB keyword as a keyword symbol
and a parameter string (to be further parsed later)."
  (cond ((string-match "\\.[[:blank:]]+\\([[:alnum:]]+\\):" line)
         (let ((keyword (intern (concat ":" (match-string 1 line))))
               (param-str (substring line (match-end 0))))
           (list keyword param-str)))
        (t
         (list nil ""))))

(defun rpl-edb-parse-calc-param-str (str)
  (cond ((string-match "[[:blank:]]*\\([[:alnum:]]+\\)[[:blank:]]*\\(\\\\\\([[:graph:]]+?\\)\\\\\\)?" str)
         (let ((addr  (match-string 1 str))
               (fmt   (match-string 3 str))
               (flags nil))
           (setq str (substring str (match-end 0)))
           (while (string-match "[[:blank:]]*\\[\\([[:graph:]]+\\)\\]" str)
             (setq flags (cons (intern (concat ":" (match-string 1 str))) flags))
             (setq str (substring str (match-end 1))))
           (list addr fmt (reverse flags))))
        (t
         (list "" "" nil))))

(defun rpl-edb-parse-aka-param-str (str)
  (let ((names nil))
    (while (string-match "[[:blank:]]*\\([[:graph:]]+\\)" str)
      (setq names (cons (match-string 1 str) names))
      (setq str (substring str (match-end 1))))
    (reverse names)))

(defun rpl-edb-parse-userrpl-param-str (str)
  (let ((names nil))
    (while (string-match "[[:blank:]]*\\([[:graph:]]+\\)" str)
      (setq names (cons (match-string 1 str) names))
      (setq str (substring str (match-end 1))))
    (reverse names)))

(defun rpl-edb-consume-keyword-line ()
  (let ((line (rpl-edb-get-line)))
    (cl-destructuring-bind (keyword param-str)
        (rpl-edb-parse-keyword-line line)
      (cond ((member keyword '(:48G :49G))
             (cl-destructuring-bind (addr fmt flags)
                 (rpl-edb-parse-calc-param-str param-str)
               (append (list keyword addr fmt) flags)))
            ((eql keyword :AKA)
             (let ((names (rpl-edb-parse-aka-param-str param-str)))
               (cons keyword names)))
            ((eql keyword :UserRPL)
             (let ((names (rpl-edb-parse-userrpl-param-str param-str)))
               (cons keyword names)))
            (t
             (error "Illegal EDB keyword, %s" keyword))))))

;;; Parsing extended description lines
;;;
(defun rpl-edb-consume-description-line ()
  "Consume an EDB extended description line.
Return a string.  Move point to the start of the next line."
  (let ((line (rpl-edb-get-line)))
    (substring line 80)))

;;; Parsing the entries.db buffer
;;;
(defvar rpl-edb-entries nil
  "A place on which to push the entries parsed from the EDB file.")

(defun rpl-edb-parse-buffer ()
  "Parse the current buffer, assumed to be the ``entries.db'' file.
Set `rpl-edb-entries' to the parsed results, a list of EDB
entries, where each entry has the format:
  (NAMES STACK-EFFECT DESCRIPTION CALC-INFOS)
where NAMES is a list of strings representing the different names
under which the entry is known, STACK-EFFECT and DESCRIPTION are
lists of strings -- one for each line of text in their respective
desciptions -- and CALC-INFOS is a list of entries of the form:
  (CALC-KEY ADDRESS NAME-FORMAT &rest FLAG-KEYS)
where CALC-KEY is a keyword specifying a calculator
model (:48G or :49G), ADDRESS is a string containing
a hexadecimal address (5 digits for a ROM address, 6 digits for a
library/flash pointer), NAME-FORMAT is a FORMAT string allowing
the name of the entry to be modified for this particular
calculator, and FLAG-KEYS are keyword symbols specifying certain
flags for this calculator."
  (interactive)
  (let ((entry-names nil)
        (entry-stack-effect nil)
        (entry-description nil)
        (entry-calc-infos nil)
        (entries nil))
    (goto-char (point-min))
    (while (not (eobp))
      (cond ((eql (char-after) ?*)
             ;; A comment line -- ignore it
             (forward-line))
            ((eql (char-after) ?@)
             ;; A directive -- ignore it
             (forward-line))
            ((eql (char-after) ?\;)
             ;; An extended description line
             (setq entry-description (cons (rpl-edb-consume-description-line) entry-description)))
            ((eql (char-after) ?.)
             ;; A keyword line
             (cl-destructuring-bind (keyword &rest params) (rpl-edb-consume-keyword-line)
               (cond ((eql keyword :AKA)
                      (dolist (name params)
                        (push name entry-names)))
                     ((eql keyword :UserRPL)
                      (dolist (name params)
                        (push name entry-names)))
                     (t
                      (push (cons keyword params) entry-calc-infos)))))
            (t
             ;; An identifier/stack-effect line
             (when entry-names
               (push (list entry-names entry-stack-effect (reverse entry-description) entry-calc-infos) entries))
             (cl-destructuring-bind (name stack-effect) (rpl-edb-consume-ident-line)
               (cond (name
                      (setq entry-names (list name))
                      (setq entry-stack-effect stack-effect))
                     (t
                      (setq entry-names nil)
                      (setq entry-stack-effect nil)))
               (setq entry-calc-infos nil)
               (setq entry-description nil)))))
    (when entry-names
      (push (list entry-names entry-stack-effect (reverse entry-description) entry-calc-infos) entries))
    (setq rpl-edb-entries (reverse entries))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Functions to create calculator data files

(defun rpl-edb-generate-calculator-data (calculator)
  "Generate data for CALCULATOR (a keyword identifying the model).
Return a hash-table whose entries are keyed by entry name and
whose values are lists of the form:
  (STACK-EFFECT DESCRIPTION ADDRESS &rest FLAGS).
Assumes `rpl-edb-entries' has been set by calling
`rpl-edb-parse-buffer'."
  (cl-assert (keywordp calculator))
  (let ((table (make-hash-table :test 'equal)))
    (dolist (entry rpl-edb-entries)
      (cl-destructuring-bind (names stack-effect description calc-infos) entry
        (let ((calc-info (car (cl-member calculator calc-infos
                                         :test (lambda (key info) (equal key (car info)))))))
          (when calc-info
            (let* ((addr-str (cadr calc-info))
                   (fmt-str (if (caddr calc-info) (caddr calc-info) "%s"))
                   (flags (cdddr calc-info))
                   (stack-str (concat (car stack-effect)
                                      (apply 'concat (mapcar (lambda (s) (concat "\n" s))
                                                             (cdr stack-effect)))))
                   (descrip-str (concat (car description)
                                        (apply 'concat (mapcar (lambda (s) (concat "\n" s))
                                                               (cdr description)))))
                   (data (cons stack-str (cons descrip-str (cons addr-str flags)))))
              (dolist (name names)
                (puthash (format fmt-str name) data table)))))))
    table))

(defun rpl-edb-make-data-filename (calculator)
  "Make the SysRPL data filename used for CALCULATOR.
Where CALCULATOR should be a keyword symbol identifying the
calculator model, e.g. :48G, :49G etc."
  (cl-assert (keywordp calculator))
  (concat "sysrpl-data." (substring (symbol-name calculator) 1) ".el"))

(defun rpl-edb-make-calculator-data-file (calculator)
  "Make the appropriate SysRPL data file for CALCULATOR.
The CALCULATOR is identified by keyword: :48G or :49G."
  (cl-assert (keywordp calculator))
  (rpl-write-data-file (rpl-edb-generate-calculator-data calculator)
                       (rpl-edb-make-data-filename calculator)))

(defun rpl-edb-make-all-data-files ()
  "Create all SysRPL data files.
Assumes the current buffer contains the ``entries.db'' file
created by Carsten Dominik, parsing it if necessary to set the
`rpl-edb-entries' variable, then writing captured data to the
SysRPL data files, one for each calculator type."
  (interactive)
  (unless rpl-edb-entries
    (rpl-edb-parse-buffer))
  (dolist (calculator '(:48G :49G))
    (rpl-edb-make-calculator-data-file calculator)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Functions to read and query calculator data files
(defvar rpl-edb-data-48g nil
  "SysRPL data for the 48G calculator.")

(defvar rpl-edb-data-49g nil
  "SysRPL data for the 49G calculator.")

(defun rpl-edb-data (calculator)
"Get SysRPL data for the specified CALCULATOR.
Returns a hash table, keyed by SysRPL word name, whose values each
have the form (STACK-EFFECT DESCRIPTION ADDRESS &rest FLAGS)."
  (cl-assert (keywordp calculator))
  (cond ((eql calculator :48G)
         (unless rpl-edb-data-48g
           (setq rpl-edb-data-48g
                 (rpl-read-data-file (rpl-edb-make-data-filename :48G))))
         rpl-edb-data-48g)
        ((or (eql calculator :49G) (eql calculator :50G)) 
         (unless rpl-edb-data-49g
           (setq rpl-edb-data-49g
                 (rpl-read-data-file (rpl-edb-make-data-filename :49G))))
         rpl-edb-data-49g)))

(defun rpl-edb-all-names (calculator)
  (cl-assert (keywordp calculator))
  (let ((names nil))
    (maphash (lambda (key _val)
               (setq names (cons key names)))
             (rpl-edb-data calculator))
    names))

(defun rpl-edb-get-stack-effect (calculator name)
  (car (gethash name (rpl-edb-data calculator))))

(defun rpl-edb-get-description (calculator name)
  (cadr (gethash name (rpl-edb-data calculator))))

(defun rpl-edb-get-address (calculator name)
  (caddr (gethash name (rpl-edb-data calculator))))

(defun rpl-edb-get-flags (calculator name)
  (cadddr (gethash name (rpl-edb-data calculator))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; End of file
;;
(provide 'rpl-edb)

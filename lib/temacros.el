;;; TEmaCrOs.el --- TEco Edit MACRO System for Emacs
;; 
;; Filename: temacros.el
;; Description: 
;; Author: Jair Wang
;; Maintainer: 
;; Created: Sat Dec 16 00:53:46 2017 (-0800)
;; Version: 
;; Package-Requires: ()
;; Last-Updated: 
;;           By: 
;;     Update #: 0
;; URL: 
;; Doc URL: 
;; Keywords: 
;; Compatibility: 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary:
;; This is a register model based Emacs TECO emulation.
;; 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Change Log:
;; 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Code:

(defvar temacros-cmd-table (make-hash-table :test 'eq))
(defvar temacros-doc-table (make-hash-table :test 'eq))

(defmacro temacros-define-key (key &optional doc &rest val)
  "Define simple command alias."
  (let* ((str (and (stringp doc)
                   doc))
         (mac (if str
                  val
                (cons doc val))))
    `(progn
       (puthash (quote ,key) (list ,@mac) temacro-cmd-table)
       ,(when str
          `(puthash (quote ,key) ,str temacro-doc-table)))))

(defmacro temacro-define-command (key arglst dftarg body)
  "Define complex command."
  `(temacro-define-key ,key ,(list 'lambda arglst)))


(defun isearch-kill-current ()
  (interactive)
  (delete-region isearch-other-end (point)))

(define-key isearch-mode-map (kbd "C-d") 'isearch-kill-current)


(progn
  ;; Move
  (temacros-define-key l [?\C-n ?\C-a])
  (temacros-define-key b "Move back" [?\C-p ?\C-a])

  (temacros-define-key c [?\C-f] "Move current")
  (temacros-define-key r [?\C-b])

  (temacros-define-key j [?\M-g] "Jump")

  (temacros-define-key s [?\C-s])

  (temacros-define-key d [?\C-d] "Delete")
  (temacros-define-key k [?\C-k])
  (temacros-define-key v [?\M-d])
  (temacros-define-key w [?\M-f])
  (temacros-define-key y [C-backspace] "Backward kill word")

  (temacros-define-key t [?\C-l])

  (temacros-define-key \[ [?\C-v])
  (temacros-define-key \] [?\M-v])

  ;; Misc
  (temacros-define-key i [i] "Insert")
  (temacros-define-key f [f] "Find")
  (temacros-define-key e [e] "Execute")
  (temacros-define-key m [?\M-x] "M-x")
  (temacros-define-key ! [?\M-!] "Shell/Label")
  (temacros-define-key ^ [\^] "Escape")

  ;; POS
  (temacros-define-key z [?\C-e] "End")
  (temacros-define-key h [?\C-a] "Entire text")
  (temacros-define-key < [?\M-<] "BOB")
  (temacros-define-key > [?\M->] "EOB")

  ;; Window
  (temacros-define-key p [p] "Display")

  ;; Edit FIXME
  (temacros-define-key a [?\M-m ?h ?d ?c] "Describe character");; For avy jump

  ;; Register
  (temacros-define-key g [?\C-y] "Paste/Insert register");; Still special case
  (temacros-define-key q [q] "Get register")
  (temacros-define-key u [?\C-u] "Universal arg/Number to register")
  (temacros-define-key x [x] "Copy whole line")

  ;; U Arg
  (temacros-define-key 1 [?\C-u ?1])
  (temacros-define-key 2 [?\C-u ?2])
  (temacros-define-key 3 [?\C-u ?3])
  (temacros-define-key 4 [?\C-u ?4])
  (temacros-define-key 5 [?\C-u ?5])
  (temacros-define-key 6 [?\C-u ?6])
  (temacros-define-key 7 [?\C-u ?7])
  (temacros-define-key 8 [?\C-u ?8])
  (temacros-define-key 9 [?\C-u ?9])
  (temacros-define-key 0 [?\C-u ?0])
  (temacros-define-key '- [?\C-u ?-]))

(defvar temacros-arg0 nil)
(defvar temacros-arg1 nil)

(defun temacros-key-command (&optional arg)
  )

(defun temacros-execute-command (&optional arg1 arg2 arg3)
  (unless (gethash (intern (char-to-string 3)) temacros-cmd-table))
  ((z) (let ((temacros-arg0 (point-max))))))

(provide 'temacros)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; temacros.el ends here

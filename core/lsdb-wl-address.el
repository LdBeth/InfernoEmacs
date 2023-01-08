;;; lsdb-wl-address.el --- Intergrate LSDB with wl-addrmgr -*- lexical-binding:t -*-

;; Copyright (C) 2021 LdBeth

;; Author: LdBeth <andpuke@foxmail.com>
;; Keywords: address book

;; This file is part of the Lovely Sister Database.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;; For Wanderlust, put the following lines into your ~/.wl:
;;; (setq wl-address-init-function #'lsdb-wl-address-init)

;;; Code:

(require 'lsdb)
(require 'wl-address)

;;;###autoload
(defun lsdb-wl-address-init ()
  "Reload `wl-address-file'.
Refresh `wl-address-list', `wl-address-completion-list', and
`wl-address-petname-hash'."
  (message "Updating addresses...")
  (lsdb-maybe-load-hash-tables)
  (setq wl-address-list
        (let (tmp)
          (maphash (lambda (key value)
                     (push (list (copy-sequence (cadr (assq 'net value)))
                                 (copy-sequence (or (cdr (assq 'attribution value))
                                                    key))
                                 (copy-sequence key))
                           tmp))
                   lsdb-hash-table)
          tmp))
  (setq wl-address-completion-list
        (wl-address-make-completion-list wl-address-list))
  (setq wl-address-petname-hash (make-hash-table :test #'equal))
  (mapc
   (lambda (addr)
     (puthash (downcase (car addr))
              (cadr addr)
              wl-address-petname-hash))
   wl-address-list)
  (message "Updating addresses...done"))

(defun lsdb-wl-address-delete (the-email)
  "Delete address entry in the LSDB."
  (message "Deleting Address...")
  (let* ((name (gethash the-email lsdb-address-cache))
         (entry (gethash name lsdb-hash-table)))
    (when (and name entry)
      (lsdb-delete-record (cons name entry))))
  ;; Delete entries.
  (dolist (entry (elmo-string-assoc-all the-email wl-address-list))
    (setq wl-address-list (delete entry wl-address-list)))
  (puthash the-email nil wl-address-petname-hash)
  (message "Deleting Address...done"))

(advice-add 'wl-address-delete :override #'lsdb-wl-address-delete)

(defun lsdb-wl-address-add-or-change (address
                                      &optional default-realname
                                      change-address)
  "Add address entry to LSDB, if not registerd.
If already registerd, change it."
  (let ((entry (assoc address wl-address-list))
	the-realname the-petname new-addr)
    (setq the-realname
	  (read-from-minibuffer "Real Name: " (or default-realname
						  (nth 2 entry))))
    (setq the-petname (read-from-minibuffer "Petname: "
					    (or (nth 1 entry)
						the-realname)))
    (when change-address
      (setq new-addr (read-from-minibuffer "E-Mail: " address))
      (cond
       ((or (not (stringp new-addr))
	    (string-match "^[ \t]*$" new-addr))
	(error "empty address"))
       ((and (not (string= address new-addr))
	     (assoc new-addr wl-address-list))
	(error "'%s' already exists" new-addr))
       (t
	;; do nothing
	)))
    (lsdb-update-record (list the-realname address)
                        (if new-addr
                            `((net ,new-addr))))
    (wl-address-init)
    (list (or new-addr address) the-petname the-realname)))

(advice-add 'wl-address-add-or-change
            :override #'lsdb-wl-address-add-or-change)

(provide 'lsdb-wl-address)

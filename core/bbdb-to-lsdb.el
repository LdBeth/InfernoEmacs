;;; bbdb-to-lsdb.el --- the data conversion utility from BBDB to LSDB

;; Copyright (C) 2002 Daiki Ueno

;; Author: Daiki Ueno <ueno@unixuser.org>
;; Keywords: adress book

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

;;; To convert your ~/.bbdb to ~/.lsdb, type the following command from shell:
;;;   emacs -batch -l ./bbdb-to-lsdb.el

;;; Code:

(require 'bbdb)
(require 'lsdb)

(let ((bbdb-records (bbdb-records))
      value)
  (lsdb-maybe-load-hash-tables)
  (while bbdb-records
    (when (setq value (bbdb-record-net (car bbdb-records)))
      (lsdb-update-record
       (list (or (bbdb-record-name (car bbdb-records))
		 (car value))
	     (car value))
       (nconc
	(if (cdr value)
	    (list (cons 'net (cdr value))))
	(if (setq value (bbdb-record-aka (car bbdb-records)))
	    (list (cons 'aka value)))
	(if (setq value (bbdb-record-company (car bbdb-records)))
	    (list (cons 'company value)))
	(bbdb-record-raw-notes (car bbdb-records)))))
    (setq bbdb-records (cdr bbdb-records)))
  (lsdb-mode-save t))

;;; bbdb-to-lsdb.el ends here

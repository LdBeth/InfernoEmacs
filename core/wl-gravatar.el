;;; wl-gravatar.el --- gravatar fetch/store functions

;; Copyright (C) 2010  Kazuhiro NISHIYAMA

;; Author: Kazuhiro NISHIYAMA <zn@mbf.nifty.com>
;; Keywords: faces, tools, extensions, mail

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Usage:
;; (require 'wl-gravatar)
;; (setq wl-highlight-x-face-function 'wl-gravatar-insert)
;; (setq gnus-gravatar-directory "~/.emacs-gravatar/")
;; (setq gravatar-unregistered-icon 'identicon)
;; (setq wl-gravatar-retrieve-once t)

;;; Code:

(require 'gravatar)

(defvar wl-gravatar-retrieve-once nil)

(defun wl-gravatar-insert (&rest dummy)
  "Display Gravatar images."
  (let ((field (std11-fetch-field "From"))
        image)
    (message "wl-gravatar-insert: field=%s, address=%s" field (when field (wl-address-header-extract-address field)))
    (when field
      (gravatar-retrieve 

       (wl-address-header-extract-address field)

       (lambda (image buffer)
         (unless (eq image 'error)
           (with-current-buffer buffer
             (save-excursion
               (goto-char (point-min))
               (when (re-search-forward "^From: " nil t)
                 (let ((inhibit-read-only t))
                   ; (message "inserting gravatar in buffer %s" (buffer-name))
                   (insert-image image)))))))

       `(,(current-buffer))))))

(provide 'wl-gravatar)
;;; wl-gravatar.el ends here

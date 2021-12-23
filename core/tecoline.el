;;; tecoline.el --- my custom nano modeline -*- lexical-binding: t -*-

;; Copyright (C) 2021 Free Software Foundation, Inc.

;; Maintainer: Nicolas P. Rougier <Nicolas.Rougier@inria.fr>
;; URL: https://github.com/rougier/nano-modeline
;; Version: 0.2
;; Package-Requires: ((emacs "27.1"))
;; Keywords: convenience, mode-line, header-line

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <https://www.gnu.org/licenses/>.
(defgroup nano-modeline nil
  "T E C O Modeline"
  :group 'nano)

(defface nano-modeline
  '((t (:inherit mode-line)))
  "Modeline face for active modeline"
  :group 'nano-modeline)

(defface nano-modeline-name
  '((t (:inherit (mode-line bold))))
  "Modeline face for active name element"
  :group 'nano-modeline)

(defface nano-modeline-primary
  '((t (:inherit mode-line)))
  "Modeline face for active primary element"
  :group 'nano-modeline)

(defface nano-modeline-secondary
  '((t (:inherit mode-line)))
  "Modeline face for active secondary element"
  :group 'nano-modeline)

(defface nano-modeline-status-RO
  '((t (:inherit mode-line)))
  "Modeline face for active READ-ONLY element"
  :group 'nano-modeline)

(defface nano-modeline-status-RW
  '((t (:inherit mode-line)))
  "Modeline face for active READ-WRITE element"
  :group 'nano-modeline)

(defface nano-modeline-status-**
  '((t (:inherit mode-line)))
  "Modeline face for active MODIFIED element"
  :group 'nano-modeline)

(defun nano-modeline-compose (status name primary secondary)
  "Compose a string with provided information"
  (let ((prefix (cond ((string= status "RO")
                       (propertize (if (window-dedicated-p)"•RO " " RO ")
                                   'face 'nano-modeline-status-RO))
                      ((string= status "**")
                       (propertize (if (window-dedicated-p)"•** " " ** ")
                                   'face 'nano-modeline-status-**))
                      ((string= status "RW")
                       (propertize (if (window-dedicated-p) "•RW " " RW ")
                                   'face 'nano-modeline-status-RW))
                      (t (propertize status
                                     'face 'nano-modeline-status-**))))
        (left (concat
               (propertize " "  'face 'nano-modeline)
               (propertize name 'face 'nano-modeline-name)
               (propertize " "  'face 'nano-modeline)
               (propertize primary 'face 'nano-modeline-primary)))
        (right (concat secondary " ")))

    (concat prefix
            left
            " >> "
            (propertize right 'face 'nano-modeline-secondary))))

(defun nano-modeline-status ()
  "Return buffer status: read-only (RO), modified (**) or read-write (RW)"
  (let ((read-only   buffer-read-only)
        (modified    (and buffer-file-name (buffer-modified-p))))
    (cond (modified  "**") (read-only "RO") (t "RW"))))

(defun nano-modeline-vc-branch ()
  "Return current VC branch if any."
  (if vc-mode
      (let ((backend (vc-backend buffer-file-name)))
        (concat "#"
                (substring-no-properties
                 vc-mode
                 (+ (if (eq backend 'Hg) 2 3) 2))))  nil))

(defun nano-modeline-default-mode ()
  (let ((buffer-name (format-mode-line mode-line-buffer-identification))
        (mode-name   (format-mode-line mode-name))
        (branch      (nano-modeline-vc-branch))
        (position    (format-mode-line "%l:%c")))
    (nano-modeline-compose (nano-modeline-status)
                           buffer-name
                           (concat mode-name
                                   (if branch (concat ", "
                                                      (propertize branch 'face 'italic)))
                                   )
                           position)))

(provide 'tecoline)

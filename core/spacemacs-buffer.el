;;; core-spacemacs-buffer.el -*- lexical-binding:t -*-
;; Spacemacs Core File
;;
;; Copyright (c) 2012-2021 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;
;;; Commentary:
;;
;;; Code:

(defconst spacemacs-buffer-name "*LaunchPad*"
  "The name of the spacemacs buffer.")

(defconst spacemacs-buffer-buttons-startup-lists-offset 25
  "Relative position between the home buffer buttons and startup lists.")

(defconst spacemacs-buffer--window-width 80
  "Current width of the home buffer if responsive, 80 otherwise.
See `dotspacemacs-startup-buffer-responsive'.")

(defvar spacemacs-buffer-startup-lists-length 20
  "Length used for startup lists with otherwise unspecified bounds.
Set to nil for unbounded.")

(defvar spacemacs-buffer-list-separator "\n\n")

(defvar spacemacs-buffer--buttons-position nil
  "Horizontal position of the home buffer buttons.
Internal use, do not set this variable.")

(defvar spacemacs-buffer-mode-map
  (eval-when-compile
    (let ((map (make-sparse-keymap)))
      (suppress-keymap map)
      (define-key map (kbd "0") 'spacemacs-buffer/jump-to-number-startup-list-line)
      (define-key map (kbd "1") 'spacemacs-buffer/jump-to-number-startup-list-line)
      (define-key map (kbd "2") 'spacemacs-buffer/jump-to-number-startup-list-line)
      (define-key map (kbd "3") 'spacemacs-buffer/jump-to-number-startup-list-line)
      (define-key map (kbd "4") 'spacemacs-buffer/jump-to-number-startup-list-line)
      (define-key map (kbd "5") 'spacemacs-buffer/jump-to-number-startup-list-line)
      (define-key map (kbd "6") 'spacemacs-buffer/jump-to-number-startup-list-line)
      (define-key map (kbd "7") 'spacemacs-buffer/jump-to-number-startup-list-line)
      (define-key map (kbd "8") 'spacemacs-buffer/jump-to-number-startup-list-line)
      (define-key map (kbd "9") 'spacemacs-buffer/jump-to-number-startup-list-line)

      (define-key map [down-mouse-1] 'widget-button-click)
      (define-key map (kbd "RET") 'widget-button-press)

      (define-key map [tab] 'widget-forward)
      (define-key map (kbd "n") 'widget-forward)
      (define-key map (kbd "C-i") 'widget-forward)

      (define-key map [backtab] 'widget-backward)
      (define-key map (kbd "p") 'widget-backward)

      (define-key map (kbd "C-r") 'spacemacs-buffer/refresh)
      (define-key map "q" 'quit-window)
      map))
  "Keymap for spacemacs buffer mode.")

(defun spacemacs//redisplay ()
  "`redisplay' wrapper."
  (force-window-update)
  (redisplay))

(define-derived-mode spacemacs-buffer-mode fundamental-mode "LaunchPad"
  "Spacemacs major mode for startup screen."
  :group 'spacemacs
  :syntax-table nil
  :abbrev-table nil
  (page-break-lines-mode)
  (setq buffer-read-only t
        truncate-lines t))

(defun spacemacs-buffer/insert-banner-and-buttons ()
  "Choose a banner according to `tecomacs-startup-lists' and insert it.
in spacemacs buffer along with quick buttons underneath.
Easter egg:
Doge special text banner can be reachable via `999', `doge' or `random*'.
Doge special text banner for dark themes can be reachable via `997',
`doge-inverted' or `random*'.
Cate special text banner can de reachable via `998', `cat' or `random*'.
`random' ignore special banners whereas `random*' does not."
  (let ((buffer-read-only nil))
    (progn
      (spacemacs-buffer//insert-image-banner)
      (spacemacs-buffer//insert-buttons)
      (let ((len (- (line-end-position)
                (line-beginning-position))))
	(spacemacs-buffer//center-line)
	(setq spacemacs-buffer--buttons-position (- (line-end-position)
						    (line-beginning-position)
						    len)))
      (spacemacs//redisplay))))

(defun spacemacs-buffer//insert-image-banner ()
  "Display an image banner."
  (let* ((size (image-size tecomacs-banner))
         (width (car size))
         (left-margin (max 0 (floor (- spacemacs-buffer--window-width width) 2))))
    (insert (make-string left-margin ?\s))
    (let ((start (point)))
      (insert " ")
      (add-text-properties
       start (point)
       `(display ,tecomacs-banner
                 rear-nonsticky t)))
    (insert "\n\n"))
  (insert (make-string (max 0 (floor (/ (- spacemacs-buffer--window-width
                                           (+ (length tecomacs-title) 1))
                                        2)))
                       ?\s))
  (insert (format "%s\n" tecomacs-title)))

(defun spacemacs-buffer/insert-page-break ()
  "Insert a page break line in spacemacs buffer."
  (insert "\n\n"))

(defun spacemacs-buffer/append (msg &optional messagebuf)
  "Append MSG to spacemacs buffer.
If MESSAGEBUF is not nil then MSG is also written in message buffer."
  (with-current-buffer (get-buffer-create spacemacs-buffer-name)
    (goto-char (point-max))
    (let ((buffer-read-only nil))
      (insert msg)
      (when messagebuf
        (message "(Spacemacs) %s" msg)))))

(defun spacemacs-buffer//startup-list-jump-func-name (str)
  "Given a string, return a spacemacs-buffer function name.

Given:           Return:
\"[?]\"            \"spacemacs-buffer/jump-to-[?]\"
\"Recent Files:\"  \"spacemacs-buffer/jump-to-recent-files\""
  (let ((s (downcase str)))
    ;; remove last char if it's a colon
    (when (string-match ":$" s)
      (setq s (substring s nil (1- (length s)))))
    ;; replace any spaces with a dash
    (setq s (replace-regexp-in-string " " "-" s))
    (concat "spacemacs-buffer/jump-to-" s)))

(defmacro spacemacs-buffer||add-shortcut
    (shortcut-char search-label &optional no-next-line)
  "Add a single-key keybinding for quick navigation in the home buffer.
Navigation is done by searching for a specific word in the buffer.
SHORTCUT-CHAR: the key that the user will have to press.
SEARCH-LABEL: the word the cursor will be brought under (or on).
NO-NEXT-LINE: if nil the cursor is brought under the searched word.

Define a named function: spacemacs-buffer/jump-to-...
for the shortcut. So that a descriptive name is shown,
in for example the view-lossage (C-h l) buffer:
 r                      ;; spacemacs-buffer/jump-to-recent-files
 p                      ;; spacemacs-buffer/jump-to-projects
instead of:
 r                      ;; anonymous-command
 p                      ;; anonymous-command"
  (let* ((func-name (spacemacs-buffer//startup-list-jump-func-name search-label))
         (func-name-symbol (intern func-name)))
    `(progn
       (defun ,func-name-symbol ()
         (interactive)
         (unless (search-forward ,search-label (point-max) t)
           (search-backward ,search-label (point-min) t))
         ,@(unless no-next-line
             '((forward-line 1)))
         (back-to-indentation))
       (define-key
	 spacemacs-buffer-mode-map ,shortcut-char ',func-name-symbol))))

(defun spacemacs-buffer//center-line (&optional real-width)
  "When point is at the end of a line, center it.
REAL-WIDTH: the real width of the line.  If the line contains an image, the size
            of that image will be considered to be 1 by the calculation method
            used in this function.  As a consequence, the caller must calculate
            himself the correct length of the line taking into account the
            images he inserted in it."
  (let* ((width (or real-width (current-column)))
         (margin (max 0 (floor (/ (- spacemacs-buffer--window-width
                                     width)
                                  2)))))
    (beginning-of-line)
    (insert (make-string margin ?\s))
    (end-of-line)))

(defun spacemacs-buffer//insert-buttons ()
  "Create and insert the interactive buttons under Spacemacs banner."
  (let ((len (- (line-end-position)
                (line-beginning-position))))
    (spacemacs-buffer//center-line)
    (setq spacemacs-buffer--buttons-position
          (- (line-end-position)
             (line-beginning-position)
             len)))
  (insert "\n")
  (widget-create 'push-button
                 :help-echo "Yet Another Message Interface On Emacsen."
                 :action (lambda (&rest _) (wl t))
                 :mouse-face 'highlight
                 :follow-link "\C-m"
                 (propertize "Wanderlust" 'face 'font-lock-keyword-face))
  (insert " ")
  (widget-create 'push-button
                 :help-echo "Read some RSS."
                 :action (lambda (&rest _)
                           (newsticker-show-news))
                 :mouse-face 'highlight
                 :follow-link "\C-m"
                 (propertize "Newsticker" 'face 'font-lock-keyword-face))
  (insert " ")
  (widget-create 'push-button
                 :help-echo
                 "Start ERC."
                 :action (lambda (&rest _)
                           (erc-tls :server "irc.libera.chat"
                                    :port "6697"))
                 :mouse-face 'highlight
                 :follow-link "\C-m"
                 (propertize "Libera Chat"
                             'face 'font-lock-keyword-face))
  (spacemacs-buffer//center-line)
  (insert "\n")
  (widget-create 'push-button
                 :help-echo
                 "The Everyday Hypertextual Information Manager."
                 :action (lambda (&rest _)
                           (hyperbole-mode 1)
                           (find-file
                            (expand-file-name
                             hbmap:filename hbmap:dir-user)))
                 :mouse-face 'highlight
                 :follow-link "\C-m"
                 (propertize "Hyperbole"
                             'face 'font-lock-keyword-face))
  (spacemacs-buffer//center-line)
  (insert "\n"))

(defun spacemacs-buffer//insert-string-list (list-display-name list)
  "Insert a non-interactive startup list in the home buffer.
LIST-DISPLAY-NAME: the displayed title of the list.
LIST: a list of strings displayed as entries."
  (when (car list)
    (insert list-display-name)
    (mapc (lambda (el)
            (insert
             "\n"
             (with-temp-buffer
               (insert el)
               (fill-paragraph)
               (goto-char (point-min))
               (insert "    - ")
               (while (= 0 (forward-line))
                 (insert "      "))
               (buffer-string))))
          list)))

(defun spacemacs-buffer//insert-file-list (list-display-name list)
  "Insert an interactive list of files in the home buffer.
LIST-DISPLAY-NAME: the displayed title of the list.
LIST: a list of string pathnames made interactive in this function.

If LIST-DISPLAY-NAME is \"Recent Files:\":
prepend each list item with a number starting at: 1
The numbers indicate that the file can be opened,
by pressing its number key."
  (when (car list)
    (insert list-display-name)
    (mapc (lambda (el)
            (insert "\n    ")
            (let ((button-text
                   (format "%2s %s" (number-to-string
                                     spacemacs-buffer--startup-list-nr)
                           (abbreviate-file-name el))))
              (widget-create 'push-button
                             :action (lambda (&rest _)
                                       (find-file-existing el))
                             :mouse-face 'highlight
                             :follow-link "\C-m"
                             :button-prefix ""
                             :button-suffix ""
                             :format "%[%t%]" button-text))
            (setq spacemacs-buffer--startup-list-nr
                  (1+ spacemacs-buffer--startup-list-nr)))
          list)))

(defun spacemacs-buffer//insert-files-by-dir-list
    (list-display-name grouped-list)
  "Insert an interactive grouped list of files in the home buffer.
LIST-DISPLAY-NAME: the displayed title of the list.
GROUPED-LIST: a list of string pathnames made interactive in this function."
  (when (car grouped-list)
    (insert list-display-name)
    (mapc (lambda (group)
            (insert "\n    ")
            (let ((button-text-project
                   (format "%2s %s" (number-to-string
                                   spacemacs-buffer--startup-list-nr)
                           (abbreviate-file-name (car group)))))
              (widget-create 'push-button
                             :action (let ((f (car group)))
                                       (lambda (&rest _)
                                         (find-file-existing f)))
                             :mouse-face 'highlight
                             :follow-link "\C-m"
                             :button-prefix ""
                             :button-suffix ""
                             :format "%[%t%]" button-text-project)
              (setq spacemacs-buffer--startup-list-nr
                    (1+ spacemacs-buffer--startup-list-nr))
              (mapc (lambda (el)
                      (let ((button-text-filename
                             (format "%2s %s" (number-to-string
                                               spacemacs-buffer--startup-list-nr)
                                     (abbreviate-file-name el))))
                        (insert "\n        ")
                        (widget-create 'push-button
                                       :action (let ((f (concat (car group) el)))
                                                 (lambda (&rest _)
                                                   (find-file-existing f)))
                                       :mouse-face 'highlight
                                       :follow-link "\C-m"
                                       :button-prefix ""
                                       :button-suffix ""
                                       :format "%[%t%]" button-text-filename))
                      (setq spacemacs-buffer--startup-list-nr
                            (1+ spacemacs-buffer--startup-list-nr)))
                    (cdr group))))
          grouped-list)))

(defun spacemacs-buffer//insert-bookmark-list (list-display-name list)
  "Insert an interactive list of bookmarks entries (if any) in the home buffer.
LIST-DISPLAY-NAME: the displayed title of the list.
LIST: a list of string bookmark names made interactive in this function."
  (when (car list)
    (insert list-display-name)
    (mapc (lambda (el)
            (insert "\n    ")
            (let ((button-text
                   (format "%2s %s"
                           (number-to-string
                            spacemacs-buffer--startup-list-nr)
                           el)))
              (widget-create 'push-button
                             :action (lambda (&rest _) (bookmark-jump el))
                             :mouse-face 'highlight
                             :follow-link "\C-m"
                             :button-prefix ""
                             :button-suffix ""
                             :format "%[%t%]" button-text))
            (setq spacemacs-buffer--startup-list-nr
                  (1+ spacemacs-buffer--startup-list-nr)))
          list)))

(defun spacemacs-buffer//associate-to-project (recent-file by-project)
  (dolist (x by-project)
    (when (string-prefix-p (car x) recent-file)
      (setcdr x (cons (string-remove-prefix (car x) recent-file) (cdr x))))))

(defun spacemacs-buffer//recent-files-by-project ()
  (let ((by-project (mapcar (lambda (p) (cons (expand-file-name p) nil))
                            (projectile-relevant-known-projects))))
    (dolist (recent-file recentf-list by-project)
      (spacemacs-buffer//associate-to-project recent-file by-project))))

(defun spacemacs-buffer//insert-recent-files (list-size)
  (recentf-mode 1)
  (let ((recent-files-list
         (seq-take recentf-list list-size)))
    (when (spacemacs-buffer//insert-file-list
           "Recent Files:"
           recent-files-list)
      (spacemacs-buffer||add-shortcut "r" "Recent Files:")))
  (insert spacemacs-buffer-list-separator))

(defun spacemacs-buffer//insert-recent-files-by-project (list-size)
  (unless recentf-mode (recentf-mode))
  ;;(unless projectile-mode (projectile-mode))
  (when (spacemacs-buffer//insert-files-by-dir-list
         "Recent Files by Project:"
         (mapcar (lambda (group)
                   (cons (car group)
                         (seq-take (reverse (cdr group))
                                   (cdr list-size))))
                 (seq-take (spacemacs-buffer//recent-files-by-project)
                           (car list-size))))
    (spacemacs-buffer||add-shortcut "R" "Recent Files by Project:")
    (insert spacemacs-buffer-list-separator)))

(defun spacemacs-buffer//insert-bookmarks (list-size)
  (require 'bookmark)
  (when (spacemacs-buffer//insert-bookmark-list
         "Bookmarks:"
         (seq-take (bookmark-all-names) list-size))
    (spacemacs-buffer||add-shortcut "b" "Bookmarks:")
    (insert spacemacs-buffer-list-separator)))

(defvar spacemacs-buffer--startup-list-nr 1)

(defun spacemacs-buffer//do-insert-startupify-lists ()
  "Insert the startup lists in the current buffer."
  (setq spacemacs-buffer--startup-list-nr 1)
  (dolist (els tecomacs-startup-lists)
    (let ((el (or (car-safe els) els))
          (list-size (or (cdr-safe els)
                         spacemacs-buffer-startup-lists-length)))
      (cond
       ((eq el 'recents) (spacemacs-buffer//insert-recent-files list-size))
       ((eq el 'recents-by-project)
        (spacemacs-buffer//insert-recent-files-by-project list-size))
       ((eq el 'bookmarks) (spacemacs-buffer//insert-bookmarks list-size))))))

(defun spacemacs-buffer//get-buffer-width ()
  "Return the length of longest line in the current buffer."
  (save-excursion
    (goto-char 0)
    (let ((current-max 0))
      (while (not (eobp))
        (let ((line-length (- (line-end-position) (line-beginning-position))))
          (if (< current-max line-length)
              (setq current-max line-length)))
        (forward-line 1))
      current-max)))

(defun spacemacs-buffer//center-startup-lists ()
  "Center startup lists after they were inserted."
  (let* ((lists-width (spacemacs-buffer//get-buffer-width))
         (margin (max 0 (- spacemacs-buffer--buttons-position
                           spacemacs-buffer-buttons-startup-lists-offset)))
         (final-padding (if (< spacemacs-buffer--window-width
                               (+ margin lists-width))
                            (max 0 (floor (/ (- spacemacs-buffer--window-width
                                                lists-width)
                                             2)))
                          margin)))
    (goto-char (point-min))
    (while (not (eobp))
      (beginning-of-line)
      (insert (make-string final-padding ?\s))
      (forward-line))))

(defun spacemacs-buffer/insert-startup-lists ()
  "Insert startup lists in home buffer."
  (interactive)
  (with-current-buffer (get-buffer spacemacs-buffer-name)
    (let ((buffer-read-only nil))
      (goto-char (point-max))
      (spacemacs-buffer/insert-page-break)
      (insert "\n")
      (save-restriction
        (narrow-to-region (point) (point))
        (spacemacs-buffer//do-insert-startupify-lists)
        (spacemacs-buffer//center-startup-lists)))))

(defun spacemacs-buffer/goto-link-line ()
  "Set point to the beginning of the link line."
  (interactive)
  (with-current-buffer spacemacs-buffer-name
    (goto-char (point-min))
    (with-demoted-errors "spacemacs buffer error: %s"
      (search-forward "[")
      (left-char 2))))

(defvar spacemacs-buffer--idle-numbers-timer nil
  "This stores the idle numbers timer.")

(defvar spacemacs-buffer--startup-list-number nil
  "This accumulates the numbers that are typed in the home buffer.
It's cleared when the idle timer runs.")

(defun spacemacs-buffer/jump-to-number-startup-list-line ()
  "Jump to the startup list line with the typed number.

The minimum delay in seconds between number key presses,
can be adjusted with the variable:
`dotspacemacs-startup-buffer-multi-digit-delay'."
  (interactive)
  (when spacemacs-buffer--idle-numbers-timer
    (cancel-timer spacemacs-buffer--idle-numbers-timer))
  (let* ((key-pressed-string (string last-input-event)))
    (setq spacemacs-buffer--startup-list-number
          (concat spacemacs-buffer--startup-list-number key-pressed-string))
    (let (message-log-max) ; only show in minibuffer
      (message "Jump to startup list: %s" spacemacs-buffer--startup-list-number))
    (setq spacemacs-buffer--idle-numbers-timer
          (run-with-idle-timer
           0.4 nil
           'spacemacs-buffer/stop-waiting-for-additional-numbers))))

(defun spacemacs-buffer/jump-to-line-starting-with-nr-space (nr-string)
  "Jump to the line number that starts with NR."
  (let ((prev-point (point)))
    (goto-char (window-start))
    (if (not (re-search-forward
              (concat "^\s?*" nr-string " ")
              ;; don't search past two lines above the window-end,
              ;; because they bottom two lines are hidden by the mode line
              (save-excursion (goto-char (window-end))
                              (forward-line -1)
                              (point))
              'noerror))
        (progn (goto-char prev-point)
               (let (message-log-max) ; only show in minibuffer
                 (message "Couldn't find startup list number: %s"
                          spacemacs-buffer--startup-list-number)))
      (back-to-indentation)
      (message "Opening file/dir: %s"
               (replace-regexp-in-string "^\s*" ""
                (widget-value (widget-at (point)))))
      (widget-button-press (point)))))

(defun spacemacs-buffer/stop-waiting-for-additional-numbers ()
  (spacemacs-buffer/jump-to-line-starting-with-nr-space
   spacemacs-buffer--startup-list-number)
  (setq spacemacs-buffer--startup-list-number nil))

(defun spacemacs-buffer//startup-hook ()
  "Code executed when Emacs has finished loading."
  (with-current-buffer (get-buffer spacemacs-buffer-name)
    (spacemacs-buffer/insert-startup-lists)
    (spacemacs-buffer-mode)
    (force-mode-line-update)
    (spacemacs-buffer/goto-link-line)))

(defvar spacemacs-buffer--last-width nil
  "Previous width of spacemacs-buffer.")

(defun spacemacs-buffer/goto-buffer (&optional refresh)
  "Create the special buffer for `spacemacs-buffer-mode' and switch to it.
REFRESH if the buffer should be redrawn.

If a prefix argument is given, switch to it in an other, possibly new window."
  (interactive)
  (let ((buffer-exists (buffer-live-p (get-buffer spacemacs-buffer-name)))
        save-line)
    (when (or (not (eq spacemacs-buffer--last-width (window-width)))
              (not buffer-exists)
              refresh)
      (setq spacemacs-buffer--window-width (window-width)
            spacemacs-buffer--last-width spacemacs-buffer--window-width)
      (with-current-buffer (get-buffer-create spacemacs-buffer-name)
        (page-break-lines-mode)
        (save-excursion
          (when (> (buffer-size) 0)
            (setq save-line (line-number-at-pos))
            (let ((inhibit-read-only t))
              (erase-buffer)))
          (let ((inhibit-read-only t))
            (insert "\n"))
          (spacemacs-buffer/insert-banner-and-buttons)
          (when (bound-and-true-p spacemacs-initialized)
	    (spacemacs-buffer/insert-startup-lists)
            (force-mode-line-update)
            (spacemacs-buffer-mode)))
        (if save-line
            (progn (goto-char (point-min))
                   (forward-line (1- save-line))
                   (forward-to-indentation 0))
          (spacemacs-buffer/goto-link-line)))
      (if current-prefix-arg
          (switch-to-buffer-other-window spacemacs-buffer-name)
        (switch-to-buffer spacemacs-buffer-name))
      (spacemacs//redisplay))))

(add-hook 'window-setup-hook
          (lambda ()
            (add-hook 'window-configuration-change-hook
                      'spacemacs-buffer//resize-on-hook)
            (spacemacs-buffer//resize-on-hook)))

(defun spacemacs-buffer//resize-on-hook ()
  "Hook run on window resize events to redisplay the home buffer."
  ;; prevent spacemacs buffer redisplay in the filetree window
  (let ((home-buffer (get-buffer-window spacemacs-buffer-name))
        (frame-win (frame-selected-window)))
    (when (and home-buffer
               (not (window-minibuffer-p frame-win)))
      (with-selected-window home-buffer
        (spacemacs-buffer/goto-buffer)))))

(defun spacemacs-buffer/refresh ()
  "Force recreation of the spacemacs buffer."
  (interactive)
  (setq spacemacs-buffer--last-width nil)
  (spacemacs-buffer/goto-buffer t))

(defalias 'spacemacs/home 'spacemacs-buffer/refresh
  "Go to home Spacemacs buffer")

(defun spacemacs/home-delete-other-windows ()
  "Open home Spacemacs buffer and delete other windows.
Useful for making the home buffer the only visible buffer in the frame."
  (interactive)
  (spacemacs/home)
  (delete-other-windows))

(provide 'spacemacs-buffer)

;;; core-spacemacs-buffer ends here

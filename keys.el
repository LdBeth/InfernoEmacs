;; -*- lexical-binding:t -*-
(defun switch-to-scratch-buffer (&optional arg)
  "Switch to the `*scratch*' buffer, creating it first if needed.
if prefix argument ARG is given, switch to it in an other, possibly new window."
  (interactive "P")
  (let ((exists (get-buffer "*scratch*")))
    (if arg
        (switch-to-buffer-other-window (get-buffer-create "*scratch*"))
      (switch-to-buffer (get-buffer-create "*scratch*")))
    (unless (or exists
                (eq major-mode initial-major-mode))
      (funcall initial-major-mode))))

(defun logging-disabled-command (&optional cmd keys)
  (unless cmd (setq cmd this-command))
  (message "%s was disabled." cmd)
  (call-interactively cmd nil keys))

(setq disabled-command-function #'logging-disabled-command)

(defun now-playing ()
  (interactive)
  (let* ((script (eval-when-compile
                   (mac-osa-compile "if application \"Music\" is running then
  tell application \"Music\"
    if player state is stopped then
      set display to \"No Track Playing\"
    else
      set track_artist to artist of current track
      set track_name to name of current track
      set display to track_artist & \" - \" & track_name
    end if
  end tell
else
  set display to \"Music.app is not running\"
end if")))
         (nowplay (read (mac-osa-script script t nil))))
    (when (called-interactively-p 'interactive)
        (message "%s" nowplay))
    nowplay))

(defun now-browsing ()
  (interactive)
  (let* ((script (eval-when-compile
                   (mac-osa-compile "if application \"Safari\" is running then
  tell application \"Safari\"
    set display to URL of front document
  end tell
else
  set display to \"No opened document.\"
end if")))
         (url (read (mac-osa-script script t nil))))
    (when (called-interactively-p 'interactive)
      (kill-new url)
      (message "%s" url))
    url))

(bind-keys
 :prefix "M-m"
 :prefix-map launchpad-key-map
 ("h" . spacemacs/home)
 ("s" . switch-to-scratch-buffer)
 ("i" . ibuffer)
 ("w" . wl)
 ("f" . make-frame)
 ("n" . newsticker-show-news))

(unbind-key "C-h C-h" global-map)
(unbind-key "C-h ?" global-map)

(define-key key-translation-map (kbd "￥") (kbd "\\"))

(add-to-list 'auto-mode-alist '("\\.spad" . prog-mode) t)

;; fixed in latest emacs
(advice-add 'newsticker--cache-read
            :override #'newsticker--cache-read-version1)

(add-hook 'minibuffer-setup-hook 'corfu-mode)

(defun enable-hol ()
  (interactive)
  (load "/usr/local/etc/HOL/tools/hol-input")
  (load "/usr/local/etc/HOL/tools/holscript-mode")
  (load "/usr/local/etc/HOL/tools/hol-mode")
  (load "/usr/local/etc/HOL/tools/hol-unicode")
  (use-package holscript-mode
    :custom-face
    (hol-free-variable
     ((t (:inherit font-lock-variable-name-face))))
    (hol-bound-variable
     ((t (:inherit font-lock-constant-face))))
    (holscript-theorem-syntax
     ((t (:inherit font-lock-keyword-face))))
    (holscript-thmname-syntax
     ((t (:inherit font-lock-function-name-face))))
    (holscript-definition-syntax
     ((t (:inherit font-lock-type-face))))
    (holscript-quoted-material
     ((t (:inherit font-lock-string-face))))))
    

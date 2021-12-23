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

(defun logging-disabled-command (&optional cmd _keys)
  (unless cmd (setq cmd this-command))
  (message "%s was disabled." cmd))

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
    (if (called-interactively-p 'interactive)
        (message nowplay)
      nowplay)))

(bind-keys
 :prefix "M-m"
 :prefix-map launchpad-key-map
 ("h" . spacemacs/home)
 ("s" . switch-to-scratch-buffer)
 ("i" . ibuffer)
 ("w" . wl)
 ("f" . make-frame))

(define-key key-translation-map [A-mouse-1]
  (lambda (&optional _prompt)
    (let ((newname 'mouse-2))
      ;; Copy the `event-kind` at the first occasion.
      (unless (get newname 'event-kind)
        (put newname 'event-kind
             (get (car last-input-event) 'event-kind)))
      ;; Modify the event in-place, otherwise we can get a prefix
      ;; added again, so a click on the header-line turns
      ;; into a [header-line header-line <newname>] :-(.
      ;; See fake_prefixed_keys in src/keyboard.c's.
      (setf (car last-input-event) newname)
      (vector last-input-event))))

(add-to-list 'auto-mode-alist '("\\.spad" . prog-mode) t)

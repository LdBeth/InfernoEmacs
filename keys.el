;; bind keys -*- lexical-binding:t -*-
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

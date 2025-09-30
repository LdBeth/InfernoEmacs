;; -*- lexical-binding:t; -*-
(defun find-buffer-or-recentf-candidates ()
  "Return candidates for `find-buffer-or-recentf'."
  (let ((buffers
         (delq nil
               (mapcar (lambda (b)
                         (when (buffer-file-name b)
                           (buffer-file-name b)))
                       (buffer-list)))))
    (append
     buffers
     (cl-remove-if (lambda (f) (member f buffers))
                   (mapcar #'substring-no-properties recentf-list)))))

(defun find-buffer-or-recentf ()
  "Find a buffer visiting a file or file on `recentf-list'."
  (interactive)
  (let ((file (completing-read "Buffer File or Recentf: "
                               (find-buffer-or-recentf-candidates)
                               nil t)))
    (if (bufferp file)
        (switch-to-buffer file)
      (find-file file))))

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

(eval-when-compile (require 'static))

(eval-when-compile
  (defmacro do-applescript (text)
    (cond
     ((functionp 'ns-do-applescript)
      `(ns-do-applescript, text))
     ((functionp 'mac-osa-script)
      `(read (mac-osa-script (eval-when-compile
                               (mac-osa-compile ,text))
                             t nil)))
     (t `(error "Not a mac"))))
  (defmacro call-applescript (file)
    (cond
     ((functionp 'ns-do-applescript)
      `(with-temp-buffer
         (call-process
          "osascript"
          nil t nil
          (expand-file-name ,file user-emacs-directory))
         (string-trim-right (buffer-string))))
     ((functionp 'mac-osa-script)
      `(read (mac-osa-script
              (expand-file-name ,file user-emacs-directory)
              "AppleScript" t)))
     (t `(error "Not a mac")))))

(defun now-browsing ()
  (interactive)
  (let ((url (do-applescript "if application \"Safari\" is running then
  tell application \"Safari\"
    set display to URL of front document
  end tell
else
  set display to \"No opened document.\"
end if")))
    (when (called-interactively-p 'interactive)
      (kill-new url)
      (message "%s" url))
    url))

(defun now-playing ()
  (interactive)
  (let ((nowplay (call-applescript "script/nowplay.scpt")))
    (when (called-interactively-p 'interactive)
      (message "%s" nowplay))
    nowplay))

(defun take-snapshot-from-browser ()
  (interactive)
  (let ((result-buffer (generate-new-buffer "*result*"))
        (error-buffer (generate-new-buffer "*errors*")))
    (with-current-buffer (url-retrieve-synchronously (now-browsing))
      (goto-char (point-min))
      (re-search-forward "\n\n" nil 'move)
      (delete-region (point-min) (point))
      (decode-coding-region (point-min) (point-max)
                            'euc-jp result-buffer))
    (with-current-buffer result-buffer
      (shell-command-on-region
       (point-min) (point-max)
       "ssh ma tidy -utf8 -q -asxhtml --doctype strict --show-comments=no"
       t t error-buffer))
    (switch-to-buffer result-buffer)))

(static-if (boundp 'mac-emulate-three-button-mouse)
    (setq mac-emulate-three-button-mouse t)
  (define-key key-translation-map [s-mouse-1]
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
                  (vector last-input-event)))))

;; (setq mac-right-command-modifier 'meta)

(defun insert-current-time (&optional _prefix)
  (interactive "P")
  (insert (format-time-string "%3a, %02d %3b %Y %02H:%02M:%02S %z")))

(defun gopher-club ()
  (interactive)
  (elpher-go "gopher://gopher.club/1/phlogs/"))

(defun toggle-theme-dark-light ()
  (interactive)
  (if (custom-theme-enabled-p 'spacemacs-light)
      (progn
        (disable-theme 'spacemacs-light)
        (enable-theme 'spacemacs-dark))
    (disable-theme 'spacemacs-dark)
    (enable-theme 'spacemacs-light)))

(defun look-for-dm5-update ()
  (interactive)
  (let ((b (current-buffer)))
    (save-excursion
      (goto-char 0)
      (while (search-forward "www.dm5" nil t)
        (let ((u (thing-at-point-url-at-point))
              (bound (bounds-of-thing-at-point 'url)))
          (url-retrieve u (lambda (&rest _)
                            (if (search-forward
                                 (string-to-unibyte
                                  "\344\270\213\344\270\200\347\253\240")
                                 nil t)
                                (progn
                                  (browse-url u)
                                  (let ((o (make-overlay (car bound) (cdr bound) b))
                                        (m (make-sparse-keymap)))
                                    (define-key m (kbd "C-d")
                                                (lambda ()
                                                  (interactive)
                                                  (delete-overlay o)))
                                    (define-key m (kbd "C-y")
                                                (lambda ()
                                                  (interactive)
                                                  (let ((u (now-browsing)))
                                                    (save-excursion
                                                      (goto-char (overlay-start o))
                                                      (insert u)
                                                      (delete-region (point)
                                                                     (overlay-end o))))))
                                    (overlay-put o 'face 'link)
                                    (overlay-put o 'keymap m)
                                    (overlay-put o 'help-echo "C-y to update, C-d to deactive.")))
                              (message "No updates.")))))
        (sleep-for 0.001)))))

(bind-keys
 :prefix "M-m"
 :prefix-map launchpad-keys
 ("h" . spacemacs/home)
 ("s" . switch-to-scratch-buffer)
 ("r" . find-buffer-or-recentf)
 ("i" . ibuffer)
 ("w" . wl)
 ("f" . make-frame-command)
 ("n" . newsticker-show-news)
 ("g" . gopher-club)
 ("t" . todo-show)
 ("l" . toggle-theme-dark-light))
(bind-keys
 :prefix "M-m p"
 :prefix-map prover-loader
 ("h" . enable-hol)
 ("l" . enable-lean4)
 ("a" . enable-acl2))

(unbind-key "C-h C-h" global-map)
(unbind-key "C-h ?" global-map)

(define-key key-translation-map (kbd "¥") (kbd "\\"))
(defun quail-input-method-translate (fn key)
  (if (eql key ?¥)
      (funcall fn ?\\)
    (funcall fn key)))
(advice-add 'quail-input-method :around #'quail-input-method-translate)

(pixel-scroll-precision-mode 1)
(setq pixel-scroll-precision-interpolate-page t)
(defalias 'scroll-up-command 'pixel-scroll-interpolate-down)
(defalias 'scroll-down-command 'pixel-scroll-interpolate-up)

;; ksh
(setq window-adjust-process-window-size-function
      (lambda (proc win)
        (if (string-match "shell" (process-name proc))
            nil
          (window-adjust-process-window-size-smallest proc win))))

(eval-when-compile
  (require 'use-package)
  (setq use-package-expand-minimally t))

(defun enable-hol ()
  (interactive)
  (progn
    (load "/usr/local/etc/HOL/tools/hol-input" t)
    (load "/usr/local/etc/HOL/tools/holscript-mode" t)
    (load "/usr/local/etc/HOL/tools/hol-mode" t)
    (load "/usr/local/etc/HOL/tools/hol-unicode" t))
  (use-package holscript-mode
    :disabled t
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
    
(defun enable-lean4 ()
  (interactive)
  (add-to-list 'exec-path "~/.elan/bin")
  (add-to-list 'load-path "~/.elan/lean4-mode")
  (with-eval-after-load 'lsp-mode
    (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\.lake\\'"))
  (require 'lean4-mode))

(defun enable-acl2 ()
  (interactive)
  (add-to-list 'load-path "/usr/local/lib/acl2-8.6/books/interface/emacs")
  (require 'acl2-interface))

(defconst ml1--keywords
  '("MCWARN" "MCINS" "MCSKIP" "MCDEF"
    "MCNOWARN" "MCNOINS" "MCNOSKIP" "MCNODEF"
    "MCWARNG" "MCINSG" "MCSKIPG" "MCDEFG"
    "MCSTOP" "MCALTER" "MCLEN" "MCSUB"
    "MCSET" "MCNOTE" "MCGO" "MCPVAR" "MCCVAR"))

(define-minor-mode ml1-mode
  "Add highlighting for ML1 keywords"
  :lighter "ML1"
  (font-lock-add-keywords nil
                          `((,(concat "\\<" (regexp-opt ml1--keywords t) "\\>")
                             . 'font-lock-keyword-face)))

  (if (fboundp 'font-lock-flush)
      (font-lock-flush)
    (when font-lock-mode
      (with-no-warnings (font-lock-fontify-buffer)))))

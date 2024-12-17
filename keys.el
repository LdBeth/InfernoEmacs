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

;(pixel-scroll-precision-mode 1)
;(setq pixel-scroll-precision-interpolate-page t)
;(defalias 'scroll-up-command 'pixel-scroll-interpolate-down)
;(defalias 'scroll-down-command 'pixel-scroll-interpolate-up)

;; Will fix in emacs 30
(defun newsticker--decode-rfc822-date-revision (rfc822-string)
  (if (and rfc822-string (stringp rfc822-string))
      (when (string-match
             (concat
              "\\s-*"
              ;; week day
              "\\(\\(Mon\\|Tue\\|Wed\\|Thu\\|Fri\\|Sat\\|Sun\\)\\s-*,?\\)?\\s-*"
              ;; day
              "\\([0-9]\\{1,2\\}\\)\\s-+"
              ;; month
              "\\(Jan\\|Feb\\|Mar\\|Apr\\|May\\|Jun\\|"
              "Jul\\|Aug\\|Sep\\|Oct\\|Nov\\|Dec\\).*?\\s-+"
              ;; year
              "\\([0-9]\\{2,4\\}\\)"
              ;; time may be missing
              "\\(\\s-+"
              ;; hour
              "\\([0-9]\\{2\\}\\)"
              ;; minute
              ":\\([0-9]\\{2\\}\\)"
              ;; second
              "\\(:\\([0-9]\\{2\\}\\)\\)?"
              ;; zone
              "\\(\\s-+\\("
              "UT\\|GMT\\|EST\\|EDT\\|CST\\|CDT\\|MST\\|MDT\\|PST\\|PDT"
              "\\|\\([-+]\\)\\([0-9]\\{2\\}\\)\\([0-9]\\{2\\}\\)"
              "\\)\\)?"
              "\\)?")
             rfc822-string)
        (let ((day (read (match-string 3 rfc822-string)))
              (month-name (match-string 4 rfc822-string))
              (month 0)
              (year (read (match-string 5 rfc822-string)))
              (hour (read (or (match-string 7 rfc822-string) "0")))
              (minute (read (or (match-string 8 rfc822-string) "0")))
              (second (read (or (match-string 10 rfc822-string) "0")))
              (zone (match-string 12 rfc822-string))
              (sign (match-string 13 rfc822-string))
              (offset-hour (read (or (match-string 14 rfc822-string)
                                     "0")))
              (offset-minute (read (or (match-string 15 rfc822-string)
                                       "0"))))
          (when zone
            (cond ((string= sign "+")
                   (setq hour (- hour offset-hour))
                   (setq minute (- minute offset-minute)))
                  ((string= sign "-")
                   (setq hour (+ hour offset-hour))
                   (setq minute (+ minute offset-minute)))
                  ((or (string= zone "UT") (string= zone "GMT"))
                   nil)
                  ((string= zone "EDT")
                   (setq hour (+ hour 4)))
                  ((or (string= zone "EST") (string= zone "CDT"))
                   (setq hour (+ hour 5)))
                  ((or (string= zone "CST") (string= zone "MDT"))
                   (setq hour (+ hour 6)))
                  ((or (string= zone "MST") (string= zone "PDT"))
                   (setq hour (+ hour 7)))
                  ((string= zone "PST")
                   (setq hour (+ hour 8)))))
          (condition-case error-data
              (let ((i 1))
                (dolist (m '("Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug"
                             "Sep" "Oct" "Nov" "Dec"))
                  (if (string= month-name m)
                      (setq month i))
                  (setq i (1+ i)))
                (encode-time second minute hour day month year t))
            (error
             (message "Cannot decode \"%s\": %s %s" rfc822-string
                      (car error-data) (cdr error-data))
             nil))))
    nil))
(advice-add 'newsticker--decode-rfc822-date
            :override #'newsticker--decode-rfc822-date-revision)

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
    
(defun enable-lean4 ()
  (interactive)
  (add-to-list 'exec-path "/usr/local/etc/lean/bin")
  (add-to-list 'load-path "/Users/ldbeth/.emacs.d/local/lean4-mode")
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

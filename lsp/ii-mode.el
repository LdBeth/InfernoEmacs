;;; ii-mode.el --- Handle files created by irc client ii

;; Copyright (C) Kristoffer Ström

;; Author: Kristoffer Ström <kristoffer@rymdkoloni.se>
;; Created: 20100618
;; Version:
;; Homepage: https://github.com/krl/ii-mode
;; Keywords: irc

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
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; ii-mode is an Emacs mode for handling files created by ii. It can help
;; you to stay logged in even through emacs restarts.

;; The model of using Emacs for interfaces to external programs, rather
;; than running them inside the elisp environment itself also seems more
;; elegant.

;; This allows for a much more detached use of irc, not having to keep one
;; buffer open for each channel, but still getting notified if someone
;; pings you.

;;; Code:

(defvar ii-irc-directory "~/irc/"
  "Directory to look for ii files in. end with slash.")
(defvar ii-prompt-marker nil
  "Marker that keeps track of where the prompt starts")
(defvar ii-prompt-text "ii> "
  "Prompt text used")
(defvar ii-inotify-process nil
  "The inotify process")
(defvar ii-channel-data (make-hash-table :test 'equal)
  "Keeps track of channel data")
(defvar ii-completing-read 'completing-read
  "Which function to use for channel name completion")
(defvar ii-prefered-antishoulder nil
  "Prefered antishoulder file to open")
(defvar ii-window-preshoulder nil
  "Window configuration before shouldering")

(defvar ii-mode-hooks nil)

(defvar ii-ssh-domain nil
  "Set this to have ii-mode run against another domain over ssh")

;; standard notifications
(defvar ii-notifications nil
  "Channel files with notifications")

(defvar ii-notify-regexps nil
  "A list of regexps to match incoming text for notification")

(defvar ii-notify-channels nil
  "A list of channels to recieve special notification love. Uses the shortname form \"server/channel\".")

;; history variables

(defvar ii-history-ring-list '()
    "holds the history")
(defvar ii-tmp-history-ring-list '()
  "copy of the variable `ii-history-ring', that is operated on
until the next insertation onto history-ring")
(defvar ii-history-pos '()
  "holds the current position in history")

(defvar ii-chunk-size (* 256 1024)
  "The size of backlog chunk to paste into buffer")
(defvar ii-backlog-offset nil
  "buffer local variable keeping track of backlog insert offset.")
(defvar ii-topline-buffer nil
  "buffer local variable keeping track of incomplete top line of backlog")
(defvar ii-buffer-logfile nil
  "buffer local variable keeping track of incomplete top line of backlog")

;; fontification
(make-face 'ii-face-nick)
(make-face 'ii-face-date)
(make-face 'ii-face-time)
(make-face 'ii-face-give-voice)
(make-face 'ii-face-take-voice)
(make-face 'ii-face-shadow)
(make-face 'ii-face-prompt)
(make-face 'ii-face-msg)
(make-face 'ii-face-bold)
(make-face 'ii-face-underline)

(set-face-attribute 'ii-face-nick nil :foreground "chocolate2")
(set-face-attribute 'ii-face-date nil :foreground "#999")
(set-face-attribute 'ii-face-time nil :foreground "#bbb")
(set-face-attribute 'ii-face-give-voice nil :foreground "#0ff")
(set-face-attribute 'ii-face-take-voice nil :foreground "#f0f")
(set-face-attribute 'ii-face-shadow nil :foreground "#ccc")
(set-face-attribute 'ii-face-prompt nil :foreground "#0f0")
(set-face-attribute 'ii-face-msg nil :foreground "#fff")
(set-face-attribute 'ii-face-bold nil :bold t)
(set-face-attribute 'ii-face-underline nil :underline t)

(defconst ii-font-lock-keywords
  (list '("^[0-9]+++-[0-9]+-[0-9]+\\ [0-9]+:[0-9]+\\ \+.*?$" 0 'ii-face-give-voice t)
        '("^[0-9]+++-[0-9]+-[0-9]+\\ [0-9]+:[0-9]+\\ -.*?$" 0 'ii-face-take-voice t)
        '("^[0-9]+++-[0-9]+-[0-9]+\\ [0-9]+:[0-9]+\\ -!-.*" 0 'ii-face-shadow t)
        '("^[0-9]+++-[0-9]+-[0-9]+\\ [0-9]+:[0-9]+\\ \<.+\>.*" 0 'ii-face-msg t)
        '("^[0-9]+++-[0-9]+-[0-9]+\\ [0-9]+:[0-9]+\\ \<.*?\>" 0 'ii-face-nick t)
        '("^[0-9]+++-[0-9]+-[0-9]+\\ [0-9]+:[0-9]+" 0 'ii-face-time t)
        '("^[0-9]+++-[0-9]+-[0-9]+" 0 'ii-face-date t)
        '("\C-b.*?\C-b" 0 'ii-face-bold append)
        '("\C-_.*?\C-_" 0 'ii-face-underline append)
        '("^ii>" 0 'ii-face-prompt t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; database/file handling
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ii-add-host (command)
  (if ii-ssh-domain
      (concat "ssh " ii-ssh-domain " " (shell-quote-argument command))
    command))

(defun ii-command-sync (command)
  (shell-command-to-string (ii-add-host command)))

(defun ii-command (command &optional filter stdin)
  (let* ((withhost (ii-add-host 
		    ;; semi hack. if data is sent, touch the active file.
		    (if stdin			
			(concat command " ; touch ~/.ii-active")
		      command)))
	 (command (if stdin
		      (concat "echo -e " (shell-quote-argument stdin) " | " withhost)
		    withhost))
	 (process (start-process-shell-command "ii-command" nil command)))
    (when filter
      (set-process-filter process filter))
    process))

(defun ii-query-file-p (file)
  (string-match (concat "^" ii-irc-directory "[^/]+/[^#&][^/]+/out$") file))

(defun ii-channel-name (name)
  (first (last (split-string (file-name-directory name) "/") 2)))

(defun ii-shortname (long)
  (string-match (concat "^" ii-irc-directory "\\(.*\\)/out$") long)
  (match-string 1 long))

(defun ii-names-to-out (namesfile)
  (concat (substring namesfile 0 -5) "out"))

(defun ii-out-to-names (outfile)
  (concat (substring outfile 0 -3) "names"))

(defun ii-longname (short)
  (concat ii-irc-directory short "/out"))

(defun ii-cache-files ()
  (interactive)
  (dolist (size-string (split-string 
			(ii-command-sync
			 (concat "find " 
				 ii-irc-directory
				 " -name out | xargs stat -c%s\\ %n")) "\n"))
    (unless (string= size-string "")
      (destructuring-bind (size file) (split-string size-string)
	(ii-set-channel-data file 'size (string-to-number size)))))
  ;; cache names
  (dolist (line (split-string (ii-command-sync (concat "find "
						       ii-irc-directory
						       " -name names | xargs grep -e '.*'")) "\n"))
    (let ((file-names (split-string line":")))
      (when (= (length file-names) 2)
	(ii-set-channel-data (first file-names) 'names (split-string (or (second file-names) "")))))))

(defun ii-cache-names-for (outfile)
  (let ((namesfile (ii-out-to-names outfile)))    
    (ii-parse-names namesfile
		    (ii-command-sync (concat "cat " namesfile " 2> /dev/null")))))

(defun ii-get-channels ()
  (remove-if (lambda (x) (string= x "")) ; no empty strings
	     (split-string (ii-command-sync
			    (concat "find " ii-irc-directory " -name out")) "\n")))

(defun ii-parse-names (file data)
  (unless (string= data "")
    (ii-set-channel-data file 'names (split-string data))))

(defun ii-set-channel-data (channel key value)
  (assert (symbolp key))
  (let* ((channel-dir (file-name-directory channel))
	 (channel-data (or (gethash channel-dir ii-channel-data)
			   (puthash channel-dir (make-hash-table) ii-channel-data))))
    (puthash key value channel-data)))

(defun ii-get-channel-data (channel key)
  (let* ((channel-dir (file-name-directory channel))
	 (channel-data (gethash channel-dir ii-channel-data)))
    (when channel-data
      (gethash key channel-data))))

(defun ii-visit-file-among (list)
  "Takes a list of channel filenames and selects one to visit."
  (ii-open-file-buffer (ii-longname
			(funcall ii-completing-read
				 "find: " (mapcar 'ii-shortname list) nil t))))

(defun ii-visit-server-file ()
  "Selects among server channel files"
  (interactive)
  (ii-visit-file-among
   (remove-if-not (lambda (x) (string-match (concat "^" ii-irc-directory "[^/]*/out$") x))
		  (ii-get-channels))))

(defun ii-visit-channel-file ()
  "Selects among all channel files"
  (interactive)
  (ii-visit-file-among (ii-get-channels)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; inotify
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ii-setup-maybe ()
  "If not already running, start the process and setup buffer sizes."
  (unless (and ii-inotify-process
	       (= (process-exit-status ii-inotify-process) 0))
    (ii-cache-files)
    (setf ii-inotify-process
	  ;; get updated files as space separated: newsize path
	  (ii-command (concat "inotifywait -mre close_write --format %w%f "
			      ii-irc-directory
			      " 2> /dev/null | xargs -L1 stat -c%s\\ %n")
		      'ii-handle-inotify))))

(defun ii-restart ()
  (interactive)
  (unless (or (not ii-inotify-process)
	      (eq (process-status ii-inotify-process) 'exit))
      (kill-process ii-inotify-process))
  (setq ii-inotify-process nil)
  (ii-setup-maybe))

(defun ii-handle-inotify (_ data)
  (dolist (line (split-string data "\n"))
    (unless (string= line "")
      (destructuring-bind (new-size file) (split-string line " ")
	;; what kind of file is it?
	(cond ((string= (substring file -3) "out")
	       (ii-get-file-delta file 
			      (string-to-number new-size)
			      'ii-handle-delta))
	      ((string= (substring file -5) "names")
	       (when (ii-get-buffer file)
		 (ii-get-file-chunk file
				    0 (string-to-number new-size)
				    'ii-parse-names))))))))

(defun ii-get-file-chunk (file start-offset length filter)
  ;;(message "g-f-c file: %s start-offset: %i length: %i" file start-offset length)
  ;;(message "ii-get-file-chunk file:%s start-offset:%i length:%i" file start-offset length)
  (lexical-let ((file   file)
		(filter filter)
		(buffer "")
		(count  0)
		(length length))
    (ii-command (format "dd ibs=1 if=%s skip=%i count=%i 2> /dev/null" 
			(shell-quote-argument file) start-offset length)
		(lambda (_ data)
		  ;; (when (string= (substring file 0 -5) "names")
		  ;;   (message "ii-got-chunk %s of %s\n----------\n%s" count length data))

		  (setf buffer (concat buffer data))
		  (incf count (string-bytes data))
		  ;; we might need to cache the data to get it in one chunk
		  ;; TODO this is not very exact, allow a bit of unsharpness
		  ;; with - length 32
		  ;; (message "%i - of %i" count length)
		  (when (>= count (- length 32))
		    (funcall filter file buffer))))))

(defun ii-get-file-chunk-sync (file start-offset length)
  (ii-command-sync (format "dd ibs=1 if=%s skip=%i count=%i 2> /dev/null %s"
			   (shell-quote-argument file) 
			   start-offset
			   length
			   (reduce (lambda (a b) (concat a "| grep -v '" b "' "))
				   ii-censor
				   :initial-value ""))))

(defun ii-get-file-delta (file new-size filter)
  "Gets the end of the file that has grown."
  (let ((old-size (or (ii-get-channel-data file 'size) 0)))
	;; update old value
    (unless (= old-size new-size)
      (ii-set-channel-data file 'size new-size)
      (ii-get-file-chunk file old-size (- new-size old-size) filter))))

(defun ii-handle-delta (file delta)
  "Called when a channel file is written to."
  (when (ii-koscher-p delta)
    (let ((buffer (ii-get-buffer file)))
      (when buffer
	;; Affected file is being changed and visited
	(with-current-buffer buffer
	  (let* ((point-past-prompt (< (1- ii-prompt-marker) (point)))
		 (point-from-end (- (point-max) (point)))
		 (inhibit-read-only t))	    
	    (save-excursion
	      (goto-char ii-prompt-marker)
	      (insert-before-markers (propertize delta 'read-only t)))
	    (when point-past-prompt
	      (goto-char (- (point-max) point-from-end))))))
      ;; Notify! but when? Listen up I'll tell you!
      (when (and (or (not buffer)                      ; either no buffer or
		     (not (get-buffer-window buffer))) ; buffer currently not visible
		 (or (ii-query-file-p file)         ; Either a personal query,
		     (ii-contains-regexp delta)         ; or containing looked-for regexp
		     (ii-special-channel file)))    ; or special channel
	(ii-notify file)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; antishoulder
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ii-antishoulder ()
  (interactive)
  (unless ii-window-preshoulder
    ;; save oldest
    (setq ii-window-preshoulder (current-window-configuration)))
  (delete-other-windows)
  (if ii-prefered-antishoulder
      (find-file ii-prefered-antishoulder)
    (dolist (buf (buffer-list))
      (when (buffer-file-name buf)
	(switch-to-buffer buf)
	(return)))))

(defun ii-postshoulder ()
  (interactive)
  (when (window-configuration-p ii-window-preshoulder)
    (set-window-configuration ii-window-preshoulder)
    (setq ii-window-preshoulder nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-derived-mode ii-mode fundamental-mode "ii" (ii-mode-init))

(defvar ii-mode-map nil)
(setq ii-mode-map (let ((map (make-sparse-keymap)))
		    (define-key map [remap end-of-buffer] 'ii-scroll-to-bottom)
		    (define-key map (kbd "C-a") 'ii-beginning-of-line)
		    (define-key map (kbd "TAB") 'completion-at-point)
		    (define-key map (kbd "M-p") 'ii-history-prev)
		    (define-key map (kbd "M-n") 'ii-history-next)
		    (define-key map (kbd "RET") 'ii-send-message)
		    map))

(defun ii-mode-init ()
  (use-local-map ii-mode-map)

  ;; local variables.
  (set (make-local-variable 'ii-prompt-marker) (make-marker))
  (set (make-local-variable 'ii-backlog-offset) nil)
  (set (make-local-variable 'ii-topline-buffer) nil)
  (make-local-variable 'ii-buffer-logfile)

  ;; bind functions
  (set (make-local-variable 'isearch-wrap-function) 'ii-isearch-autogrow)

  ;; coloring
  (set (make-local-variable 'font-lock-defaults)
       '((ii-font-lock-keywords) t))
  (set (make-local-variable 'font-lock-keywords)
       ii-font-lock-keywords)

  ;; init history-ring
  (ii-history-ring-init)

  ;; add hooks
  (add-hook 'window-configuration-change-hook 'ii-clear-notifications nil t)
  (add-hook 'completion-at-point-functions    'ii-completion-at-point nil t)
  (add-hook 'window-scroll-functions          'ii-window-scroll-function nil t)

  ;; setup

  (ii-setup-maybe)
  (goto-char (point-max))
  (ii-scroll-to-bottom)
  (run-hooks ii-mode-hooks)

  ;; insert prompt and make log readonly.
  (goto-char (point-max))
  (set-marker ii-prompt-marker (point))

  (insert ii-prompt-text)
  (ii-insert-history-chunk)
  (ii-cache-names-for (shell-quote-argument ii-buffer-logfile))

  ;; make it all readonly
  (let ((inhibit-read-only t))
    (put-text-property (point-min) (1+ (point-min)) 'front-sticky t)
    (put-text-property (point-min) (point-max) 'read-only t)
    (put-text-property (1- (point-max)) (point-max) 'rear-nonsticky t)))

(defun ii-scroll-to-bottom ()
  (interactive)
  (end-of-buffer)
  (recenter -1))

(defun ii-window-scroll-function (window display-start)
  "Taken from comint mode, originally ERC. <3 Dirty emacs hackarounds"
  (when (and window (window-live-p window))
    (let ((resize-mini-windows nil))
      (save-selected-window
	(select-window window)
	(save-restriction
	  (with-current-buffer (window-buffer window)
	    (widen)
	    (when (< (1- ii-prompt-marker) (point))
	      (save-excursion
		(recenter -1)
		(sit-for 0)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; completion
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ii-completion-at-point ()
  (list (save-excursion
	  (search-backward-regexp "\\s-")
	  (forward-char)
	  (point))
	(point)
	(or (ii-get-channel-data ii-buffer-logfile 'names) '())))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; movement
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ii-beginning-of-line ()
  (interactive)
  (if (> (point) ii-prompt-marker)
      (goto-char (+ ii-prompt-marker (length ii-prompt-text)))
    (move-beginning-of-line nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	     
;; history
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ii-history-ring-init ()
  "initialize a history ring for current buffer"  
  (set (make-local-variable 'ii-history-ring-list) '())
  (set (make-local-variable 'ii-tmp-history-ring-list) '())
  (set (make-local-variable 'ii-history-pos) 0))

(defun ii-history-ring-access (elem beg end)
  "access the history ring

ELEM should hold an positive or negative integer.
\"-1\" equals return the previous element and \"1\" equals return the next
element in ring

BEG and END should be the beginnig and ending point of prompt"

  (when (equal (length ii-history-ring-list)
               (length ii-tmp-history-ring-list))
    (push "" ii-tmp-history-ring-list))
  (let ((current-line (buffer-substring beg end)))
    (when (and (nth (+ ii-history-pos elem) ii-tmp-history-ring-list)
               (>= (+ ii-history-pos elem) 0))
      (setcar (nthcdr ii-history-pos ii-tmp-history-ring-list) current-line)
      (setq ii-history-pos (+ ii-history-pos elem))
      (delete-region beg end)
      (insert (nth ii-history-pos ii-tmp-history-ring-list)))))

(defun ii-history-ring-add (new)
  "add NEW to history ring"
  (unless (equal new (car ii-history-ring-list))
    (push new ii-history-ring-list))
  (setq ii-tmp-history-ring-list '())
  (setq ii-history-pos 0)
  (setq ii-tmp-history-ring-list (copy-list ii-history-ring-list)))

(defun ii-history-prev ()
  "put the previous message in history-ring at prompt"
  (interactive)
  (ii-history-ring-access 1 (+ ii-prompt-marker (length ii-prompt-text)) (point-max)))

(defun ii-history-next ()
  "put the next message in history-ring at prompt"
  (interactive)
  (ii-history-ring-access -1 (+ ii-prompt-marker (length ii-prompt-text)) (point-max)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; sending messages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ii-send-message ()
  "Sends a message to the 'in' file in channel files directory."
  (interactive)
  (let* ((fifo-in (concat (file-name-directory ii-buffer-logfile) "in"))
         (msg (ii-clear-and-return-prompt)))
    (unless (ii-get-channel-data ii-buffer-logfile 'size)
      (error "Invalid channel directory"))
    (ii-command (concat "cat > " (shell-quote-argument fifo-in)) nil msg)
    (ii-set-channel-data ii-buffer-logfile 'last-write (current-time))
    (ii-history-ring-add msg)))

(defun ii-clear-and-return-prompt ()
  "Returns the content of prompt while clearing it."
  (let* ((start-pos (+ ii-prompt-marker (length ii-prompt-text)))
	 (text (buffer-substring start-pos (point-max))))
    (delete-region start-pos (point-max))
    text))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; notifications
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ii-notify (file)
  (setf ii-notifications (remove file ii-notifications))
  (push file ii-notifications)
  (add-to-list 'global-mode-string "*ii*"))

(defun ii-contains-regexp (lines)
  (some (lambda (x) (string-match x lines)) ii-notify-regexps))

(defun ii-special-channel (filename)
  (member (ii-shortname filename) ii-notify-channels))

(defun ii-visit-notified-file ()
  "Select among notified files"
  (interactive)
  (when (null ii-notifications) (error "No notifications"))
  (ii-visit-file-among ii-notifications))

(defun ii-clear-all-notifications ()
  (interactive)
  (setf ii-notifications nil)
  (ii-clear-notifications))

(defun ii-clear-notifications ()
  "Removes notification on current buffer if any."
  (when (member ii-buffer-logfile ii-notifications)
    (setf ii-notifications
	  (remove ii-buffer-logfile ii-notifications)))
  (when (null ii-notifications) 
    (setf global-mode-string (delete "*ii*" global-mode-string))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; censorship
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ii-koscher-p (string)
  (not (some (lambda (x) (string-match x string)) ii-censor)))

(defun ii-censor (string)
  (reduce (lambda (out line)
	    (concat out
		    (and (not (zerop (length out))) 
			 "\n")
		    (and (ii-koscher-p line)
			 line)))
	  (split-string string "[\n\r]+")
	  :initial-value ""))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; open-partial
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ii-get-buffer (file)
  (when (buffer-live-p (ii-get-channel-data file 'buffer))
    (ii-get-channel-data file 'buffer)))

(defun ii-get-channel-buffer (file)
  (or (ii-get-buffer file)
      (let ((buffer (get-buffer-create (ii-channel-name file))))
	(with-current-buffer buffer
	  (setf ii-buffer-logfile file)
	  (ii-mode)
	  (ii-set-channel-data file 'buffer buffer))
	buffer)))

(defun ii-open-file-buffer (file)
  (ii-postshoulder)
  (switch-to-buffer (ii-get-channel-buffer file)))

(defun ii-insert-history-chunk ()
  "inserts an additional chunk of history into buffer, keeps track of its state through buffer-local variables"
  (let* ((size              (ii-get-channel-data ii-buffer-logfile 'size))
	 (end-offset        (1+ (or ii-backlog-offset size)))
	 (start-offset      (max (- end-offset ii-chunk-size) 0)))
    (ii-insert-text-top
     (ii-get-file-chunk-sync ii-buffer-logfile start-offset (- end-offset start-offset))
     start-offset
     end-offset)))

(defun ii-insert-text-top (data start-offset end-offset)
  (let ((inhibit-read-only t))
    (unless (= end-offset 0)
      (save-excursion
	(goto-char (point-min))
	(save-excursion
	  (insert-before-markers (or ii-topline-buffer "")))
	(goto-char (point-min))
	(save-excursion
	  (insert-before-markers data))
	(unless (= start-offset 0)
	  ;; unless the whole file is read, delete and buffer the topmost line
	  ;; this is to prevent incomplete lines from showing up at the top
	  (save-excursion
	    (goto-char (point-min))
	    (setf ii-topline-buffer (substring (buffer-string) (point) (line-end-position)))
	    (delete-region (point) (1+ (line-end-position)))))
	(setf ii-backlog-offset start-offset)))))

(defun ii-isearch-autogrow ()
  (unless isearch-forward
    (ii-insert-history-chunk)))

;; leverera

(provide 'ii-mode)

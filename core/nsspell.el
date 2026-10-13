;;; nsspell.el --- macOS grammar checking via Flymake  -*- lexical-binding: t; -*-

;; Version: 0.1
;; Package-Requires: ((emacs "29.1"))
;; Keywords: wp, convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

;; A Flymake backend that reports grammar issues found by the macOS
;; text checker (NSSpellChecker), through the `nsspell grammar --server'
;; process.  Spelling is left to ispell/flyspell with
;; `ispell-program-name' set to nsspell.
;;
;; Usage:
;;
;;   (add-hook 'text-mode-hook #'nsspell-grammar-mode)
;;
;; In programming modes only comments and docstrings are checked.
;; `nsspell-grammar-correct' offers the suggested corrections for the
;; issue at point, or ignores it.

;;; Code:

(require 'flymake)
(require 'seq)

(defgroup nsspell nil
  "Grammar checking with the macOS text checker."
  :group 'flymake
  :prefix "nsspell-")

(defcustom nsspell-program "nsspell"
  "Name or path of the nsspell executable."
  :type 'string)

(defcustom nsspell-grammar-full t
  "Non-nil means wait for the full grammar analysis.
The full analysis finds more issues but takes seconds per paragraph
the first time it is seen; nil reports only results available at once."
  :type 'boolean)

(defcustom nsspell-grammar-check-spelling nil
  "Non-nil means also report misspelled words as grammar diagnostics."
  :type 'boolean)

(defcustom nsspell-grammar-language nil
  "Language for grammar checking, as accepted by \"nsspell -d\".
If nil, use `ispell-local-dictionary' or `ispell-dictionary', and
failing those let the system choose."
  :type '(choice (const :tag "Default" nil) string))

(defface nsspell-grammar-face
  '((((supports :underline (:style wave)) (background light))
     :underline (:style wave :color "DodgerBlue3"))
    (((supports :underline (:style wave)) (background dark))
     :underline (:style wave :color "turquoise3"))
    (t :underline t))
  "Face for grammar issues.")

(put 'nsspell-grammar 'flymake-category 'flymake-note)
(put 'nsspell-grammar 'flymake-type-name "grammar")
(put 'nsspell-grammar 'face 'nsspell-grammar-face)
(put 'nsspell-grammar 'flymake-bitmap 'flymake-note-bitmap)

;;;; Server process

(defvar nsspell--process nil
  "The shared \"nsspell grammar --server\" process.")

(defvar nsspell--next-id 0
  "Id of the last request sent.")

(defvar nsspell--pending (make-hash-table :test #'eql)
  "Map from request id to (BUFFER . REPORT-FN).")

(defvar nsspell--latest (make-hash-table :test #'eq :weakness 'key)
  "Map from buffer to the id of its latest check request.")

(defun nsspell--ensure-process ()
  "Return the live server process, starting it if needed."
  (unless (process-live-p nsspell--process)
    (setq nsspell--process
          (make-process :name "nsspell"
                        :command (list nsspell-program "grammar" "--server")
                        :connection-type 'pipe
                        :coding 'utf-8-unix
                        :noquery t
                        :stderr (get-buffer-create " *nsspell-stderr*")
                        :filter #'nsspell--filter
                        :sentinel #'nsspell--sentinel))
    (process-put nsspell--process 'nsspell-partial ""))
  nsspell--process)

(defun nsspell--send (request)
  "Send REQUEST, a plist, to the server."
  (process-send-string (nsspell--ensure-process)
                       (concat (json-serialize request) "\n")))

(defun nsspell--filter (process string)
  "Collect complete JSON lines from PROCESS output STRING."
  (let ((data (concat (process-get process 'nsspell-partial) string))
        (start 0))
    (while (string-match "\n" data start)
      (let ((line (substring data start (match-beginning 0))))
        (setq start (match-end 0))
        (unless (string-blank-p line)
          (with-demoted-errors "nsspell: %S"
            (nsspell--dispatch
             (json-parse-string line :object-type 'plist :array-type 'list
                                :null-object nil :false-object nil))))))
    (process-put process 'nsspell-partial (substring data start))))

(defun nsspell--sentinel (process _event)
  "Fail pending checks when PROCESS dies."
  (unless (process-live-p process)
    (maphash (lambda (_id entry)
               (funcall (cdr entry) :panic
                        :explanation "nsspell server exited"))
             nsspell--pending)
    (clrhash nsspell--pending)))

(defun nsspell--dispatch (reply)
  "Handle REPLY, a plist parsed from a server line."
  (when-let* ((id (plist-get reply :id))
              (entry (gethash id nsspell--pending)))
    (remhash id nsspell--pending)
    (let ((buffer (car entry))
          (report-fn (cdr entry)))
      ;; A newer request for the same buffer supersedes this one.
      (when (and (buffer-live-p buffer)
                 (eql id (gethash buffer nsspell--latest)))
        (if-let* ((message (plist-get reply :error)))
            (funcall report-fn :panic :explanation message)
          (funcall report-fn
                   (nsspell--diagnostics buffer (plist-get reply :issues))))))))

(defun nsspell--diagnostics (buffer issues)
  "Make Flymake diagnostics in BUFFER from server ISSUES."
  (with-current-buffer buffer
    (save-restriction
      (widen)
      (let ((min (point-min)) (max (point-max)) diagnostics)
        (dolist (issue issues)
          (let* ((beg (min max (max min (plist-get issue :start))))
                 (end (min max (max beg (plist-get issue :end)))))
            (when (= beg end) (setq end (min max (1+ beg))))
            (when (< beg end)
              (push (flymake-make-diagnostic
                     buffer beg end 'nsspell-grammar
                     (plist-get issue :message)
                     (list :corrections (plist-get issue :corrections)
                           :uuid (plist-get issue :uuid)))
                    diagnostics))))
        (nreverse diagnostics)))))

;;;###autoload
(defun nsspell-kill-server ()
  "Stop the nsspell server; it restarts on the next check."
  (interactive)
  (when (process-live-p nsspell--process)
    (delete-process nsspell--process))
  (setq nsspell--process nil)
  (clrhash nsspell--pending))

;;;; Text to check

(defun nsspell--language ()
  "Language name to send with requests, or nil."
  (or nsspell-grammar-language
      (bound-and-true-p ispell-local-dictionary)
      (bound-and-true-p ispell-dictionary)))

(defun nsspell--blank (string from to)
  "Replace characters FROM..TO of STRING with spaces, in place."
  (let ((i (max 0 from)) (to (min (length string) to)))
    (while (< i to)
      (aset string i ?\s)
      (setq i (1+ i)))))

(defun nsspell--comment-segment (beg end)
  "Segment for the comment block BEG..END with comment starters blanked."
  (let ((text (buffer-substring-no-properties beg end)))
    (save-excursion
      (goto-char beg)
      (while (< (point) end)
        (let* ((bol (point))
               (eol (min end (line-end-position)))
               (cs (comment-search-forward eol t)))
          (if (and cs (>= cs bol))
              (nsspell--blank text (- cs beg) (- (min (point) eol) beg))
            ;; Continuation line of a block comment, e.g. " * text".
            (goto-char bol)
            (when (looking-at "[ \t]*\\*+")
              (nsspell--blank text (- bol beg) (- (min (match-end 0) eol) beg))))
          (goto-char bol)
          (forward-line 1))))
    (list :start beg :text text)))

(defun nsspell--doc-face-p (pos)
  "Non-nil if POS has `font-lock-doc-face'."
  (let ((face (get-text-property pos 'face)))
    (if (listp face) (memq 'font-lock-doc-face face) (eq face 'font-lock-doc-face))))

(defun nsspell--prog-segments ()
  "Segments for the comments and docstrings of the current buffer."
  (let (segments cs)
    (goto-char (point-min))
    (while (setq cs (comment-search-forward (point-max) t))
      (goto-char cs)
      (forward-comment (buffer-size))
      (skip-chars-backward " \t\n")
      (let ((end (max cs (point))))
        (when (> end cs)
          (push (nsspell--comment-segment cs end) segments))
        (goto-char (max end (1+ cs)))))
    (when font-lock-mode
      (font-lock-ensure)
      (let ((pos (point-min)) end)
        (while (< pos (point-max))
          (setq end (next-single-property-change pos 'face nil (point-max)))
          (when (nsspell--doc-face-p pos)
            (while (and (< end (point-max)) (nsspell--doc-face-p end))
              (setq end (next-single-property-change end 'face nil (point-max))))
            (push (list :start pos :text (buffer-substring-no-properties pos end))
                  segments))
          (setq pos end))))
    (sort segments (lambda (a b) (< (plist-get a :start) (plist-get b :start))))))

(defun nsspell--segments ()
  "Segments of the current buffer to check, as plists."
  (save-excursion
    (save-restriction
      (widen)
      (if (derived-mode-p 'prog-mode)
          (nsspell--prog-segments)
        (list (list :start (point-min)
                    :text (buffer-substring-no-properties (point-min) (point-max))))))))

;;;; Flymake backend

(defun nsspell-grammar-flymake (report-fn &rest _args)
  "Flymake backend reporting macOS grammar issues to REPORT-FN."
  (let* ((id (setq nsspell--next-id (1+ nsspell--next-id)))
         (tex (derived-mode-p 'tex-mode 'latex-mode 'TeX-mode))
         (request (list :op "check" :id id
                        :full (if nsspell-grammar-full t :false)
                        :spelling (if nsspell-grammar-check-spelling t :false)
                        :tex (if tex t :false)
                        :segments (vconcat (nsspell--segments))))
         (language (nsspell--language)))
    (when language
      (setq request (plist-put request :lang language)))
    (puthash id (cons (current-buffer) report-fn) nsspell--pending)
    (puthash (current-buffer) id nsspell--latest)
    (nsspell--send request)))

;; The backend only sends buffer text to the local checker; it never
;; evaluates it, so it is safe in buffers that are not trusted.
(function-put 'nsspell-grammar-flymake 'flymake-always-safe t)

(defun nsspell--diagnostics-at (pos)
  "Grammar diagnostics at POS."
  (seq-filter (lambda (d) (eq (flymake-diagnostic-type d) 'nsspell-grammar))
              (flymake-diagnostics pos)))

;;;###autoload
(defun nsspell-grammar-correct (&optional event)
  "Correct or ignore the grammar issue at point.
When called from a mouse EVENT, use a popup menu at the click."
  (interactive (list (and (mouse-event-p last-input-event) last-input-event)))
  (when event
    (posn-set-point (event-start event)))
  (let ((diagnostic (car (nsspell--diagnostics-at (point)))))
    (unless diagnostic
      (user-error "No grammar issue here"))
    (let* ((data (flymake-diagnostic-data diagnostic))
           (ignore "[Ignore]")
           (choices (append (plist-get data :corrections) (list ignore)))
           (text (flymake-diagnostic-text diagnostic))
           (choice (if event
                       (x-popup-menu event
                                     (list "Grammar"
                                           (cons text (mapcar (lambda (c) (cons c c))
                                                              choices))))
                     (completing-read (concat text " ") choices nil t))))
      (cond
       ((or (null choice) (equal choice "")))
       ((equal choice ignore)
        (when-let* ((uuid (plist-get data :uuid)))
          (nsspell--send (list :op "ignore" :uuid uuid)))
        (flymake-start))
       (t
        (let ((beg (flymake-diagnostic-beg diagnostic))
              (end (flymake-diagnostic-end diagnostic)))
          (save-excursion
            (goto-char beg)
            (delete-region beg end)
            (insert choice)))
        (flymake-start))))))

;;;###autoload
(defun nsspell-grammar-next (&optional n)
  "Go to the Nth next grammar issue."
  (interactive "p")
  (flymake-goto-next-error n '(nsspell-grammar) t))

;;;###autoload
(defun nsspell-grammar-previous (&optional n)
  "Go to the Nth previous grammar issue."
  (interactive "p")
  (flymake-goto-prev-error n '(nsspell-grammar) t))

;; `C-c' followed by punctuation is reserved for minor modes; `$'
;; echoes `ispell-word' on M-$.
(defvar-keymap nsspell-grammar-mode-map
  "C-c $" #'nsspell-grammar-correct)

;;;###autoload
(define-minor-mode nsspell-grammar-mode
  "Check grammar with the macOS text checker, using Flymake."
  :lighter " Grammar"
  :keymap nsspell-grammar-mode-map
  (if nsspell-grammar-mode
      (progn
        (add-hook 'flymake-diagnostic-functions #'nsspell-grammar-flymake nil t)
        (if flymake-mode (flymake-start) (flymake-mode 1)))
    (remove-hook 'flymake-diagnostic-functions #'nsspell-grammar-flymake t)
    (when flymake-mode (flymake-start))))

(provide 'nsspell)
;;; nsspell.el ends here

;; -*- lexical-binding:t -*-
(defgroup paren-face nil
  "Face for parentheses in lisp modes."
  :group 'font-lock-extra-types
  :group 'faces)

(defface parenthesis '((t (:inherit shadow)))
  "Face for parentheses in lisp modes.
This face is only used if `paren-face-mode' is turned on.
See `global-paren-face-mode' for an easy way to do so."
  :group 'paren-face)

(defcustom paren-face-modes
  '(lisp-mode
    emacs-lisp-mode lisp-interaction-mode ielm-mode
    scheme-mode inferior-scheme-mode
    clojure-mode cider-repl-mode nrepl-mode
    arc-mode inferior-arc-mode)
  "Major modes in which `paren-face-mode' should be turned on.
When `global-paren-face-mode' is turned on, the buffer-local mode
is turned on in all buffers whose major mode is or derives from
one of the modes listed here."
  :type '(repeat symbol)
  :group 'paren-face)

(defcustom paren-face-regexp "[][()]"
  "Regular expression to match parentheses."
  :type 'regexp
  :group 'paren-face)

(defcustom paren-face-mode-lighter ""
  "String to display in the mode line when `paren-face-mode' is active."
  :type 'string
  :group 'paren-face)

;;;###autoload
(define-minor-mode paren-face-mode
  "Use a dedicated face just for parentheses."
  :lighter paren-face-mode-lighter
  (let ((keywords `((,paren-face-regexp 0 'parenthesis))))
    (if paren-face-mode
        (font-lock-add-keywords  nil keywords)
      (font-lock-remove-keywords nil keywords)))
  (when font-lock-mode
    (if (and (fboundp 'font-lock-flush)
             (fboundp 'font-lock-ensure))
        (save-restriction
          (widen)
          (font-lock-flush)
          (font-lock-ensure))
      (with-no-warnings
        (font-lock-fontify-buffer)))))

;;;###autoload
(define-globalized-minor-mode global-paren-face-mode
  paren-face-mode turn-on-paren-face-mode-if-desired
  :group 'paren-face)

(defun turn-on-paren-face-mode-if-desired ()
  (when (apply #'derived-mode-p paren-face-modes)
    (paren-face-mode 1)))

(provide 'dim-paren)

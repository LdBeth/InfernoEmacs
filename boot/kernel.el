;; First we are introducing the macro `eval-when-compile', this macro
;; can avoid the compilation of certain codes.
(eval-when-compile
  (unless (featurep 'core-lib)
    ;; `signal' is a good way to stop and fire an error
    ;; at this point.
    (signal 'quit '("kernel.el is not compiled and `core-lib' have not loaded.
You may run install script to bootstrap inferno firest."))))

;;; Pre-init
(setq message-log-max 16384)
(defconst emacs-init-time (current-time))

;; See `core-lib' and docstring.
(self-byte-compile)



;;; Load Path
(defun add-to-load-path (&rest dir)
  "Add DIRs to `load-path'."
  (let ((arg (car dir)))
    (when arg
      (add-to-list 'load-path arg)
      (add-to-load-path (cdr dir)))))

(defun add-to-load-path-if-exists (&rest dir)
  "Add existed DIRs to `load-path'."
  (dolist (x dir)
          (when (file-exists-p x)
            (add-to-load-path x))))

(defvar inferno-start-directory
  user-emacs-directory
  "Inferno start directory.")

;; Also see `core-lib' and docstring.
(repeat-declare-syntax defconst
    (lambda (dir) (expand-file-name dir inferno-start-directory))
  (inferno-init-directory
   "boot/"
   "Inferno essentials.")
  (inferno-lib-directory
   "lib/"
   "The function and variable library of inferno.")
  (inferno-config-directory
   "etc/"
   "Per-package related configurations.")
  (inferno-lisp-directory
   "lsp/"
   "Additional lisp scripts.")
  (inferno-resource-directory
    "share/"
    "Where the extra resources go.")
  (inferno-user-directory
   "usr/"
   "Users own \".emacs.d\"."))

(add-to-load-path
 inferno-init-directory
 inferno-lib-directory
 inferno-config-directory)



;;; Init

;; Load generated autoloads files.
(load "inferno-autoloads")
(repeat-declare-syntax require nil
  'inferno-pkg)

(defgroup inferno nil
  "Inferno customizations."
  :group 'starter-kit
  :prefix 'inferno-)

(provide 'inferno-kernel)

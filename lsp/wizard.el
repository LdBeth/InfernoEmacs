;;;###autoload
(defun inferno-gen-autoloads ()
  "Generate autoloads files for inferno."
  (let ((generated-autoload-file
         (expand-file-name "inferno-autoloads.el" inferno-init-directory)))
    (update-directory-autoloads
     inferno-init-directory
     inferno-lib-directory
     inferno-lisp-directory)))

(provide 'wizard)

;;; Bootstrap
(cl-eval-when 'eval; avoid compiltion into bytecode.

  (provide 'wizard '(install))

  (defun load-bootstrap-files ()
    "Load neccessary bootstrap files."
    (unless (featurep 'inferno-kernel)
      (let ((path (read-file-name "Install Path?"
                                  (file-name-directory load-file-name)
                                  user-emacs-directory)))
        (load-file (expand-file-name "lib/core-lib.el" path))
        (load-file (expand-file-name "boot/kernel.el" path)))))

  (defun install-straight ()
    "Bootstrap and install straight.el, the package manager."
    (interactive)
    (let ((bootstrap-file (concat inferno-start-directory "straight/bootstrap.el"))
          (bootstrap-version 2))
      (unless (file-exists-p bootstrap-file)
        (with-current-buffer
            (url-retrieve-synchronously
             "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
             'silent 'inhibit-cookies)
          (goto-char (point-max))
          (eval-print-last-sexp)))
      (load bootstrap-file nil 'nomessage)))

  (defun install-inferno ()
    "Bootstrap and install inferno."
    (interactive)
    (inferno-gen-autoloads)
    (install-straight)
    (load-bootstrap-files))
  (message "Installation wizard loaded. Excute `install-inferno' to bootstrap inferno."))

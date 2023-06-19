(package-initialize)
(load (concat user-emacs-directory "pkgs"))
(dolist (package
         '(mwim unfill ctrlf orderless
                which-key bind-key
                emacs-gc-stats))
  (require package))
(defconst dumped-load-path load-path)
(garbage-collect)
(dump-emacs-portable "~/.emacs.d/Emacs.pdmp")

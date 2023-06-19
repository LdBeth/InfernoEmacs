(package-initialize)
(load (concat user-emacs-directory "pkgs"))
(dolist (package
         '(mwim unfill ctrlf
                which-key bind-key))
  (require package))
(defconst dumped-load-path load-path)
(garbage-collect)
(dump-emacs-portable "~/.emacs.d/Emacs.pdmp")

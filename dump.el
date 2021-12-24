(package-initialize)
(load (concat user-emacs-directory "pkgs"))
(dolist (package
         '(mwim unfill hl-todo smex swiper
                which-key paren-face
                bind-key))
  (require package))
(defconst dumped-load-path load-path)
(garbage-collect)
(dump-emacs-portable "~/.emacs.d/Emacs.pdmp")

(load (concat user-emacs-directory "pkgs"))
(dolist (package
         '(mwim unfill hl-todo smex diminish swiper
                which-key paren-face
                bind-key))
  (require package))
(dolist (elt (package--alist))
  (package-activate (car elt) t))
(print load-path)
(defconst dumped-load-path load-path)
(garbage-collect)
(dump-emacs-portable "~/.emacs.d/Emacs.pdmp")

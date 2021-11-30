;; Simple config

;;(package-initialize)

;; init
(load (concat user-emacs-directory "params"))

(require 'package)
(add-to-list
   'package-archives
   '("melpa" . "https://melpa.org/packages/") t)

(require 'use-package)
(load (concat user-emacs-directory "pkgs"))
(load (concat user-emacs-directory "core/spacemacs-buffer"))

(spacemacs-buffer/goto-buffer)
(spacemacs-buffer//startup-hook)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" default))
 '(package-selected-packages
   '(hl-todo sly smex diminish which-key counsel ivy paren-face use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Simple config

;;(package-initialize)

(setq gc-cons-threshold 402653184 gc-cons-percentage 0.6)
;; init
(load (concat user-emacs-directory "params"))

;(require 'use-package)
(load (concat user-emacs-directory "pkgs"))
(load (concat user-emacs-directory "core/spacemacs-buffer"))
(load (concat user-emacs-directory "keys"))

(spacemacs-buffer/goto-buffer)
(spacemacs-buffer//startup-hook)

(setq spacemacs-initialized t)
(setq gc-cons-threshold 10485760
      gc-cons-percentage 0.1)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" default))
 '(package-selected-packages
   '(notmuch gnu-elpa-keyring-update pinentry mu-cite wanderlust flyspell-correct-ivy mwim unfill hl-todo sly smex diminish which-key counsel ivy paren-face use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

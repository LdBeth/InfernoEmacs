;; Simple config

;;(package-initialize)

(setq gc-cons-threshold 402653184 gc-cons-percentage 0.6)

(load (concat user-emacs-directory "params"))
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
   '("512fe37090a702098710d2a33ab1aad365d7f7e0063fe6afb75b20df25f16903" "4f87934838378e5bdd9a85a8ef46eb75e55a68b0a400a44ea45a3fd3c9167562" default))
 '(package-selected-packages
   '(flycheck abc-mode elpher rnc-mode maude-mode corfu dyalog-mode filladapt gnu-elpa-keyring-update pinentry wanderlust flyspell-correct-ivy mwim unfill sly smex which-key counsel ivy paren-face use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(dyalog-apl-char ((t (:inherit font-lock-builtin-face))) t))

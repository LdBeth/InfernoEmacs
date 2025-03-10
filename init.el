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
   '("58500d06b2e3d12c038c153f568b244611d584230fb04324b995c16b3e886dd4" "512fe37090a702098710d2a33ab1aad365d7f7e0063fe6afb75b20df25f16903" "4f87934838378e5bdd9a85a8ef46eb75e55a68b0a400a44ea45a3fd3c9167562" default))
 '(org-fold-core-style 'overlays)
 '(package-selected-packages
   '(avy geiser-chez hyperbole yasnippet langtool flyspell-correct frimacs orderless vertico ctrlf sudo-edit z3-mode xquery-mode emmet-mode elpher maude-mode corfu dyalog-mode filladapt gnu-elpa-keyring-update pinentry wanderlust mwim unfill sly which-key))
 '(safe-local-variable-values
   '((xquery-tool-xml-catalog-file . "/Users/ldbeth/Public/Projects/symbdoc/catalog.xml")
     (xquery-tool-resolve-xincludes . t)))
 '(warning-suppress-types '((auto-save))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

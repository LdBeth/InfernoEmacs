;; Simple config  -*- lexical-binding: t; -*-

(let ((gc-cons-threshold 402653184)
      (gc-cons-percentage 0.6))

  (load (concat user-emacs-directory "params"))
  (load (concat user-emacs-directory "keys")))

(spacemacs-buffer/goto-buffer)
(spacemacs-buffer//startup-hook)

(setq spacemacs-initialized t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("58500d06b2e3d12c038c153f568b244611d584230fb04324b995c16b3e886dd4"
     "512fe37090a702098710d2a33ab1aad365d7f7e0063fe6afb75b20df25f16903"
     "4f87934838378e5bdd9a85a8ef46eb75e55a68b0a400a44ea45a3fd3c9167562"
     default))
 '(hsys-org-enable-smart-keys t)
 '(org-fold-core-style 'overlays)
 '(package-selected-packages
   '(auctex avy corfu ctrlf dash dyalog-mode elpher emmet-mode filladapt
            flyspell-correct frimacs geiser-chez
            gnu-elpa-keyring-update gptel hyperbole langtool lsp-mode
            magit-section maude-mode mwim orderless rime sly sudo-edit
            swift-mode unfill vertico wanderlust which-key xquery-mode
            yasnippet z3-mode))
 '(safe-local-variable-values
   '((TeX-engine . aptex)
     (xquery-tool-xml-catalog-file
      . "/Users/ldbeth/Public/Projects/symbdoc/catalog.xml")
     (xquery-tool-resolve-xincludes . t)))
 '(warning-suppress-types '((lsp-mode) (auto-save))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(use-package diminish
  :config
  (diminish 'eldoc-mode))

(use-package spacemacs-common
  :load-path "core/spacemacs-theme/"
  :config
  (load-theme 'spacemacs-dark t))

(use-package paren-face
  :defer t
  :config
  (global-paren-face-mode 1))

(use-package hl-todo
  :defer t
  :init
  (global-hl-todo-mode 1))

(use-package ivy
  :diminish ivy-mode
  :config
  (ivy-mode))

(use-package smex
  :defer t
  :init (setq smex-history-length 32))

(use-package counsel
  :diminish counsel-mode
  :config
  (counsel-mode))

(use-package swiper
  :defer t
  :bind
  ("C-s" . swiper))

(use-package which-key
  :diminish which-key-mode
  :config
  (setq which-key-show-early-on-C-h t
	which-key-idle-secondary-delay 0.05)
  (which-key-mode 1))

(use-package page-break-lines
  :load-path "core/"
  :diminish page-break-lines-mode
  :config
  (global-page-break-lines-mode 1))

;; Programming
(use-package sly
  :defer t
  :init
  (setq inferior-lisp-program "ccl"))

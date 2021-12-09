;; -*- lexical-binding:t -*-
(require 'bind-key)

(use-package diminish
  :config
  (diminish 'eldoc-mode))

(use-package spacemacs-common
  :load-path "core/spacemacs-theme/"
  :config
  (load-theme 'spacemacs-dark t))

(use-package paren-face
  :config
  (global-paren-face-mode 1))

(use-package hl-todo
  :defer t
  :init
  (global-hl-todo-mode 1))

(use-package ivy
  :diminish ivy-mode
  :init
  (setq ivy-use-virtual-buffers t)
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
  ([remap isearch-forward] . swiper-isearch))

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

;; Better defaults
(use-package mwim
  :defer t
  :bind (("C-a" . mwim-beginning-of-code-or-line)
	 ("C-e" . mwim-end-of-code-or-line)))

(use-package unfill
  :defer t
  :commands (unfill-region unfill-paragraph unfill-toggle)
  :bind
  ([remap fill-paragraph] . unfill-toggle))

;; Spell
(use-package flyspell
  :defer t
  :diminish " s"
  :bind (:map flyspell-mode-map
              ("C-;" . flyspell-correct-wrapper)))

;; Mail
(use-package wl
  :defer t
  :init
  (progn
    (setq read-mail-command 'wl
          mail-user-agent 'wl-user-agent
          mail-envelope-from 'header
          mail-specify-envelope-from t
          mime-header-accept-quoted-encoded-words t)
    (define-mail-user-agent
        'wl-user-agent
        'wl-user-agent-compose
        'wl-draft-send
        'wl-draft-kill
        'mail-send-hook)))

(use-package mu-cite
  :disabled
  :defer t
  :init
  (add-hook 'mail-citation-hook 'mu-cite-original))

(use-package x-face-e21
  :defer t
  :commands (x-face-decode-message-header)
  :init
  (setq x-face-image-attributes
        '((light :ascent 80 :foreground "#655370")
          (dark :ascent 80 :foreground "#292b2e" :background "#b2b2b2"))))

;; Programming
(use-package sly
  :defer t
  :init
  (setq inferior-lisp-program "ccl"))

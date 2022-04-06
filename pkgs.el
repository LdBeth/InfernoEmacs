;; -*- lexical-binding:t -*-
(require 'bind-key)
(eval-when-compile
  (require 'use-package)
  (setq use-package-expand-minimally t))

(use-package core-autoloads
  :load-path "core/")

(use-package diminish
  :disabled
  :config
  (diminish 'eldoc-mode)
  (diminish 'visual-line-mode))

(use-package spacemacs-theme-autoloads
  :load-path "core/spacemacs-theme/"
  :config
  (load-theme 'spacemacs-dark t)
  (load-theme 'spacemacs-light t t))

(use-package spacemacs-buffer
  :init
  (defvar dotspacemacs-banner
    (eval-when-compile
      (create-image (concat user-emacs-directory "banner.xpm")
                    'xpm nil)))
  (defvar dotspacemacs-startup-lists
    '((bookmarks . 3) (recents  . 5))))

(use-package tecoline
  :config
  (setq-default mode-line-buffer-identification
                '(:propertize "%12b" face nano-modeline-name)
                mode-line-format (nano-modeline-default-mode)))

(use-package paren-face
  :config
  (global-paren-face-mode 1))

(use-package hl-todo
  :disabled
  :config
  (global-hl-todo-mode 1))

(use-package ivy
  ; :diminish ivy-mode
  :init
  (setq ivy-use-virtual-buffers t
        ivy-use-selectable-prompt t
        ivy-do-completion-in-region nil)
  :config
  (ivy-mode))

(use-package smex
  :defer t
  :init (setq smex-history-length 32))

(use-package counsel
  ; :diminish counsel-mode
  :config
  (counsel-mode))

(use-package swiper
  :defer t
  :bind
  ([remap isearch-forward] . swiper-isearch))

(use-package which-key
  :config
  (setq which-key-show-early-on-C-h t
	which-key-idle-secondary-delay 0.05)
  (which-key-mode 1))

(use-package page-break-lines
  :config
  (global-page-break-lines-mode 1))

;; Better defaults
(use-package mwim
  :defer t
  :bind (("C-a" . mwim-beginning-of-code-or-line)
	 ("C-e" . mwim-end-of-code-or-line)))

(use-package unfill
  :defer t
  :bind
  ([remap fill-paragraph] . unfill-toggle))

(use-package filladapt
  :init
  (setq-default filladapt-mode t))

;; Spell
(use-package flyspell
  :defer t
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
  :defer t
  :init
  (setq mu-cite-prefix-format '(lsdb-prefix-register "> "))
  (add-hook 'mail-citation-hook 'mu-cite-original)
  :config
  (lsdb-mu-insinuate))

(use-package x-face-e21
  :defer t
  :init
  (setq x-face-image-attributes
        '((light :ascent 80 :foreground "#655370")
          (dark :ascent 80 :foreground "#292b2e" :background "#b2b2b2"))))

(use-package lsdb
  :defer t)

;; Programming
(use-package sly
  :defer t
  :init
  (setq inferior-lisp-program "ccl"))

(use-package dyalog-mode
  :defer t
  :init
  (add-to-list 'auto-mode-alist '("\\.apl[afno]" . dyalog-mode)))

;; Net
(use-package aria2
  :defer t)

(use-package newst-plainview
  :defer t
  :init
  (setq newsticker-retrieval-interval -1
        newsticker-frontend 'newsticker-plainview
        newsticker-download-logos nil)
  :config
  (defun newsticker-setup-mode-line ()
    (setq mode-line-format
          (nano-modeline-compose
           '(newsticker--buffer-uptodate-p
             (:propertize " SC " face nano-modeline-status-RO)
             (:propertize " NI " face nano-modeline-status-**))
           '(:propertize " Newsticker " face nano-modeline-name)
           '(:eval (format " %d " (length newsticker--process-ids)))
           '((-3 "%p") " "
             (:eval (newsticker--buffer-get-feed-title-at-point))
             ": "
             (:eval (newsticker--buffer-get-item-title-at-point))))))
  (add-hook
   'newsticker-mode-hook 'newsticker-setup-mode-line))

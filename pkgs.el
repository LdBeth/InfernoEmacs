;; -*- lexical-binding:t -*-
(require 'bind-key)
(eval-when-compile
  (require 'use-package)
  (setq use-package-expand-minimally t))

(use-package core-autoloads
  :load-path "core/")

(use-package spacemacs-theme-autoloads
  :load-path "core/spacemacs-theme/"
  :config
  (load-theme 'spacemacs-dark t)
  (load-theme 'spacemacs-light t t))

(use-package spacemacs-buffer
  :init
  (defconst tecomacs-banner
    (eval-when-compile
      (create-image (concat user-emacs-directory "banner.xpm")
                    'xpm nil)))
  (defconst tecomacs-title "[T E C O M A C S]")
  (defconst tecomacs-startup-lists
    '((bookmarks . 3) (recents  . 5))))

(use-package tecoline
  :config
  (setq-default mode-line-buffer-identification
                '(:propertize "%12b" face nano-modeline-name)
                mode-line-format (nano-modeline-default-mode)))

(use-package dim-paren
  :config
  (global-paren-face-mode 1))

(use-package hl-todo
  :disabled
  :config
  (global-hl-todo-mode 1))

(use-package ivy
  ; :diminish ivy-mode
  :disabled
  :init
  (setq ivy-use-virtual-buffers t
        ivy-use-selectable-prompt t
        ivy-do-completion-in-region nil)
  :config
  (ivy-mode))

(use-package vertico-directory
  :after vertico
  :ensure nil
  ;; More convenient directory navigation commands
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  ;; Tidy shadowed file names
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

(use-package smex
  :disabled
  :defer t
  :init (setq smex-history-length 32))

(use-package counsel
  ; :diminish counsel-mode
  :disabled
  :config
  (counsel-mode))

(use-package ctrlf
  :config
  (ctrlf-mode 1))

(use-package which-key
  :config
  (setq which-key-show-early-on-C-h t
        which-key-idle-secondary-delay 0.05)
  (which-key-add-key-based-replacements
    "C-x RET" "language"
    "C-x 8" "unicode"
    "C-x 8 e" "emoji"
    "C-x a" "abbrev"
    "C-x n" "narrow"
    "C-x p" "project"
    "C-x r" "register"
    "C-x t" "tab"
    "C-x w" "window"
    "C-x w ^" "detach"
    "C-x x" "buffer")
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
  :hook (text-mode . flyspell-mode)
  :bind (:map flyspell-mode-map
              ("C-;" . flyspell-correct-wrapper)))

;; Calender
(use-package diary-lib
  :defer t
  :init
  (setq diary-display-function #'diary-fancy-display
        diary-number-of-entries 7)
  :autoload
  diary-sort-entries
  diary-include-other-diary-files
  diary-mark-included-diary-files
  :config
  (add-hook 'diary-list-entries-hook #'diary-sort-entries t)
  (add-hook 'diary-list-entries-hook #'diary-include-other-diary-files)
  (add-hook 'diary-mark-entries-hook #'diary-mark-included-diary-files))

;; Mail
(use-package wl
  :defer t
  :init
  (setq read-mail-command 'wl
        mail-user-agent 'wl-user-agent
        mail-envelope-from 'header
        mail-specify-envelope-from t
        mime-header-accept-quoted-encoded-words t
        ;; epg-pinentry-mode 'loopback
        plstore-cache-passphrase-for-symmetric-encryption t)
  (define-mail-user-agent
    'wl-user-agent
    'wl-user-agent-compose
    'wl-draft-send
    'wl-draft-kill
    'mail-send-hook))

(use-package mu-cite
  :defer t
  :init
  (setq mu-cite-prefix-format '(lsdb-prefix-register "> "))
  :config
  (lsdb-mu-insinuate)
  :hook
  (mail-citation . mu-cite-original))

(use-package x-face-e21
  :defer t
  :init
  (setq x-face-image-attributes
        '((light :ascent 80 :foreground "#655370")
          (dark :ascent 80 :foreground "#292b2e" :background "#b2b2b2"))))

;; Programming
(use-package lsp-mode
  :disabled
  :defer t
  :init
  (setq lsp-completion-provider :none) ;; we use Corfu!
  (defun lsp-mode-setup-completion ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(flex))) ;; Configure flex
  (add-hook 'lsp-completion-mode-hook #'lsp-mode-setup-completion))

(use-package lsp-sourcekit
  :disabled
  :after lsp-mode
  :config
  (setq lsp-sourcekit-executable "/usr/bin/sourcekit-lsp"))

(use-package eglot
  :defer t
  :config
  (add-to-list 'eglot-server-programs
               '(objc-mode . ("clangd"))))

(use-package dyalog-mode
  :defer t
  :init
  (setq dyalog-fix-whitespace-before-save t
        dyalog-leading-spaces 0)
  (defun dyalog-buffer-set-font ()
    (interactive)
    (buffer-face-set '(:family "BQN386 Unicode" :height 145))
    (buffer-face-mode 1))
  (add-hook 'dyalog-mode-hook #'dyalog-buffer-set-font)
  :interpreter
  ("dyalogscript\\(\\.bash\\)?" . dyalog-mode)
  :mode
  (("\\.apl[afno]" . dyalog-mode))
  :config
  (modify-syntax-entry ?# ". 1" dyalog-mode-syntax-table)
  (modify-syntax-entry ?! ". 2<" dyalog-mode-syntax-table))

;; nXML
(use-package nxml-mode
  :defer t
  :functions nxml-token-after
  :hook (nxml-mode . corfu-mode)
  :config
  (setq nxml-section-element-name-regexp
        "article\\|sect\\([1-5]\\|ion\\)\\|chapter\\|appendix\\|part\\|preface\\|reference\\|simplesect\\|bibliography\\|bibliodiv\\|glossary\\|glossdiv")
  (add-to-list 'rng-schema-locating-files
               (expand-file-name "~/.emacs.d/schema/schemas.xml")))

(use-package emmet-mode
  :defer t
  :hook nxml-mode html-mode)

;; FriCAS
(use-package frimacs
  :defer t
  :init
  (setq frimacs-process-program "fricas -noht -noclef"))

;; Hyperbole
(use-package hpath
  :defer t
  :config
  (setq
   hpath:external-display-alist-x
   (list (cons (format "\\.\\(%s\\)$"
					   hpath:external-file-suffixes)
			   "open"))))

;; TeX
(use-package tex-mode
  :disabled
  :defer t
  :config
  (setq tex-compile-commands
        `(,@(mapcar (lambda (prefix)
                      `((concat ,prefix tex-command
                                " " tex-start-options
                                " " (if (< 0 (length tex-start-commands))
                                        (shell-quote-argument tex-start-commands))
                                " %f")
                        t "%r.pdf"))
                    '("pdf" "ams" "lua"))
    ((concat tex-command
         " " (if (< 0 (length tex-start-commands))
             (shell-quote-argument tex-start-commands))
             " %f")
     t "%r.dvi")
    ("open %r.pdf" "%r.pdf")
    ("bibtex %r" "%r.aux" "%r.bbl")
    ("dvipdfmx %r" "%r.dvi" "%r.pdf"))))

;; Net
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

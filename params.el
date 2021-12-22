(setq ad-redefinition-action 'accept)
;; Visuals
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(prefer-coding-system 'utf-8)

(setq inhibit-startup-screen t)
(setq make-backup-files nil
      create-lockfiles nil)

(set-frame-font (font-spec :name "SF Mono" :size 13
			   :weight 'normal
                           :width 'normal) nil t)

(set-fontset-font "fontset-default"
                  'unicode
                  (font-spec :name "SImPL Medium" :size 13
			     :weight 'normal
                             :width 'normal) nil 'prepend)

(set-face-attribute 'variable-pitch
                    nil
                    :font "IBM Plex Sans Text")

(blink-cursor-mode -1)

(setq scroll-preserve-screen-position t
      scroll-margin 0
      scroll-conservatively 97
      make-cursor-line-fully-visible nil
      shr-width 72)

(setq-default indent-tabs-mode nil)
(setq ring-bell-function 'ignore)
(defalias 'yes-or-no-p 'y-or-n-p)

(setq recentf-exclude '("mime-example$" ".emacs.d/\\([a-z]\\|-\\)*$"))

;; ERC
(setq erc-server "irc.libera.chat"
      erc-nick "ldb")

;; elpa
(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
	("melpa" . "https://melpa.org/packages/")))

(if (not (boundp 'dumped-load-path))
    (load (concat user-emacs-directory "pkgs"))
  (setq load-path dumped-load-path)
  (global-font-lock-mode t)
  (transient-mark-mode t))

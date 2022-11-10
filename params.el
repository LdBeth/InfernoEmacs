(setq ad-redefinition-action 'accept)
;; Visuals
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(blink-cursor-mode -1)
(prefer-coding-system 'utf-8)

(setq inhibit-startup-screen t)
(setq make-backup-files nil
      create-lockfiles nil)

(set-frame-font (font-spec :name "SF Mono" :size 13
			   :weight 'normal
                           :width 'normal)
                nil t)

(set-fontset-font "fontset-default"
                  'unicode
                  (font-spec :name "SImPL Medium" :size 13
			     :weight 'normal
                             :width 'normal)
                  nil 'prepend)

(set-face-attribute 'variable-pitch
                    nil
                    :font "IBM Plex Sans Text")

(setq scroll-preserve-screen-position t
      scroll-margin 0
      scroll-conservatively 97
      make-cursor-line-fully-visible nil
      shr-width 72
      gamegrid-glyph-height-mm 3.0)

(setq mac-emulate-three-button-mouse t
      track-eol t
      kill-whole-line t)

(setq-default indent-tabs-mode nil)
(setq ring-bell-function 'ignore
      use-short-answers t
      tab-always-indent 'complete)

(setq recentf-exclude '("mime-example$" ".emacs.d/\\([a-z]\\|-\\)*$"))

;; ERC
(setq erc-server "irc.libera.chat"
      erc-nick "ldb"
      erc-insert-timestamp-function 'erc-insert-timestamp-left
      erc-timestamp-format "%H%M ")

;; Newsticker
(setq newsticker-url-list
      '(("xkcd" "https://xkcd.com/rss.xml")
        ("williamlong" "http://feed.williamlong.info/")
        ("ACG" "http://www.acgpiping.net/feed/")))

(setq corfu-auto t)

;; elpa
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))

(if (not (boundp 'dumped-load-path))
    (load (concat user-emacs-directory "pkgs"))
  (setq load-path dumped-load-path)
  (global-font-lock-mode t)
  (transient-mark-mode t))

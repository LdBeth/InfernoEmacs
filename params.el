(setq ad-redefinition-action 'accept)
;; Visuals
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(prefer-coding-system 'utf-8)

(setq inhibit-startup-screen t)
(setq make-backup-files nil)

(set-frame-font (font-spec :name "SF Mono" :size 13
			   :weight 'normal
               :width 'normal) nil t)

(blink-cursor-mode -1)

(setq scroll-preserve-screen-position t
      scroll-margin 0
      scroll-conservatively 97)

;; Splash
(defvar dotspacemacs-banner
  (concat user-emacs-directory "banner.pbm"))
(defvar dotspacemacs-startup-lists
  '((recents  . 5)))

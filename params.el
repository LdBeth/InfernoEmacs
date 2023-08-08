;; (let ((paths
;;        (eval-when-compile
;;          (with-temp-buffer
;;            (call-process-shell-command "cat /etc/paths /etc/paths.d/*" nil t)
;;            (split-string (buffer-string))))))
;;   (setenv "PATH" (mapconcat #'identity paths ":"))
;;   (setq exec-path (add-to-list 'paths exec-directory t)))

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
                    :family "IBM Plex Sans")

(setq scroll-preserve-screen-position t
      scroll-margin 0
      scroll-conservatively 97
      make-cursor-line-fully-visible nil
      shr-width 72
      gamegrid-glyph-height-mm 3.0)

(setq ;; auth-sources '(macos-keychain-internet macos-keychain-generic)
      track-eol t
      kill-whole-line t)

(setq-default indent-tabs-mode nil
              tab-width 4)
(setq ring-bell-function 'ignore
      use-short-answers t
      tab-always-indent 'complete
      enable-recursive-minibuffers t)

(setq recentf-exclude '("mime-example$" ".emacs.d/\\([a-z]\\|-\\)*$")
      dired-use-ls-dired nil)

;; ERC
(setq erc-server "irc.libera.chat"
      erc-nick "ldb"
      erc-insert-timestamp-function 'erc-insert-timestamp-left
      erc-timestamp-format "%H%M "
      erc-autojoin-timing 'ident
      ;; erc-autojoin-channels-alist '(("libera.chat" "#commonlisp" "#emacs"))
      )

;; Newsticker
(setq newsticker-url-list
      '(("xkcd" "https://xkcd.com/rss.xml")
        ("williamlong" "http://feed.williamlong.info/rss.xml")
        ("ldbeth" "https://ldbeth.sdf.org/rss.xml")
        ;;("ACG" "http://www.acgpiping.net/feed/")
        ("XML" "https://www.xml.com/feed/all/")
        ("ndw" "https://so.nwalsh.com/feed/whatsnew.xml")
        ))

(setq corfu-auto t
      orderless-matching-styles '(orderless-prefixes
                                  orderless-regexp)
      completion-styles '(flex orderless basic)
      completion-category-defaults nil
      completion-category-overrides
      '((file (styles basic partial-completion))))

;; lisp
(setq inferior-lisp-program "ccl"
      common-lisp-hyperspec-root "file:///usr/local/share/doc/HyperSpec/")
;; elpa
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))

(setq tramp-auto-save-directory (concat temporary-file-directory
                                        "tramp/"))

(defvar dumped-load-path)
(if (not (boundp 'dumped-load-path))
    (load (concat user-emacs-directory "pkgs"))
  (setq load-path dumped-load-path)
  (global-font-lock-mode t)
  (transient-mark-mode t)
  (set-face-foreground 'homoglyph "#686868"))

(vertico-mode 1)

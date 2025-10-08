;; -*- lexical-binding:nil -*-
(let ((paths
       (eval-when-compile
         (with-temp-buffer
           (call-process-shell-command "cat /etc/paths /etc/paths.d/*" nil t)
           (split-string (buffer-string))))))
  (setenv "PATH" (mapconcat #'identity paths ":"))
  (setq exec-path (append paths (list exec-directory))))

(setq ad-redefinition-action 'accept)
;; Visuals
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(blink-cursor-mode -1)
;; (mac-auto-ascii-mode 1)
;; (prefer-coding-system 'utf-8)

(when (boundp 'mac-command-modifier)
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'super))

(setq inhibit-startup-screen t)
(setq make-backup-files nil
      create-lockfiles nil)

(setq-default bidi-display-reordering nil)
(setq bidi-inhibit-bpa t
      long-line-threshold 1000
      large-hscroll-threshold 1000
      syntax-wholeline-max 1000)

(set-frame-font (font-spec :name "SF Mono" :size 13
			               :weight 'normal
                           :width 'normal)
                nil t)

(set-fontset-font t
                  'symbol
                  (font-spec :name "SImPL" :size 13
                             :width 'normal))

(set-face-attribute 'variable-pitch
                    nil
                    :family "IBM Plex Sans")

(setq scroll-preserve-screen-position t
      scroll-margin 0
      scroll-conservatively 97
      make-cursor-line-fully-visible nil
      shr-width 72
      gamegrid-glyph-height-mm 3.0)

(setq auth-sources '(macos-keychain-internet macos-keychain-generic)
      track-eol t
      kill-whole-line t)

(setq-default indent-tabs-mode nil
              tab-width 4)
(setq ring-bell-function 'ignore
      use-short-answers t
      tab-always-indent 'complete
      enable-recursive-minibuffers t
      help-enable-completion-autoload nil)

(setq recentf-exclude '("mime-example$" "HYPB$"
                        "\\.emacs\\.d/\\([a-z]\\|-\\)*$" "[0a-f]+\\.plstore$")
      dired-use-ls-dired nil)

;; ERC
(setq erc-server "irc.libera.chat"
      erc-nick "ldb"
      erc-autojoin-channels-alist
      '(("libera.chat" "#emacs" "#jsoftware" "#commonlisp")))

;; langtool
(setq langtool-http-server-host "localhost"
      langtool-http-server-port 8080
      gptel-directives #'nekomimi-agent-generate-directives)

;; Newsticker
(setq newsticker-url-list
      '(("xkcd" "https://xkcd.com/rss.xml")
        ("williamlong" "http://feed.williamlong.info/rss.xml")
        ("ldbeth" "https://ldbeth.sdf.org/rss.xml")
        ("RnE" "https://kekkan.org/atom.xml")
        ;;("ACG" "http://www.acgpiping.net/feed/")
        ("XML" "https://www.xml.com/feed/all/")
        ("ndw" "https://so.nwalsh.com/feed/fulltext.xml")
        ))

(setq corfu-auto t
      orderless-matching-styles '(orderless-prefixes
                                  orderless-regexp)
      completion-styles '(flex orderless basic)
      completion-category-defaults nil
      completion-category-overrides
      '((file (styles basic partial-completion))))

;; lisp
(setq inferior-lisp-program "sbcl"
      common-lisp-hyperspec-root "file:///usr/local/share/doc/HyperSpec/")

;;
(setq smart-c-cpp-include-path
      '("/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/"))
;; elpa
(add-to-list 'package-archives
             '("melpa" . "https://mirrors.ustc.edu.cn/elpa/melpa/"))

(setq tramp-auto-save-directory (concat temporary-file-directory
                                        "tramp/")
      tramp-use-ssh-controlmaster-options nil)

(setq j-help-use-jwiki t)

(defvar dumped-load-path)
(if (not (boundp 'dumped-load-path))
    (load (concat user-emacs-directory "pkgs"))
  (setq load-path dumped-load-path)
  (global-font-lock-mode t)
  (transient-mark-mode t)
  (set-face-foreground 'homoglyph "#686868"))

(vertico-mode 1)

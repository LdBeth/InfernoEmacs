;;; spacemacs-common.el --- Color theme with a dark and light versions.

;; Copyright (C) 2015-2018 Nasser Alshammari

;; Author: Nasser Alshammari
;; URL: <https://github.com/nashamri/spacemacs-theme>
;;
;; Version: 0.1
;; Keywords: color, theme
;; Package-Requires: ((emacs "24"))

;; Initially created with the help of emacs-theme-generator, <https://github.com/mswift42/theme-creator>.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;; This file is not part of Emacs.

;;; Commentary:

;; This is a color theme for spacemacs <https://github.com/syl20bnr/spacemacs>.
;; It comes with two versions, dark and light and should work well in
;; a 256 color terminal.

;;; Code:

(defgroup spacemacs-theme nil
  "Spacemacs-theme options."
  :group 'faces)

(defcustom spacemacs-theme-comment-bg t
  "Use a background for comment lines."
  :type 'boolean
  :group 'spacemacs-theme)

(defcustom spacemacs-theme-comment-italic nil
  "Enable italics for comments and also disable background."
  :type 'boolean
  :group 'spacemacs-theme)

(defcustom spacemacs-theme-keyword-italic nil
  "Enable italics for keywords."
  :type 'boolean
  :group 'spacemacs-theme)

(defcustom spacemacs-theme-org-agenda-height nil
  "If non-nil, use varying text heights for agenda items.

Note that if you change this to a non-nil value, you may want to
also adjust the value of `org-agenda-tags-column'. If that is set
to 'auto, tags may not be properly aligned. "
  :type 'boolean
  :group 'spacemacs-theme)

(defcustom spacemacs-theme-org-height t
  "Use varying text heights for org headings."
  :type 'boolean
  :group 'spacemacs-theme)

(defcustom spacemacs-theme-org-bold t
  "Inherit text bold for org headings"
  :type 'boolean
  :group 'spacemacs-theme)

(defcustom spacemacs-theme-org-priority-bold t
  "Inherit text bold for priority items in agenda view"
  :type 'boolean
  :group 'spacemacs-theme)

(defcustom spacemacs-theme-org-highlight nil
  "Highlight org headings."
  :type 'boolean
  :group 'spacemacs-theme)

(defcustom spacemacs-theme-custom-colors nil
  "Specify a list of custom colors."
  :type 'alist
  :group 'spacemacs-theme)

(defcustom spacemacs-theme-underline-parens t
  "If non-nil, underline matching parens when using `show-paren-mode' or similar."
  :type 'boolean
  :group 'spacemacs-theme)

(defun true-color-p ()
  (or
   (display-graphic-p)
   (= (tty-display-color-cells) 16777216)))

(defmacro color-variant (dark-gui dark-term light-gui light-term)
  `(if (eq variant 'dark)
       (if (true-color-p) ,dark-gui ,dark-term)
     (if (true-color-p) ,light-gui ,light-term)))

(defmacro create-spacemacs-theme (variant theme-name)
  (let ((class '((class color) (min-colors 89))) ;;
        ;;                                 ~~ Dark ~~         ~~ Light ~~
        ;; generic                       GUI       TER       GUI       TER
        (act1          (color-variant "#222226" "#121212" "#e7e5eb" "#d7dfff"))
        (act2          (color-variant "#5d4d7a" "#444444" "#d3d3e7" "#afafd7"))
        (base          (color-variant "#b2b2b2" "#b2b2b2" "#655370" "#5f5f87"))
        (base-dim      (color-variant "#5e5e7f" "#585858" "#a094a2" "#afafd7"))
        (bg1           (color-variant "#292b2e" "#262626" "#fbf8ef" "#ffffff"))
        (bg2           (color-variant "#212026" "#1c1c1c" "#efeae9" "#e4e4e4"))
        (bg3           (color-variant "#100a14" "#121212" "#e3dedd" "#d0d0d0"))
        (bg4           (color-variant "#0a0814" "#080808" "#d2ceda" "#bcbcbc"))
        (bg-alt        (color-variant "#44234a" "#663366" "#efeae9" "#e4e4e4"))
        (border        (color-variant "#5d4d7a" "#111111" "#b3b9be" "#b3b9be"))
        (cblk          (color-variant "#cbc1d5" "#b2b2b2" "#655370" "#5f5f87"))
        (cblk-bg       (color-variant "#2f2b33" "#262626" "#e8e3f0" "#ffffff"))
        (cblk-ln       (color-variant "#827591" "#af5faf" "#9380b2" "#af5fdf"))
        (cblk-ln-bg    (color-variant "#373040" "#333333" "#ddd8eb" "#dfdfff"))
        (cursor        (color-variant "#e3dedd" "#d0d0d0" "#100a14" "#121212"))
        (const         (color-variant "#ce3fa5" "#d75fd7" "#4e3163" "#8700af"))
        (comment       (color-variant "#2aa1ae" "#008787" "#2aa1ae" "#008787"))
        (comment-light (color-variant "#2aa1ae" "#008787" "#a49da5" "#008787"))
        (comment-bg    (color-variant "#292e34" "#262626" "#ecf3ec" "#ffffff"))
        (comp          (color-variant "#a24aad" "#d75fd7" "#af0058" "#8700af"))
        (err           (color-variant "#e0211d" "#e0211d" "#e0211d" "#e0211d"))
        (func          (color-variant "#bc5ea7" "#d75fd7" "#6c3163" "#8700af"))
        (head1         (color-variant "#4f97d7" "#268bd2" "#3a81c3" "#268bd2"))
        (head1-bg      (color-variant "#293239" "#262626" "#edf1ed" "#ffffff"))
        (head2         (color-variant "#2d9574" "#2aa198" "#2d9574" "#2aa198"))
        (head2-bg      (color-variant "#293235" "#262626" "#edf2e9" "#ffffff"))
        (head3         (color-variant "#67b11d" "#67b11d" "#67b11d" "#5faf00"))
        (head3-bg      (color-variant "#293235" "#262626" "#edf2e9" "#ffffff"))
        (head4         (color-variant "#b1951d" "#875f00" "#b1951d" "#875f00"))
        (head4-bg      (color-variant "#32322c" "#262626" "#f6f1e1" "#ffffff"))
        (highlight     (color-variant "#444155" "#444444" "#d3d3e7" "#d7d7ff"))
        (highlight-dim (color-variant "#3b314d" "#444444" "#e7e7fc" "#d7d7ff"))
        (keyword       (color-variant "#2e87d0" "#268bd2" "#3a81c3" "#268bd2"))
        (lnum          (color-variant "#44505c" "#444444" "#a8a8bf" "#af87af"))
        (mat           (color-variant "#86dc2f" "#86dc2f" "#ba2f59" "#af005f"))
        (meta          (color-variant "#cd8fa0" "#cc9999" "#da8b55" "#df5f5f"))
        (str           (color-variant "#2d9574" "#2aa198" "#2d9574" "#2aa198"))
        (suc           (color-variant "#86dc2f" "#86dc2f" "#42ae2c" "#00af00"))
        (ttip          (color-variant "#9a9aba" "#888888" "#8c799f" "#5f5f87"))
        (ttip-sl       (color-variant "#5e5079" "#333333" "#c8c6dd" "#afafff"))
        (ttip-bg       (color-variant "#34323e" "#444444" "#e2e0ea" "#dfdfff"))
        (type          (color-variant "#ce537a" "#df005f" "#ba2f59" "#af005f"))
        (var           (color-variant "#7590db" "#8787d7" "#715ab1" "#af5fd7"))
        (war           (color-variant "#dc752f" "#dc752f" "#dc752f" "#dc752f"))

        ;; colors
        (aqua          (color-variant "#2d9574" "#2aa198" "#2d9574" "#2aa198"))
        (aqua-bg       (color-variant "#293235" "#262626" "#edf2e9" "#ffffff"))
        (green         (color-variant "#67cd0d" "#67b11d" "#67cd0d" "#5faf00"))
        (green-bg      (color-variant "#293235" "#262626" "#edf2e9" "#ffffff"))
        (green-bg-s    (color-variant "#29422d" "#262626" "#dae6d0" "#ffffff"))
        (cyan          (color-variant "#28def0" "#00ffff" "#21b8c7" "#008080"))
        (red           (color-variant "#ff3233" "#d70000" "#ff3233" "#d70008"))
        (red-bg        (color-variant "#3c2a2c" "#262626" "#faede4" "#ffffff"))
        (red-bg-s      (color-variant "#512e31" "#262626" "#eed9d2" "#ffffff"))
        (blue          (color-variant "#4f97d7" "#268bd2" "#3a81c3" "#268bd2"))
        (blue-bg       (color-variant "#293239" "#262626" "#edf1ed" "#d7d7ff"))
        (blue-bg-s     (color-variant "#2d4252" "#262626" "#d1dcdf" "#d7d7ff"))
        (magenta       (color-variant "#cc1db1" "#af00df" "#cc1db1" "#800080"))
        (yellow        (color-variant "#f3990d" "#875f00" "#f3990d" "#875f00"))
        (yellow-bg     (color-variant "#32322c" "#262626" "#f6f1e1" "#ffffff")))

    (cl-loop for (cvar . val) in spacemacs-theme-custom-colors
             do (set cvar val))

  `(progn
    (custom-theme-set-faces
     ,theme-name

;;;;; basics
     '(cursor ((,class (:background "SkyBlue2"))))
     '(custom-button ((,class :background ,bg2 :foreground ,base :box (:line-width 2 :style released-button))))
     '(default ((,class (:background ,bg1 :foreground ,base))))
     '(default-italic ((,class (:italic t))))
     '(error ((,class (:foreground ,err))))
     '(escape-glyph ((,class (:foreground ,comp))))
     '(eval-sexp-fu-flash ((,class (:background ,suc :foreground ,bg1))))
     '(eval-sexp-fu-flash-error ((,class (:background ,err :foreground ,bg1))))
     '(font-lock-builtin-face ((,class (:foreground ,keyword))))
     '(font-lock-comment-face ((,class (:foreground ,(if spacemacs-theme-comment-italic comment-light comment) :background ,(when spacemacs-theme-comment-bg comment-bg) :slant ,(if spacemacs-theme-comment-italic 'italic 'normal)))))
     '(font-lock-constant-face ((,class (:foreground ,const))))
     '(font-lock-doc-face ((,class (:foreground ,meta))))
     '(font-lock-function-name-face ((,class (:foreground ,func :inherit bold))))
     '(font-lock-keyword-face ((,class (:inherit bold :foreground ,keyword :slant ,(if spacemacs-theme-keyword-italic 'italic 'normal)))))
     '(font-lock-negation-char-face ((,class (:foreground ,const))))
     '(font-lock-preprocessor-face ((,class (:foreground ,func))))
     '(font-lock-string-face ((,class (:foreground ,str))))
     '(font-lock-type-face ((,class (:foreground ,type :inherit bold))))
     '(font-lock-variable-name-face ((,class (:foreground ,var))))
     '(font-lock-warning-face ((,class (:foreground ,war :background ,bg1))))
     '(fringe ((,class (:background ,bg1 :foreground ,base))))
     '(header-line ((,class :background ,bg2)))
     '(help-key-binding ((,class :background ,bg4 :foreground ,func
                                 :box (:line-width (1 . -1) :color ,border))))
     '(highlight ((,class (:foreground ,base :background ,highlight))))
     '(hl-line ((,class (:background ,bg2 :extend t))))
     '(homoglyph ((,class (:foreground ,var :extend t))))
     '(isearch ((,class (:foreground ,bg1 :background ,mat))))
     '(lazy-highlight ((,class (:background ,green-bg-s :weight normal))))
     '(link ((,class (:foreground ,comment :underline t))))
     '(link-visited ((,class (:foreground ,comp :underline t))))
     '(match ((,class (:background ,highlight :foreground ,mat))))
     '(minibuffer-prompt ((,class (:inherit bold :foreground ,keyword))))
     '(page-break-lines ((,class (:foreground ,act2))))
     '(region ((,class (:background ,highlight :extend t))))
     '(secondary-selection ((,class (:background ,bg3))))
     '(shadow ((,class (:foreground ,base-dim))))
     '(success ((,class (:foreground ,suc))))
     '(tooltip ((,class (:background ,ttip-sl :foreground ,base :bold nil :italic nil :underline nil))))
     '(vertical-border ((,class (:foreground ,border))))
     '(warning ((,class (:foreground ,war))))
     '(widget-button-pressed ((,class (:foreground ,green))))

;;;;; ace-window
     '(aw-leading-char-face ((,class (:foreground ,func :weight bold :height 2.0 :box (:line-width 1 :color ,keyword :style released-button)))))

;;;;; ahs
     '(ahs-face ((,class (:background ,highlight))))
     '(ahs-face-unfocused ((,class (:background ,highlight))))
     '(ahs-definition-face ((,class (:background ,highlight))))
     '(ahs-definition-face-unfocused ((,class (:background ,highlight))))
     '(ahs-plugin-whole-buffer-face ((,class (:background ,mat :foreground ,bg1))))
     '(ahs-plugin-default-face ((,class (:background ,highlight))))
     '(ahs-plugin-default-face-unfocused ((,class (:background ,highlight))))

;;;;; anzu-mode
     '(anzu-mode-line ((,class (:foreground ,yellow :inherit bold))))

;;;;; avy
     '(avy-lead-face   ((,class (:background ,green-bg :foreground ,green))))
     '(avy-lead-face-0 ((,class (:background ,green-bg :foreground ,yellow))))
     '(avy-lead-face-1 ((,class (:background ,green-bg :foreground ,magenta))))
     '(avy-lead-face-2 ((,class (:background ,green-bg :foreground ,blue))))

;;;;; cider
     '(cider-enlightened ((,class (:background nil :box (:color ,yellow :line-width -1 :style nil) :foreground ,yellow))))
     '(cider-enlightened-local ((,class (:foreground ,yellow))))
     '(cider-instrumented-face ((,class (:background nil :box (:color ,red :line-width -1 :style nil) :foreground ,red))))
     '(cider-result-overlay-face ((,class (:background nil :box (:color ,blue :line-width -1 :style nil) :foreground ,blue))))
     '(cider-test-error-face ((,class (:background ,war :foreground ,bg1))))
     '(cider-test-failure-face ((,class (:background ,err :foreground ,bg1))))
     '(cider-test-success-face ((,class (:background ,suc :foreground ,bg1))))
     '(cider-traced-face ((,class :box (:color ,cyan :line-width -1 :style nil))))

;;;;; company
     '(company-echo-common ((,class (:background ,base :foreground ,bg1))))
     '(company-preview ((,class (:background ,ttip-bg :foreground ,ttip))))
     '(company-preview-common ((,class (:background ,ttip-bg :foreground ,base))))
     '(company-preview-search ((,class (:inherit match))))
     '(company-scrollbar-bg ((,class (:background ,bg2))))
     '(company-scrollbar-fg ((,class (:background ,act2))))
     '(company-template-field ((,class (:inherit region))))
     '(company-tooltip ((,class (:background ,ttip-bg :foreground ,ttip))))
     '(company-tooltip-annotation ((,class (:foreground ,type))))
     '(company-tooltip-common ((,class (:background ,ttip-bg :foreground ,keyword))))
     '(company-tooltip-common-selection ((,class (:foreground ,keyword))))
     '(company-tooltip-mouse ((,class (:inherit highlight))))
     '(company-tooltip-search ((,class (:inherit match))))
     '(company-tooltip-selection ((,class (:background ,ttip-sl :foreground ,base))))

;;;;; diff
     '(diff-added             ((,class :background nil :foreground ,green :extend t)))
     '(diff-error             ((,class :background nil :foreground ,err)))
     '(diff-changed           ((,class :background nil :foreground ,blue)))
     '(diff-header            ((,class :background ,cblk-ln-bg :foreground ,func :extend t)))
     '(diff-file-header       ((,class :background ,cblk-ln-bg :foreground ,cblk :extend t)))
     '(diff-indicator-added   ((,class :background nil :foreground ,green :extend t)))
     '(diff-indicator-changed ((,class :background nil :foreground ,blue)))
     '(diff-indicator-removed ((,class :background nil :foreground ,red)))
     '(diff-refine-added      ((,class :background ,green :foreground ,bg1)))
     '(diff-refine-changed    ((,class :background ,blue :foreground ,bg1)))
     '(diff-refine-removed    ((,class :background ,red :foreground ,bg1)))
     '(diff-removed           ((,class :background nil :foreground ,red :extend t)))

;;;;; diff-hl
     '(diff-hl-insert ((,class :background ,green :foreground ,green)))
     '(diff-hl-delete ((,class :background ,red :foreground ,red)))
     '(diff-hl-change ((,class :background ,blue :foreground ,blue)))

;;;;; dired
     '(dired-directory ((,class (:foreground ,keyword :background ,bg1 :inherit bold))))
     '(dired-flagged ((,class (:foreground ,red))))
     '(dired-header ((,class (:foreground ,comp :inherit bold))))
     '(dired-ignored ((,class (:inherit shadow))))
     '(dired-mark ((,class (:foreground ,comp :inherit bold))))
     '(dired-marked ((,class (:foreground ,magenta :inherit bold))))
     '(dired-perm-write ((,class (:foreground ,base :underline t))))
     '(dired-symlink ((,class (:foreground ,cyan :background ,bg1 :inherit bold))))
     '(dired-warning ((,class (:foreground ,war))))

;;;;; doom-modeline
     '(doom-modeline-bar ((,class (:background ,keyword))))
;;;;; nano-modeline
     '(nano-modeline ((,class (:foreground ,base))))
     '(nano-modeline-primary ((,class (:inherit nano-modeline :background ,act2))))
     '(nano-modeline-secondary ((,class (:foreground ,base-dim))))
     '(nano-modeline-name ((,class (:inherit mode-line-buffer-id))))
     '(nano-modeline-status-** ((,class (:background ,red-bg
                                                     :foreground ,red))))
     '(nano-modeline-status-RW ((,class (:inherit nano-modeline-primary
                                                  :foreground ,bg1))))
     '(nano-modeline-status-RO ((,class (:background "SkyBlue2" :foreground ,blue))))
;;;;; ediff
     '(ediff-current-diff-A ((,class(:background ,red-bg :foreground ,red :extend t))))
     '(ediff-current-diff-Ancestor ((,class(:background ,aqua-bg :foreground ,aqua :extend t))))
     '(ediff-current-diff-B ((,class(:background ,green-bg :foreground ,green :extend t))))
     '(ediff-current-diff-C ((,class(:background ,blue-bg :foreground ,blue :extend t))))
     '(ediff-even-diff-A ((,class(:background ,bg3 :extend t))))
     '(ediff-even-diff-Ancestor ((,class(:background ,bg3 :extend t))))
     '(ediff-even-diff-B ((,class(:background ,bg3 :extend t))))
     '(ediff-even-diff-C ((,class(:background ,bg3 :extend t))))
     '(ediff-fine-diff-A ((,class(:background ,red :foreground ,bg1 :extend t))))
     '(ediff-fine-diff-Ancestor ((,class(:background nil :inherit bold :extend t))))
     '(ediff-fine-diff-B ((,class(:background ,green :foreground ,bg1))))
     '(ediff-fine-diff-C ((,class(:background ,blue :foreground ,bg1))))
     '(ediff-odd-diff-A ((,class(:background ,bg4 :extend t))))
     '(ediff-odd-diff-Ancestor ((,class(:background ,bg4 :extend t))))
     '(ediff-odd-diff-B ((,class(:background ,bg4 :extend t))))
     '(ediff-odd-diff-C ((,class(:background ,bg4 :extend t))))

;;;;; ein
     '(ein:cell-input-area((,class (:background ,bg2))))
     '(ein:cell-input-prompt ((,class (:foreground ,suc))))
     '(ein:cell-output-prompt ((,class (:foreground ,err))))
     '(ein:notification-tab-normal ((,class (:foreground ,keyword))))
     '(ein:notification-tab-selected ((,class (:foreground ,suc :inherit bold))))

;;;;; eldoc
     '(eldoc-highlight-function-argument ((,class (:foreground ,mat :inherit bold))))

;;;;; elfeed
     '(elfeed-search-date-face ((,class (:foreground ,head2))))
     '(elfeed-search-feed-face ((,class (:foreground ,blue))))
     '(elfeed-search-tag-face ((,class (:foreground ,func))))
     '(elfeed-search-title-face ((,class (:foreground ,var))))
     '(elfeed-search-unread-title-face ((,class (:foreground ,base))))

;;;;; enh-ruby
     '(enh-ruby-op-face ((,class (:background ,bg1 :foreground ,base))))
     '(enh-ruby-string-delimiter-face ((,class (:foreground ,str))))

;;;;; erc
     '(erc-input-face ((,class (:foreground ,func))))
     '(erc-my-nick-face ((,class (:foreground ,keyword))))
     '(erc-nick-default-face ((,class (:foreground ,keyword))))
     '(erc-nick-prefix-face ((,class (:foreground ,yellow))))
     '(erc-notice-face ((,class (:foreground ,str))))
     '(erc-prompt-face ((,class (:foreground ,mat :inherit bold))))
     '(erc-timestamp-face ((,class (:foreground ,keyword))))

;;;;; eshell
     '(eshell-ls-archive ((,class (:foreground ,red :inherit bold))))
     '(eshell-ls-backup ((,class (:inherit font-lock-comment-face))))
     '(eshell-ls-clutter ((,class (:inherit font-lock-comment-face))))
     '(eshell-ls-directory ((,class (:foreground ,keyword :inherit bold))))
     '(eshell-ls-executable ((,class (:foreground ,suc :inherit bold))))
     '(eshell-ls-missing ((,class (:inherit font-lock-warning-face))))
     '(eshell-ls-product ((,class (:inherit font-lock-doc-face))))
     '(eshell-ls-special ((,class (:foreground ,yellow :inherit bold))))
     '(eshell-ls-symlink ((,class (:foreground ,cyan :inherit bold))))
     '(eshell-ls-unreadable ((,class (:foreground ,base))))
     '(eshell-prompt ((,class (:foreground ,keyword :inherit bold))))

;;;;; ESS
     '(ess-assignment-face ((,class (:foreground ,type :inherit bold))))
     '(ess-backquoted-face ((,class (:foreground ,var))))
     '(ess-constant-face ((,class (:inherit font-lock-constant-face))))
     '(ess-f-t-face ((,class (:inherit font-lock-constant-face))))
     '(ess-function-call-face ((,class (:foreground ,func))))
     '(ess-keyword-face ((,class (:inherit font-lock-keyword-face))))
     '(ess-matrix-face ((,class (:foreground ,base-dim))))
     '(ess-modifiers-face ((,class (:foreground ,keyword))))
     '(ess-numbers-face ((,class (:inherit font-lock-constant-face))))
     '(ess-operator-face ((,class (:foreground ,var))))
     '(ess-paren-face ((,class (:foreground ,blue))))
     '(ess-r-control-flow-keyword-face ((,class (:foreground ,keyword))))
     '(ess-r-signal-keyword-face ((,class (:foreground ,war))))

;;;;; evil
     '(evil-ex-lazy-highlight ((,class (:background ,mat :foreground ,bg1))))
     '(evil-ex-substitute-matches ((,class (:background ,red-bg :foreground ,red))))
     '(evil-ex-substitute-replacement ((,class (:background ,green-bg :foreground ,green))))

;;;;; evil-goggles
      '(evil-goggles--pulse-face ((,class (:background ,yellow-bg :foreground ,yellow))))
      '(evil-goggles-change-face ((,class (:background ,blue-bg-s :foreground ,blue))))
      '(evil-goggles-commentary-face ((,class (:background ,aqua-bg :foreground ,aqua))))
      '(evil-goggles-delete-face ((,class (:background ,red-bg-s :foreground ,red))))
      '(evil-goggles-fill-and-move-face ((,class (:background ,green-bg-s :foreground ,green))))
      '(evil-goggles-indent-face ((,class (:background ,green-bg-s :foreground ,green))))
      '(evil-goggles-join-face ((,class (:background ,green-bg-s :foreground ,green))))
      '(evil-goggles-nerd-commenter-face ((,class (:background ,aqua-bg :foreground ,aqua))))
      '(evil-goggles-paste-face ((,class (:background ,green-bg-s :foreground ,green))))
      '(evil-goggles-record-macro-face ((,class (:background ,blue-bg-s :foreground ,blue))))
      '(evil-goggles-replace-with-register-face ((,class (:background ,yellow-bg :foreground ,yellow))))
      '(evil-goggles-set-marker-face ((,class (:background ,blue-bg-s :foreground ,blue))))
      '(evil-goggles-shift-face ((,class (:background ,blue-bg-s :foreground ,blue))))
      '(evil-goggles-surround-face ((,class (:background ,blue-bg-s :foreground ,blue))))
      '(evil-goggles-yank-face ((,class (:background ,blue-bg-s :foreground ,blue))))
      '(evil-goggles-undo-redo-add-face ((,class (:background ,green-bg-s :foreground ,green))))
      '(evil-goggles-undo-redo-change-face ((,class (:background ,blue-bg-s :foreground ,blue))))
      '(evil-goggles-undo-redo-remove-face ((,class (:background ,red-bg-s :foreground ,red))))

;;;;; flycheck
     '(flycheck-error
       ((,(append '((supports :underline (:style line))) class)
         (:underline (:style line :color ,err)))
        (,class (:foreground ,base :background ,err :inherit bold :underline t))))
     '(flycheck-error-list-checker-name ((,class (:foreground ,keyword))))
     '(flycheck-fringe-error ((,class (:foreground ,err :inherit bold))))
     '(flycheck-fringe-info ((,class (:foreground ,keyword :inherit bold))))
     '(flycheck-fringe-warning ((,class (:foreground ,war :inherit bold))))
     '(flycheck-info
       ((,(append '((supports :underline (:style line))) class)
         (:underline (:style line :color ,keyword)))
        (,class (:foreground ,base :background ,keyword :inherit bold :underline t))))
     '(flycheck-warning
       ((,(append '((supports :underline (:style line))) class)
         (:underline (:style line :color ,war)))
        (,class (:foreground ,base :background ,war :inherit bold :underline t))))

;;;;; flymake
     '(flymake-error ((,(append '((supports :underline (:style line))) class)
                       (:underline (:style line :color ,err)))
                      (,class (:foreground ,base :background ,err :inherit bold :underline t))))
     '(flymake-note ((,(append '((supports :underline (:style line))) class)
                      (:underline (:style wave :color ,keyword)))
                     (,class (:foreground ,base :background ,keyword :inherit bold :underline t))))
     '(flymake-warning ((,(append '((supports :underline (:style line))) class)
                         (:underline (:style line :color ,war)))
                        (,class (:foreground ,base :background ,war :inherit bold :underline t))))

;;;;; flyspell
     '(flyspell-incorrect ((,(append '((supports :underline (:style line))) class)
                            (:underline (:style wave :color ,war)))
                           (,class (:foreground ,base :background ,war :inherit bold :underline t))))
     '(flyspell-duplicate ((,(append '((supports :underline (:style line))) class)
                            (:underline (:style wave :color ,keyword)))
                           (,class (:foreground ,base :background ,keyword :inherit bold :underline t))))

;;;;; git-gutter-fr
     '(git-gutter-fr:added ((,class (:foreground ,green :inherit bold))))
     '(git-gutter-fr:deleted ((,class (:foreground ,red :inherit bold))))
     '(git-gutter-fr:modified ((,class (:foreground ,blue :inherit bold))))

;;;;; git-timemachine
     '(git-timemachine-minibuffer-detail-face ((,class (:foreground ,blue :inherit bold :background ,blue-bg))))

;;;;; gnus
     '(gnus-emphasis-highlight-words ((,class (:background ,suc :foreground ,bg1))))
     '(gnus-header-content ((,class (:foreground ,keyword))))
     '(gnus-header-from ((,class (:foreground ,var))))
     '(gnus-header-name ((,class (:foreground ,comp))))
     '(gnus-header-subject ((,class (:foreground ,func :inherit bold))))
     '(gnus-summary-cancelled ((,class (:background ,war :foreground ,bg1))))

;;;;; guide-key
     '(guide-key/highlight-command-face ((,class (:foreground ,base))))
     '(guide-key/key-face ((,class (:foreground ,keyword))))
     '(guide-key/prefix-command-face ((,class (:foreground ,keyword :inherit bold))))

;;;;; highlights
     '(hi-green  ((,class (:foreground ,green :background ,green-bg))))
     '(hi-yellow ((,class (:foreground ,yellow :background ,yellow-bg))))

;;;;; highlight-indentation
     '(highlight-indentation-face ((,class (:background ,comment-bg))))

;;;;; highlight-symbol
     '(highlight-symbol-face ((,class (:background ,bg-alt))))

;;;;; highlight-thing
     '(highlight-thing       ((,class (:background ,bg-alt))))

;;;;; hydra
     '(hydra-face-blue ((,class (:foreground ,blue))))
     '(hydra-face-red ((,class (:foreground ,red))))

;;;;; ido
     '(ido-first-match ((,class (:foreground ,comp :inherit bold))))
     '(ido-only-match ((,class (:foreground ,mat :inherit bold))))
     '(ido-subdir ((,class (:foreground ,keyword))))
     '(ido-vertical-match-face ((,class (:foreground ,comp :underline nil))))

;;;;; info
     '(info-header-xref ((,class (:foreground ,func :underline t))))
     '(info-menu ((,class (:foreground ,suc))))
     '(info-node ((,class (:foreground ,func :inherit bold))))
     '(info-quoted-name ((,class (:foreground ,keyword))))
     '(info-reference-item ((,class (:background nil :underline t :inherit bold))))
     '(info-string ((,class (:foreground ,str))))
     '(info-title-1 ((,class (:height 1.4 :inherit bold))))
     '(info-title-2 ((,class (:height 1.3 :inherit bold))))
     '(info-title-3 ((,class (:height 1.3))))
     '(info-title-4 ((,class (:height 1.2))))

;;;;; ivy
     '(ivy-current-match ((,class (:background ,highlight :inherit bold :extend t))))
     '(ivy-minibuffer-match-face-1 ((,class (:inherit bold))))
     '(ivy-minibuffer-match-face-2 ((,class (:foreground ,head1 :underline t))))
     '(ivy-minibuffer-match-face-3 ((,class (:foreground ,head4 :underline t))))
     '(ivy-minibuffer-match-face-4 ((,class (:foreground ,head3 :underline t))))
     '(ivy-remote ((,class (:foreground ,cyan))))

;;;;; ivy-posframe
     '(ivy-posframe ((,class (:background ,bg3))))

;;;;; latex
     '(font-latex-bold-face ((,class (:foreground ,comp))))
     '(font-latex-italic-face ((,class (:foreground ,keyword :italic t))))
     '(font-latex-match-reference-keywords ((,class (:foreground ,const))))
     '(font-latex-match-variable-keywords ((,class (:foreground ,var))))
     '(font-latex-sectioning-0-face ((,class (:inherit bold :foreground ,head3 :height ,(if spacemacs-theme-org-height 1.3 1.0) :background ,(when spacemacs-theme-org-highlight head3-bg)))))
     '(font-latex-sectioning-1-face ((,class (:inherit bold :foreground ,head4 :height ,(if spacemacs-theme-org-height 1.3 1.0) :background ,(when spacemacs-theme-org-highlight head4-bg)))))
     '(font-latex-sectioning-2-face ((,class (:inherit bold :foreground ,head1 :height ,(if spacemacs-theme-org-height 1.3 1.0) :background ,(when spacemacs-theme-org-highlight head1-bg)))))
     '(font-latex-sectioning-3-face ((,class (:inherit bold :foreground ,head2 :height ,(if spacemacs-theme-org-height 1.2 1.0) :background ,(when spacemacs-theme-org-highlight head2-bg)))))
     '(font-latex-sectioning-4-face ((,class (:bold nil :foreground ,head3 :height ,(if spacemacs-theme-org-height 1.1 1.0) :background ,(when spacemacs-theme-org-highlight head3-bg)))))
     '(font-latex-sectioning-5-face ((,class (:bold nil :foreground ,head4 :background ,(when spacemacs-theme-org-highlight head4-bg)))))
     '(font-latex-string-face ((,class (:foreground ,str))))
     '(font-latex-warning-face ((,class (:foreground ,war))))

;;;;; ledger-mode
     '(ledger-font-directive-face ((,class (:foreground ,meta))))
     '(ledger-font-posting-amount-face ((,class (:foreground ,yellow))))
     '(ledger-font-posting-date-face ((,class (:foreground ,head1))))
     '(ledger-occur-xact-face ((,class (:background ,bg2))))

;;;;; line-numbers
     '(line-number ((,class (:foreground ,lnum :background ,bg2 :inherit default))))
     '(line-number-current-line ((,class (:foreground ,base :background ,bg2 :inherit line-number))))

;;;;; lsp
     '(lsp-ui-doc-background ((,class (:background ,bg2))))
     '(lsp-ui-doc-header ((,class (:foreground ,head1 :background ,head1-bg))))

     '(lsp-ui-sideline-code-action ((,class (:foreground ,comp))))

;;;;; magit
     '(magit-blame-culprit ((,class :background ,yellow-bg :foreground ,yellow)))
     '(magit-blame-date    ((,class :background ,yellow-bg :foreground ,green)))
     '(magit-blame-hash    ((,class :background ,yellow-bg :foreground ,func)))
     '(magit-blame-header  ((,class :background ,yellow-bg :foreground ,green)))
     '(magit-blame-heading ((,class :background ,yellow-bg :foreground ,green)))
     '(magit-blame-name    ((,class :background ,yellow-bg :foreground ,yellow)))
     '(magit-blame-sha1    ((,class :background ,yellow-bg :foreground ,func)))
     '(magit-blame-subject ((,class :background ,yellow-bg :foreground ,yellow)))
     '(magit-blame-summary ((,class :background ,yellow-bg :foreground ,yellow :extend t)))
     '(magit-blame-time    ((,class :background ,yellow-bg :foreground ,green)))
     '(magit-branch ((,class (:foreground ,const :inherit bold))))
     '(magit-branch-current ((,class (:background ,blue-bg :foreground ,blue :inherit bold :box t))))
     '(magit-branch-local ((,class (:background ,blue-bg :foreground ,blue :inherit bold))))
     '(magit-branch-remote ((,class (:background ,aqua-bg :foreground ,aqua :inherit bold))))
     '(magit-diff-context-highlight ((,class (:background ,bg2 :foreground ,base :extend t))))
     '(magit-diff-hunk-heading ((,class (:background ,ttip-bg :foreground ,ttip :extend t))))
     '(magit-diff-hunk-heading-highlight ((,class (:background ,ttip-sl :foreground ,base :extend t))))
     '(magit-hash ((,class (:foreground ,var))))
     '(magit-hunk-heading           ((,class (:background ,bg3 :extend t))))
     '(magit-hunk-heading-highlight ((,class (:background ,bg3 :extend t))))
     '(magit-item-highlight ((,class :background ,bg2 :extend t)))
     '(magit-log-author ((,class (:foreground ,func))))
     '(magit-log-head-label-head ((,class (:background ,yellow :foreground ,bg1 :inherit bold))))
     '(magit-log-head-label-local ((,class (:background ,keyword :foreground ,bg1 :inherit bold))))
     '(magit-log-head-label-remote ((,class (:background ,suc :foreground ,bg1 :inherit bold))))
     '(magit-log-head-label-tags ((,class (:background ,magenta :foreground ,bg1 :inherit bold))))
     '(magit-log-head-label-wip ((,class (:background ,cyan :foreground ,bg1 :inherit bold))))
     '(magit-log-sha1 ((,class (:foreground ,str))))
     '(magit-process-ng ((,class (:foreground ,war :inherit bold))))
     '(magit-process-ok ((,class (:foreground ,func :inherit bold))))
     '(magit-reflog-amend ((,class (:foreground ,magenta))))
     '(magit-reflog-checkout ((,class (:foreground ,blue))))
     '(magit-reflog-cherry-pick ((,class (:foreground ,green))))
     '(magit-reflog-commit ((,class (:foreground ,green))))
     '(magit-reflog-merge ((,class (:foreground ,green))))
     '(magit-reflog-other ((,class (:foreground ,cyan))))
     '(magit-reflog-rebase ((,class (:foreground ,magenta))))
     '(magit-reflog-remote ((,class (:foreground ,cyan))))
     '(magit-reflog-reset ((,class (:foreground ,red))))
     '(magit-section-heading        ((,class (:foreground ,keyword :inherit bold :extend t))))
     '(magit-section-highlight      ((,class (:background ,bg2 :extend t))))
     '(magit-section-title ((,class (:background ,bg1 :foreground ,keyword :inherit bold))))

;;;;; man
     '(Man-overstrike ((,class (:foreground ,head1 :inherit bold))))
     '(Man-reverse ((,class (:foreground ,highlight))))
     '(Man-underline ((,class (:foreground ,comp :underline t))))

;;;;; markdown
     '(markdown-header-face-1 ((,class (:inherit bold :foreground ,head1 :height ,(if spacemacs-theme-org-height 1.3 1.0) :background ,(when spacemacs-theme-org-highlight head1-bg)))))
     '(markdown-header-face-2 ((,class (:inherit bold :foreground ,head2 :height ,(if spacemacs-theme-org-height 1.2 1.0) :background ,(when spacemacs-theme-org-highlight head2-bg)))))
     '(markdown-header-face-3 ((,class (:bold nil :foreground ,head3 :height ,(if spacemacs-theme-org-height 1.1 1.0) :background ,(when spacemacs-theme-org-highlight head3-bg)))))
     '(markdown-header-face-4 ((,class (:bold nil :foreground ,head4 :background ,(when spacemacs-theme-org-highlight head4-bg)))))
     '(markdown-header-face-5 ((,class (:bold nil :foreground ,head1))))
     '(markdown-header-face-6 ((,class (:bold nil :foreground ,head2))))
     '(markdown-table-face ((,class (:foreground ,base :background ,head1-bg))))

;;;;; mode-line
     '(mode-line           ((,class (:foreground ,base :background ,act1 :box (:color ,border :line-width 1)))))
     '(mode-line-buffer-id ((,class (:inherit bold :foreground ,func))))
     '(mode-line-inactive  ((,class (:foreground ,base :background ,bg1  :box (:color ,border :line-width 1)))))

;;;;; notmuch
     '(notmuch-search-date ((,class (:foreground ,func))))
     '(notmuch-search-flagged-face ((,class (:weight extra-bold))))
     '(notmuch-search-non-matching-authors ((,class (:foreground ,base-dim))))
     '(notmuch-search-unread-face ((,class (:background ,highlight-dim))))
     '(notmuch-tag-face ((,class (:foreground ,keyword))))
     '(notmuch-tag-flagged ((,class (:foreground ,war))))

;;;;; neotree
     '(neo-dir-link-face ((,class (:foreground ,keyword :inherit bold))))
     '(neo-expand-btn-face ((,class (:foreground ,base))))
     '(neo-file-link-face ((,class (:foreground ,base))))
     '(neo-root-dir-face ((,class (:foreground ,func :inherit bold))))

;;;;; org
     '(org-agenda-clocking ((,class (:background ,highlight :foreground ,comp))))
     '(org-agenda-date ((,class (:foreground ,var :height ,(if spacemacs-theme-org-agenda-height 1.1 1.0)))))
     '(org-agenda-date-today ((,class (:foreground ,keyword :inherit bold :height ,(if spacemacs-theme-org-agenda-height 1.3 1.0)))))
     '(org-agenda-date-weekend ((,class (:inherit bold :foreground ,var))))
     '(org-agenda-done ((,class (:foreground ,suc :height ,(if spacemacs-theme-org-agenda-height 1.2 1.0)))))
     '(org-agenda-structure ((,class (:inherit bold :foreground ,comp))))
     '(org-block ((,class (:background ,cblk-bg :foreground ,cblk :extend t))))
     '(org-block-begin-line ((,class (:background ,cblk-ln-bg :foreground ,cblk-ln :extend t))))
     '(org-block-end-line ((,class (:background ,cblk-ln-bg :foreground ,cblk-ln :extend t))))
     '(org-clock-overlay ((,class (:foreground ,comp))))
     '(org-code ((,class (:foreground ,cyan))))
     '(org-column ((,class (:background ,highlight :inherit ,(if spacemacs-theme-org-height 'default)))))
     '(org-column-title ((,class (:background ,highlight))))
     '(org-date ((,class (:underline t :foreground ,var))))
     '(org-date-selected ((,class (:background ,func :foreground ,bg1))))
     '(org-document-info-keyword ((,class (:foreground ,meta))))
     '(org-document-title ((,class (:foreground ,func :inherit bold :height ,(if spacemacs-theme-org-height 1.4 1.0) :underline t))))
     '(org-done ((,class (:foreground ,suc :inherit bold :background ,green-bg))))
     '(org-ellipsis ((,class (:foreground ,keyword))))
     '(org-footnote  ((,class (:underline t :foreground ,base))))
     '(org-headline-done ((,class (:foreground ,aqua))))
     '(org-headline-todo ((,class (:foreground ,meta))))
     '(org-hide ((,class (:foreground ,base))))
     '(org-kbd ((,class (:inherit region :foreground ,base :box (:line-width 1 :style released-button)))))
     '(org-level-1 ((,class (:inherit bold :bold ,(if spacemacs-theme-org-bold 'unspecified nil) :foreground ,head1 :height ,(if spacemacs-theme-org-height 1.3 1.0) :background ,(when spacemacs-theme-org-highlight head1-bg)))))
     '(org-level-2 ((,class (:inherit bold :bold ,(if spacemacs-theme-org-bold 'unspecified nil) :foreground ,head2 :height ,(if spacemacs-theme-org-height 1.2 1.0) :background ,(when spacemacs-theme-org-highlight head2-bg)))))
     '(org-level-3 ((,class (:bold nil :foreground ,head3 :height ,(if spacemacs-theme-org-height 1.1 1.0) :background ,(when spacemacs-theme-org-highlight head3-bg)))))
     '(org-level-4 ((,class (:bold nil :foreground ,head4 :background ,(when spacemacs-theme-org-highlight head4-bg)))))
     '(org-level-5 ((,class (:bold nil :foreground ,head1))))
     '(org-level-6 ((,class (:bold nil :foreground ,head2))))
     '(org-level-7 ((,class (:bold nil :foreground ,head3))))
     '(org-level-8 ((,class (:bold nil :foreground ,head4))))
     '(org-link ((,class (:underline t :foreground ,comment))))
     '(org-meta-line ((,class (:foreground ,meta))))
     '(org-mode-line-clock-overrun ((,class (:foreground ,err))))
     '(org-priority ((,class (:foreground ,war :inherit bold :bold ,(if spacemacs-theme-org-priority-bold 'unspecified nil)))))
     '(org-quote ((,class (:inherit org-block :slant italic))))
     '(org-scheduled ((,class (:foreground ,comp))))
     '(org-scheduled-today ((,class (:foreground ,func :height ,(if spacemacs-theme-org-agenda-height 1.2 1.0)))))
     '(org-scheduled-previously ((,class (:foreground ,base :slant italic))))
     '(org-sexp-date ((,class (:foreground ,base))))
     '(org-special-keyword ((,class (:foreground ,func))))
     '(org-table ((,class (:foreground ,base :background ,head1-bg))))
     '(org-tag ((,class (:foreground ,meta))))
     '(org-time-grid ((,class (:foreground ,str))))
     '(org-todo ((,class (:foreground ,war :inherit bold :background ,yellow-bg))))
     '(org-upcoming-deadline ((,class (:foreground ,war :inherit org-priority))))
     '(org-upcoming-distant-deadline ((,class (:foreground ,suc :inherit org-priority))))
     '(org-verbatim ((,class (:foreground ,keyword))))
     '(org-verse ((,class (:inherit org-block :slant italic))))
     '(org-warning ((,class (:foreground ,err :inherit org-priority))))

;;;;; outline
     '(outline-1 ((,class (:inherit org-level-1))))
     '(outline-2 ((,class (:inherit org-level-2))))
     '(outline-3 ((,class (:inherit org-level-3))))
     '(outline-4 ((,class (:inherit org-level-4))))
     '(outline-5 ((,class (:inherit org-level-5))))
     '(outline-6 ((,class (:inherit org-level-6))))
     '(outline-7 ((,class (:inherit org-level-7))))
     '(outline-8 ((,class (:inherit org-level-8))))

;;;;; perspective
     '(persp-selected-face ((,class (:inherit bold :foreground ,func))))

;;;;; popup
     '(popup-enu-selection-face ((,class (:background ,ttip-sl :foreground ,base))))
     '(popup-face ((,class (:background ,ttip-bg :foreground ,ttip))))
     '(popup-isearch-match ((,class (:inherit match))))
     '(popup-menu-face ((,class (:background ,ttip-bg :foreground ,base))))
     '(popup-menu-mouse-face ((,class (:inherit highlight))))
     '(popup-scroll-bar-background-face ((,class (:background ,bg2))))
     '(popup-scroll-bar-foreground-face ((,class (:background ,act2))))
     '(popup-tip-face ((,class (:background ,ttip-sl :foreground ,base :bold nil :italic nil :underline nil))))

;;;;; rainbow-delimiters
     '(rainbow-delimiters-depth-1-face ((,class :foreground ,keyword)))
     '(rainbow-delimiters-depth-2-face ((,class :foreground ,func)))
     '(rainbow-delimiters-depth-3-face ((,class :foreground ,str)))
     '(rainbow-delimiters-depth-4-face ((,class :foreground ,green)))
     '(rainbow-delimiters-depth-5-face ((,class :foreground ,yellow)))
     '(rainbow-delimiters-depth-6-face ((,class :foreground ,keyword)))
     '(rainbow-delimiters-depth-7-face ((,class :foreground ,func)))
     '(rainbow-delimiters-depth-8-face ((,class :foreground ,str)))
     '(rainbow-delimiters-mismatched-face ((,class :foreground ,err :overline t)))
     '(rainbow-delimiters-unmatched-face ((,class :foreground ,err :overline t)))

;;;;; rcirc
     '(rcirc-bright-nick ((,class (:background ,aqua-bg :foreground ,cyan))))
     '(rcirc-dim-nick ((,class (:foreground ,base-dim))))
     '(rcirc-keyword ((,class (:background ,green-bg-s :foreground ,green))))
     '(rcirc-timestamp ((,class (:foreground ,keyword))))
     '(rcirc-track-keyword ((,class (:background ,green :foreground ,bg1))))
     '(rcirc-url ((,class (:inherit link))))

;;;;; sh-mode
     '(sh-heredoc ((,class :foreground ,str)))
     '(sh-quoted-exec ((,class :foreground ,func)))

;;;;; show-paren
     '(show-paren-match ((,class (:foreground ,mat :inherit bold  :underline ,(when spacemacs-theme-underline-parens t)))))
     '(show-paren-match-expression ((,class (:background ,green-bg-s))))
     '(show-paren-mismatch ((,class (:foreground ,err :inherit bold :underline ,(when spacemacs-theme-underline-parens t)))))

;;;;; smartparens
     '(sp-pair-overlay-face ((,class (:background ,highlight :foreground nil))))
     '(sp-show-pair-match-face ((,class (:foreground ,mat :inherit bold  :underline ,(when spacemacs-theme-underline-parens t)))))

;;;;; smerge
     '(smerge-base ((,class (:background ,yellow-bg :extend t))))
     '(smerge-markers ((,class (:background ,ttip-bg :foreground ,ttip :extend t))))
     '(smerge-mine ((,class (:background ,red-bg))))
     '(smerge-other ((,class (:background ,green-bg))))
     '(smerge-refined-added ((,class (:background ,green-bg-s :foreground ,green))))
     '(smerge-refined-changed ((,class (:background ,blue-bg-s :foreground ,blue))))
     '(smerge-refined-removed ((,class (:background ,red-bg-s :foreground ,red))))

;;;;; solaire
     '(solaire-default-face ((,class (:inherit default :background ,bg2))))
     '(solaire-minibuffer-face ((,class (:inherit default :background ,bg2))))
     '(solaire-hl-line-face ((,class (:inherit hl-line :background ,bg2))))
     '(solaire-org-hide-face ((,class (:inherit org-hide :background ,bg2))))

;;;;; spacemacs-specific
     '(spacemacs-transient-state-title-face ((,class (:background nil :foreground ,comp :box nil :inherit bold))))

;;;;; swiper
     '(swiper-line-face ((,class (:background ,highlight :inherit bold))))
     '(swiper-match-face-1 ((,class (:inherit bold))))
     '(swiper-match-face-2 ((,class (:foreground ,head1 :underline t))))
     '(swiper-match-face-3 ((,class (:foreground ,head4 :underline t))))
     '(swiper-match-face-4 ((,class (:foreground ,head3 :underline t))))

;;;;; tabbar
     '(tabbar-button ((,class (:inherit tabbar-default ))))
     '(tabbar-button-highlight ((,class (:inherit tabbar-default))))
     '(tabbar-default ((,class (:background ,bg1 :foreground ,head1 :height 0.9))))
     '(tabbar-highlight ((,class (:underline t))))
     '(tabbar-selected ((,class (:inherit tabbar-default :foreground ,func :weight bold))))
     '(tabbar-selected-modified ((,class (:inherit tabbar-default :foreground ,red :weight bold))))
     '(tabbar-separator ((,class (:inherit tabbar-default))))
     '(tabbar-unselected ((,class (:inherit tabbar-default :background ,bg1 :slant italic :weight light))))
     '(tabbar-unselected-modified ((,class (:inherit tabbar-unselected :background ,bg1 :foreground ,red))))

;;;;; term
     '(term ((,class (:foreground ,base :background ,bg1))))
     '(term-color-black ((,class (:foreground ,bg4 :background ,bg4))))
     '(term-color-blue ((,class (:foreground ,keyword :background ,keyword))))
     '(term-color-cyan ((,class (:foreground ,cyan :background ,cyan))))
     '(term-color-green ((,class (:foreground ,green :background ,green))))
     '(term-color-magenta ((,class (:foreground ,magenta :background ,magenta))))
     '(term-color-red ((,class (:foreground ,red :background ,red))))
     '(term-color-white ((,class (:foreground ,base :background ,base))))
     '(term-color-yellow ((,class (:foreground ,yellow :background ,yellow))))

;;;;; todo-mode
     '(todo-date ((,class (:foreground ,func ))))
     '(todo-done ((,class (:foreground ,suc :inherit bold :background ,green-bg))))
     '(todo-time ((,class (:foreground ,str))))
     '(todo-nondiary ((,class (:foreground ,meta))))

;;;;; vertico
     '(vertico-current ((,class (:background ,highlight :extend t))))
     '(completions-common-part ((,class (:foreground ,var))))

;;;;; vterm
     '(vterm-color-default ((,class (:foreground ,base :background ,bg1))))
     ;; vterm-color-black used to render black color code.
     ;; The foreground color is used as ANSI color 0 and the background
     ;; color is used as ANSI color 8.
     '(vterm-color-black ((,class (:foreground ,bg4 :background ,bg4))))
     '(vterm-color-blue ((,class (:foreground ,blue :background ,blue))))
     '(vterm-color-cyan ((,class (:foreground ,cyan :background ,cyan))))
     '(vterm-color-green ((,class (:foreground ,green :background ,green))))
     '(vterm-color-magenta ((,class (:foreground ,magenta :background ,magenta))))
     '(vterm-color-red ((,class (:foreground ,red  :background ,red))))
     '(vterm-color-white ((,class (:foreground ,base  :background ,base))))
     '(vterm-color-yellow ((,class (:foreground ,yellow   :background ,yellow))))

;;;;; tide
     '(tide-hl-identifier-face ((,class (:foreground ,yellow :background ,yellow-bg))))

;;;;; treemacs
     '(treemacs-git-added-face ((,class (:foreground ,green :background ,green-bg))))
     '(treemacs-git-conflict-face ((,class (:foreground ,red :background ,red-bg))))
     '(treemacs-git-ignored-face ((,class (:foreground ,yellow))))
     '(treemacs-git-modified-face ((,class (:foreground ,blue :background ,blue-bg))))
     '(treemacs-git-untracked-face ((,class (:foreground ,aqua :background ,aqua-bg))))

;;;;; tab-bar-mode
     '(tab-bar ((,class (:foreground ,base :background ,bg1))))
     '(tab-bar-tab ((,class (:foreground ,base :background ,bg1 :weight bold))))
     '(tab-line ((,class (:foreground ,base :background ,bg1))))
     '(tab-bar-tab-inactive ((,class (:foreground ,base-dim :background ,bg2 :weight light))))

;;;;; web-mode
     '(web-mode-builtin-face ((,class (:inherit ,font-lock-builtin-face))))
     '(web-mode-comment-face ((,class (:inherit ,font-lock-comment-face))))
     '(web-mode-constant-face ((,class (:inherit ,font-lock-constant-face))))
     '(web-mode-current-element-highlight-face ((,class (:background ,bg3))))
     '(web-mode-doctype-face ((,class (:inherit ,font-lock-comment-face))))
     '(web-mode-function-name-face ((,class (:inherit ,font-lock-function-name-face))))
     '(web-mode-html-attr-name-face ((,class (:foreground ,func))))
     '(web-mode-html-attr-value-face ((,class (:foreground ,keyword))))
     '(web-mode-html-tag-face ((,class (:foreground ,keyword))))
     '(web-mode-keyword-face ((,class (:foreground ,keyword))))
     '(web-mode-string-face ((,class (:foreground ,str))))
     '(web-mode-symbol-face ((,class (:foreground ,type))))
     '(web-mode-type-face ((,class (:inherit ,font-lock-type-face))))
     '(web-mode-warning-face ((,class (:inherit ,font-lock-warning-face))))

;;;;; which-key
     '(which-key-command-description-face ((,class (:foreground ,base))))
     '(which-key-group-description-face ((,class (:foreground ,keyword))))
     '(which-key-key-face ((,class (:foreground ,func :inherit bold))))
     '(which-key-separator-face ((,class (:background nil :foreground ,str))))
     '(which-key-special-key-face ((,class (:background ,func :foreground ,bg1))))

;;;;; which-function-mode
     '(which-func ((,class (:foreground ,func))))

;;;;; whitespace-mode
     '(whitespace-empty ((,class (:background nil :foreground ,yellow))))
     '(whitespace-indentation ((,class (:background nil :foreground ,war))))
     '(whitespace-line ((,class (:background nil :foreground ,comp))))
     '(whitespace-newline ((,class (:background nil :foreground ,comp))))
     '(whitespace-space ((,class (:background nil :foreground ,act2))))
     '(whitespace-space-after-tab ((,class (:background nil :foreground ,yellow))))
     '(whitespace-space-before-tab ((,class (:background nil :foreground ,yellow))))
     '(whitespace-tab ((,class (:background nil :foreground ,act2))))
     '(whitespace-trailing ((,class (:background ,err :foreground ,war))))

;;;;; other, need more work
     '(ac-completion-face ((,class (:underline t :foreground ,keyword))))
     '(ffap ((,class (:foreground ,base))))
     '(flx-highlight-face ((,class (:foreground ,comp :underline nil))))
     '(icompletep-determined ((,class :foreground ,keyword)))
     '(js2-external-variable ((,class (:foreground ,comp))))
     '(js2-function-param ((,class (:foreground ,const))))
     '(js2-jsdoc-html-tag-delimiter ((,class (:foreground ,str))))
     '(js2-jsdoc-html-tag-name ((,class (:foreground ,keyword))))
     '(js2-jsdoc-value ((,class (:foreground ,str))))
     '(js2-private-function-call ((,class (:foreground ,const))))
     '(js2-private-member ((,class (:foreground ,base))))
     '(js3-error-face ((,class (:underline ,war))))
     '(js3-external-variable-face ((,class (:foreground ,var))))
     '(js3-function-param-face ((,class (:foreground ,keyword))))
     '(js3-instance-member-face ((,class (:foreground ,const))))
     '(js3-jsdoc-tag-face ((,class (:foreground ,keyword))))
     '(js3-warning-face ((,class (:underline ,keyword))))
     '(slime-repl-inputed-output-face ((,class (:foreground ,comp))))
     '(trailing-whitespace ((,class :foreground nil :background ,err)))
     '(undo-tree-visualizer-current-face ((,class :foreground ,keyword)))
     '(undo-tree-visualizer-default-face ((,class :foreground ,base)))
     '(undo-tree-visualizer-register-face ((,class :foreground ,comp)))
     '(undo-tree-visualizer-unmodified-face ((,class :foreground ,var))))

    (custom-theme-set-variables
     ,theme-name

;;;;; ansi-color-names
     '(ansi-color-names-vector [,bg4 ,red ,green ,yellow ,blue ,magenta ,cyan ,base])

;;;;; hl-todo
     '(hl-todo-keyword-faces '(("TODO"        . ,war)
                               ("NEXT"        . ,war)
                               ("THEM"        . ,aqua)
                               ("PROG"        . ,blue)
                               ("OKAY"        . ,blue)
                               ("DONT"        . ,red)
                               ("FAIL"        . ,red)
                               ("DONE"        . ,suc)
                               ("NOTE"        . ,yellow)
                               ("KLUDGE"      . ,yellow)
                               ("HACK"        . ,yellow)
                               ("TEMP"        . ,yellow)
                               ("FIXME"       . ,war)
                               ("XXX+"        . ,war)
                               ("\\?\\?\\?+"  . ,war)))
;;;;; org
     '(org-fontify-done-headline nil)
     '(org-fontify-todo-headline nil)

;;;;; pdf-tools
     '(pdf-view-midnight-colors '(,base . ,bg1))))
  ))


;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide 'spacemacs-common)

;; Local Variables:
;; no-byte-compile: nil
;; End:

;;; spacemacs-common.el ends here

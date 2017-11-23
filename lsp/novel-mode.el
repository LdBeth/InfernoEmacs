;;; novel-mode.el --- screen reader

;; Copyright (C) 2016, T.v.Dein <tlinden@cpan.org>

;; This file is NOT part of Emacs.

;; This  program is  free  software; you  can  redistribute it  and/or
;; modify it  under the  terms of  the GNU  General Public  License as
;; published by the Free Software  Foundation; either version 2 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT  ANY  WARRANTY;  without   even  the  implied  warranty  of
;; MERCHANTABILITY or FITNESS  FOR A PARTICULAR PURPOSE.   See the GNU
;; General Public License for more details.

;; You should have  received a copy of the GNU  General Public License
;; along  with  this program;  if  not,  write  to the  Free  Software
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
;; USA

;; Version: 0.01
;; Author: T.v.Dein <tlinden@cpan.org>
;; Keywords: read books novels
;; URL: https://github.com/tlinden/novel-mode
;; License: GNU General Public License >= 2

;;; Commentary:

;; Novel  mode is  a minor  mode which  converts emacs  into a  screen
;; reader, or in other words,  it enables distraction free reading. It
;; is however not  suited for distraction free  editing. Try writeroom
;; mode if you're looking for this.

;; When turned on, it does the following conversions:

;;   - disable almost all distractions, as menu, toolbar, scrollbar
;;   - enlarge font size
;;   - switch to variable width font
;;   - enable word wrap (without fringe marker)
;;   - increase line spacing
;;   - add a window margin to the left and right (thereby centering the text)
;;   - disable all input keys (rendering the buffer read-only)
;;   - disable the cursor
;;   - switch to buffer-scrolling (like e.g. in Acroread)
;;   - display current reading position in percent
;;   - add a couple of convenience one-key commands

;; Novel mode provides the following one-key commands, when active:

;;     n           scroll one page down
;;     p           scroll one page up
;;     <down>      scroll one line down
;;     <up>        scroll one line up
;;     mousewheel  scroll linewise up or down
;;     SPC         scroll one page down
;;     <left>      increase margins, makes visible text narrower
;;     <right>     decrease margins, makes visible text wider
;;     +           increase font size
;;     -           decrease font size
;;     i           invert video display
;;     q           quit novel mode
;;     ?           display key mapping

;; Important: while normal  key input (beside the  ones listed above),
;; is disabled, Control and Meta still work, of course. Please be also
;; aware that this mode might conflict with god-mode or evil-mode.

;; If you use this  mode quite often, then it might be  a good idea to
;; use save-place mode,  so that a text file will  be opened where you
;; left last time (just like any  ebook reader would do. Here's how to
;; do that:

;;     (if (version< emacs-version "25.0")
;;         (progn
;;           (require 'saveplace)
;;           (setq-default save-place t))
;;       (save-place-mode 1))


;; The name  novel mode is  not my idea,  there's a function  on Xah's
;; ergomacs   page  with   a  function   for  this   kind  of   stuff:
;; http://ergoemacs.org/emacs/emacs_novel_reading_mode.html.  In fact,
;; this mode is based on this function, I had it in my .emacs file and
;; enhanced it  all the  time.  At  some point it  made more  sence to
;; maintain this baby in its own mode - hence novel-mode.

;;; Install:

;; To use, save novel-mode.el to a directory in your load-path.

;; Add something like this to your config:

;;     (require 'novel-mode)
;;     (add-hook 'text-mode-hook 'novel-mode)

;; or load it manually, when needed:

;;     M-x novel-mode

;;; Customize:

;; You can customize the following variables:

;; To setup a default left and right margin, use this:

;;     (setq novel-default-margin 50)

;; All available  novel-mode variables  can be  modified interactively
;; with:

;;      M-x customize-group RET novel-mode RET

;; You can also use hooks to novel  mode as a way to modify or enhance
;; its behavior.  The following hooks are available:

;;     novel-mode-pre-start-hook
;;     novel-mode-post-start-hook
;;     novel-mode-pre-stop-hook
;;     novel-mode-post-stop-hook

;; Example:

;;     (add-hook 'novel-mode-post-start-hook
;;               (lambda ()
;;                 (set-face-font 'default "DejaVu Sans")))
;;     (add-hook 'novel-mode-post-stop-hook
;;               (lambda ()
;;                 (set-face-font 'default "Courier")))




;;; Code:
;;;; Constants:

(defconst novel-mode-versioni "0.01" "Novel mode version")

(defgroup novel-mode nil
  "screen reader mode"
  :group 'extensions
  :group 'tools
  :link '(url-link :tag "Repository" "https://github.com/tlinden/novel-mode"))

;; various vars to remember previous states
(defvar novel--mlf nil)
(defvar novel--vlm nil)
(defvar novel--ww  nil)
(defvar novel--mbm nil)
(defvar novel--tbm nil)
(defvar novel--sbm nil)
(defvar novel--ct nil)

;; set on startup
(defvar novel--max-margin nil)
(defvar novel--cur-margin nil)

;; remember last invertion state, if any. we start with t by purpose, keep this
(defvar novel--invert-state t)


;;;; Customizable variables:

(defcustom novel-default-margin nil
  "Initial margin (used left+right) in chars, calculated if nil.")

(defcustom novel-feedback nil
  "Display feedback in minibuffer on actions")


;;;; Internal Functions:

(defun novel--cursor-pos()
  "return current cursor position in %"
  ;; percent code stolen shamelessly from simple.el/what-cursor-position
  (interactive)
  (let* ((total (buffer-size))
         (pos (point))
         (percent (if (> total 50000)
                      ;; Avoid overflow from multiplying by 100!
                      (/ (+ (/ total 200) (1- pos)) (max (/ total 100) 1))
                    (/ (+ (/ total 2) (* 100 (1- pos))) (max total 1)))))
    (message "%S%%" percent)))

(defun novel--set-margins()
  "Set window margins"
  (interactive)
  (set-window-margins nil novel--cur-margin novel--cur-margin)
  (when novel-feedback
    (message "set window margins to: %S (max: %S)"
             novel--cur-margin novel--max-margin)))


;; disable self-insert-command.
;; remap code by Thorsten Jolitz, outshine.el, GPL2.0+
(defun novel--self-insert-command(N)
  (interactive "c")
  (message "try ?"))

(defun novel--remap-self-insert()
  (novel--remap novel-mode-map 'self-insert-command 'novel--self-insert-command))

(defun novel--reset-remap-self-insert()
  (novel--remap novel-mode-map 'novel--self-insert-command 'self-insert-command))

(defun novel--defkey (keymap key def)
  "Define a KEY in a KEYMAP with definition DEF."
  (define-key keymap key def))

(defun novel--remap (map &rest commands)
  "In MAP, remap the functions given in COMMANDS.
COMMANDS is a list of alternating OLDDEF NEWDEF command names."
  (let (new old)
    (while commands
      (setq old (pop commands) new (pop commands))
      (if (fboundp 'command-remapping)
          (novel--defkey map (vector 'remap old) new)
        (substitute-key-definition old new map global-map)))))

(defun novel--backup-states()
  "Store current states in variables for later restoration"
  ;; various vars to remember previous states
  (setq novel--mlf mode-line-format
        novel--vlm visual-line-mode
        novel--ww  word-wrap
        novel--mbm menu-bar-mode
        novel--tbm tool-bar-mode
        novel--ct  cursor-type
        novel--sbm scroll-bar-mode))


;;;; Hooks:

(defvar novel-mode-pre-start-hook ()
  "Called before startup")

(defvar novel-mode-post-start-hook ()
  "Called after startup")

(defvar novel-mode-pre-stop-hook ()
  "Called before stopping")

(defvar novel-mode-post-stop-hook ()
  "Called after stopping")

;;;; API Functions:

(defun novel-up()
  "Scroll buffer one line up"
  (interactive)
  (scroll-down 1)
  (novel--cursor-pos))

(defun novel-down()
  "Scroll buffer one line down"
  (interactive)
  (scroll-up 1)
  (novel--cursor-pos))

(defun novel-page-up()
  "Scroll buffer one page up"
  (interactive)
  (scroll-down)
  (novel--cursor-pos))

(defun novel-page-down()
  "Scroll buffer one page down"
  (interactive)
  (scroll-up)
  (novel--cursor-pos))

;; invert everything, reverse it when called again
(defun novel-invert()
  "Invert foreground and background colors"
  (interactive)
  (invert-face 'default)
  (set-face-attribute 'fringe nil :inverse-video novel--invert-state)
  (setq novel--invert-state (not novel--invert-state)) ;; cycle
  (when novel-feedback
    (message "inverted colors")))

(defun novel-incr-margins()
  "Increment window margins"
  (interactive)
  (setq novel--cur-margin (if (= novel--cur-margin novel--max-margin)
                              novel--cur-margin
                            (+ novel--cur-margin 1)))
  (novel--set-margins))

(defun novel-decr-margins()
  "Decrement window margins"
  (interactive)
  (setq novel--cur-margin (if (= novel--cur-margin 0) 0 (- novel--cur-margin 1)))
  (novel--set-margins))

(defun novel-incr-font-size ()
  "Increase font size"
  (interactive)
  (text-scale-increase 0.5)
  (when novel-feedback
    (message "increased font size")))

(defun novel-decr-font-size ()
  "Decrease font size"
  (interactive)
  (text-scale-increase -0.5)
  (when novel-feedback
    (message "decreased font size")))

(defun novel-toggle()
  "Toggle reading mode"
  (interactive)
  (if (null (get this-command 'state-on-p))
      ;; enable, primary novel mode setup
      (progn
        (novel--backup-states)
        (run-hooks 'novel-mode-pre-start-hook)
        
        (setq novel--max-margin (/ (- (window-body-width) 40) 2))
        (if (not novel-default-margin)
            (setq novel-default-margin (/ (- (window-body-width) fill-column) 3)))
        (setq novel--cur-margin novel-default-margin)
        
        (setq scroll-step            1       ; scroll linewise
              scroll-conservatively  10000
              cursor-type            nil     ; no cursor 
              line-spacing           3       ; more distance between lines
              mode-line-format       nil     ; no modeline
              visual-line-mode       t       ; no wrap marker on fringe
              word-wrap              t
              )
        
        (delete-other-windows)               ; There can be only one, McLeod.
        (novel--set-margins)
        
        (variable-pitch-mode 1)              ; enable variable width font
        (text-scale-increase 2)              ; larger font size
        (put this-command 'state-on-p t)     ; remeber current state
        
        (menu-bar-mode          -1)          ; disable widgets
        (tool-bar-mode          -1)
        (scroll-bar-mode        0)
        (set-fringe-mode        0)           ; no fringe
        
        (novel--remap-self-insert)           ; disable all keys but ours

        (run-hooks 'novel-mode-post-start-hook)
        )
    
    ;; disable, restore everything back to normal
    (progn
      (run-hooks 'novel-mode-pre-stop-hook)
      
      (setq scroll-step            0
            scroll-conservatively  0
            line-spacing           nil
            cursor-type            novel--ct
            mode-line-format       novel--mlf
            visual-line-mode       novel--vlm
            word-wrap              novel--ww
            menu-bar-mode          novel--mbm
            tool-bar-mode          novel--tbm
            scroll-bar-mode        novel--sbm)

      (set-fringe-mode        1)
      (set-window-margins nil 0 0)
      (variable-pitch-mode 0)
      (text-scale-increase -2)
      (put this-command 'state-on-p nil)
      (novel--reset-remap-self-insert)

      (if (not novel--invert-state)
          (novel-invert))

      (run-hooks 'novel-mode-post-stop-hook)
      ))
  (redraw-frame (selected-frame)))

(defun novel-help()
  "Display help"
  (interactive)
  (message "Available commands in novel mode:
   n           scroll one page down
   p           scroll one page up
   <down>      scroll one line down
   <up>        scroll one line up
   mousewheel  scroll linewise up or down
   SPC         scroll one page down
   <left>      increase margins, makes visible text narrower
   <right>     decrease margins, makes visible text wider
   +           increase font size
   -           decrease font size
   i           invert video display
   q           quit novel mode
   ? or h      display key mapping
"))

;;;; Interface

;;;###autoload
(define-minor-mode novel-mode "screen reader mode"
  :lighter " V"
  :group 'novel-mode
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "<up>")      'novel-up)
            (define-key map (kbd "<down>")    'novel-down)
            (define-key map (kbd "<mouse-4>") 'novel-up)
            (define-key map (kbd "<mouse-5>") 'novel-down)

            (define-key map (kbd "SPC")       'novel-page-down)
            (define-key map (kbd "<next>")    'novel-page-down)
            (define-key map (kbd "<prior>")   'novel-page-up)
            (define-key map (kbd "n")         'novel-page-down)
            (define-key map (kbd "p")         'novel-page-up)

            (define-key map (kbd "<right>")   'novel-decr-margins)
            (define-key map (kbd "<left>")    'novel-incr-margins)

            (define-key map (kbd "i")         'novel-invert)
            (define-key map (kbd "+")         'novel-incr-font-size)
            (define-key map (kbd "-")         'novel-decr-font-size)
            (define-key map (kbd "q")         'novel-mode)

            (define-key map (kbd "h")         'novel-help)
            (define-key map (kbd "?")         'novel-help)
            map)
  (novel-toggle)
  )


(provide 'novel-mode)

;;; novel-mode.el ends here

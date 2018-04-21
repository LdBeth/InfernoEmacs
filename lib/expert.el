(defalias 'yes-or-no-p 'y-or-n-p)

(setq make-backup-files nil)

(auto-save-mode 1)
(setq auto-save-default t)

(setq require-final-newline t
      track-eol t)

(set-buffer-file-coding-system 'utf-8)

(setq buffer-file-coding-system 'utf-8)

(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq file-name-coding-system 'utf-8)

(setq-default major-mode 'text-mode)

(setq visible-bell nil)

(setq kill-ring-max 175)

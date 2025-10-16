;; -*- lexical-binding: t; -*-
(setq native-comp-jit-compilation nil)
(package-initialize)
(load (concat user-emacs-directory "pkgs"))
(dolist (package '(mwim unfill ctrlf yasnippet ;orderless
                        which-key bind-key))
  (require package))

(dolist (file '("keys" "which-key" "tecoline" "set"
                "spacemacs-buffer" "filladapt" "pkgs" "hact" "hhist"
                "page-break-lines" "tex-site"
                "dim-paren" "ctrlf"))
  (copy-file (let ((default-directory
                    (concat "~/.emacs.d/eln-cache/" comp-native-version-dir)))
               (car (file-expand-wildcards (concat file "-*") t)))
             (concat "/Applications/Emacs.app/Contents/Frameworks/native-lisp/"
                     comp-native-version-dir "/")
             t))

(defconst dumped-load-path load-path)
(defconst dumped-eln-path native-comp-eln-load-path)
(garbage-collect)
(dump-emacs-portable "~/.emacs.d/Emacs.pdmp")

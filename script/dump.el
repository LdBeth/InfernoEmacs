;; -*- lexical-binding: t; -*-
(setq native-comp-jit-compilation nil)
(package-initialize)
(load (concat user-emacs-directory "pkgs"))
(dolist (package '(mwim unfill ctrlf yasnippet ;orderless
                        which-key bind-key))
  (require package))

(dolist (file '("keys" "which-key" "tecoline"
                "spacemacs-buffer" "filladapt" "pkgs"
                "page-break-lines" "tex-site"
                "dim-paren" "ctrlf"))
  (let* ((default-directory
          (concat "~/.emacs.d/eln-cache/" comp-native-version-dir))
         (filename (car (file-expand-wildcards (concat file "-*") t))))
    (if filename
        (copy-file filename
                   (concat "/Applications/Emacs.app/Contents/Frameworks/native-lisp/"
                           comp-native-version-dir "/")
                   t)
      (error "file: %s not exists!" file))))

(defconst dumped-load-path load-path)
(defconst dumped-eln-path native-comp-eln-load-path)
(garbage-collect)
(dump-emacs-portable "~/.emacs.d/Emacs.pdmp")

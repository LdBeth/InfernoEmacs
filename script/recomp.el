;; -*- lexical-binding: t; -*-
(byte-recompile-directory (concat user-emacs-directory "core"))
(dolist (f '("params.el" "keys.el" "pkgs.el"))
  (byte-compile-file (concat user-emacs-directory f)))
(package-initialize)
(load (concat user-emacs-directory "pkgs"))
(dolist (package '(mwim unfill ctrlf yasnippet orderless
                        which-key bind-key))
    (require package))
(native-compile-prune-cache)

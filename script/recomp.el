(byte-recompile-directory (concat user-emacs-directory "core"))
(dolist (f '("params.el" "keys.el" "pkgs.el"))
  (byte-compile-file (concat user-emacs-directory f)))
(loaddefs-generate (concat user-emacs-directory "core")
                   (concat user-emacs-directory "core/core-autoloads.el"))

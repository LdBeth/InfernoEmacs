(excl
 (progn (setq custom-file ($ locate-user-emacs-file "custom.el"))
        (load ($ concat
                 inferno-start-directory
                 "straight/bootstrap.el") nil 'nomessage)))

(eval-when-compile
  (require 'use-package))

(repeat-declare-syntax use-package nil
  diminish
  bind-key)

(provide 'inferno-pkg)

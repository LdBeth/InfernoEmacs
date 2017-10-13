;; Without this comment emacs25 adds (package-initialize) here
;; (package-initialize)

;; Increase gc-cons-threshold, depending on your system you may set it back to a
;; lower value in your dotfile (function `dotspacemacs/user-config')
(setq gc-cons-threshold 100000000)

(defconst inferno-emacs-min-version "25.3" "Mininal version of Emacs.")
(defvar inferno-temacs nil "Boolean to decide use autoload or not.")

(if (not (version<= inferno-emacs-min-version emacs-version))
    (error "Inferno only offically support Emacs %s or later."
           inferno-emacs-min-version)
  (load (concat (file-name-directory load-file-name)
                "boot/kernel")))

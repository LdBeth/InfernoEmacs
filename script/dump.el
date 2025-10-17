;; -*- lexical-binding: t; -*-
(setq native-comp-jit-compilation nil)
(package-initialize)
(load (concat user-emacs-directory "pkgs"))
(dolist (package '(mwim unfill ctrlf yasnippet orderless
                        which-key bind-key))
  (require package))

(let (loaded-config-files) 
  (mapcar (lambda (pair)
            (if (and (string-prefix-p (expand-file-name user-emacs-directory)
                                      (car pair))
                     (not (string-suffix-p "-autoloads.el"
                                           (car pair))))
                (push (file-name-base (car pair)) loaded-config-files)))
          load-history)

  (dolist (file loaded-config-files)
    (let* ((default-directory
            (concat "~/.emacs.d/eln-cache/" comp-native-version-dir))
           (filename (car (file-expand-wildcards (concat file "-*") t))))
      (if filename
          (copy-file filename
                     (concat "/Applications/Emacs.app/Contents/Frameworks/native-lisp/"
                             comp-native-version-dir "/")
                     t)
        (warn "%s not exists!" file)))))

(defconst dumped-load-path load-path)
(defconst dumped-eln-path native-comp-eln-load-path)
(garbage-collect)
(dump-emacs-portable "~/.emacs.d/Emacs.pdmp")

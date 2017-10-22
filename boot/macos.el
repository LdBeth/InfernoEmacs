;;; Modifiers
(setq mac-command-modifier 'hyper
      mac-option-modifier 'meta)

(defvar mac-modifier-registry nil
  "Used to store the vaule of `mac-command-modifier' and `mac-option-modifier'.")

(define-minor-mode option-input-mode
  "Swap `mac-command-modifier' and `mac-option-modifier' at OS X/macOS."
  nil "⌥"
  :global t
  (if (if (eq arg 'toggle)
          (not option-input-mode)
        (> (prefix-numeric-value arg) 0))
      (setq mac-modifier-registry
            (cons mac-command-modifier
                  mac-option-modifier)
            mac-command-modifier 'meta
            mac-option-modifier 'alt)
    (setq mac-command-modifier (car mac-modifier-registry)
          mac-option-modifier (cdr mac-modifier-registry)
          mac-modifier-registry nil)))


;;; PLIST
;; Allow editing of binary .plist files.
(add-to-list 'jka-compr-compression-info-list
             ["\\.plist$"
              "converting text XML to binary plist"
              "plutil"
              ("-convert" "binary1" "-o" "-" "-")
              "converting binary plist to text XML"
              "plutil"
              ("-convert" "xml1" "-o" "-" "-")
              nil nil "bplist"])

;;It is necessary to perform an update!
(jka-compr-update)

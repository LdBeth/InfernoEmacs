(require 'wl)
(require 'wl-batch)
(wl 1)
(when (functionp elmo-split-mail-function)
  (funcall elmo-split-mail-function))
(let (wl-demo elmo-folder-update-confirm wl-interactive-exit)
  (wl-exit))


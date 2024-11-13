;; Gen syntax hl -*- lexical-binding: t -*-
(require 'htmlize)
(defun preserve-only-pre ()
  (let ((s (buffer-substring (plist-get htmlize-buffer-places 'content-start)
                             (plist-get htmlize-buffer-places 'content-end))))
    (erase-buffer)
    (insert s)))
(add-hook 'htmlize-after-hook #'preserve-only-pre)

(defun htmlize-gen-ml1 (file &optional target)
  "Load FILE, fontify it, convert it to HTML, and save the result.

Contents of FILE are inserted into a temporary buffer, whose major mode
is set with `normal-mode' as appropriate for the file type.  The buffer
is subsequently fontified with `font-lock' and converted to HTML.  Note
that, unlike `htmlize-buffer', this function explicitly turns on
font-lock.  If a form of highlighting other than font-lock is desired,
please use `htmlize-buffer' directly on buffers so highlighted.

Buffers currently visiting FILE are unaffected by this function.  The
function does not change current buffer or move the point.

If TARGET is specified and names a directory, the resulting file will be
saved there instead of to FILE's directory.  If TARGET is specified and
does not name a directory, it will be used as output file name."
  (interactive (list (read-file-name
                      "HTML-ize file: "
                      nil nil nil (and (buffer-file-name)
                                       (file-name-nondirectory
                                        (buffer-file-name))))))
  (let ((output-file (if (and target (not (file-directory-p target)))
                         target
                       (expand-file-name
                        (htmlize-make-file-name (file-name-nondirectory file))
                        (or target (file-name-directory file)))))
        ;; Try to prevent `find-file-noselect' from triggering
        ;; font-lock because we'll fontify explicitly below.
        (font-lock-mode nil)
        (font-lock-auto-fontify nil)
        (global-font-lock-mode nil))
    (with-temp-buffer
      ;; Insert FILE into the temporary buffer.
      (insert-file-contents file)
      ;; Set the file name so normal-mode and htmlize-buffer-1 pick it
      ;; up.  Restore it afterwards so with-temp-buffer's kill-buffer
      ;; doesn't complain about killing a modified buffer.
      (let ((buffer-file-name file))
        ;; Set the major mode for the sake of font-lock.
        (normal-mode)
        ;; htmlize the buffer and save the HTML.
        (with-current-buffer (htmlize-buffer-1)
          (unwind-protect
              (progn
                (run-hooks 'htmlize-file-hook)
                (write-region (concat "@NEWCODE \"\""
                                      (buffer-substring (point-min) (point-max))
                                      ";;")
                              nil
                              output-file))
            (kill-buffer (current-buffer)))))))
  ;; I haven't decided on a useful return value yet, so just return
  ;; nil.
  nil)

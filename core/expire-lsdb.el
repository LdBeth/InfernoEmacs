(require 'lsdb)

(lsdb-maybe-load-hash-tables)

(defun decode-date (string)
  (let ((time (iso8601-parse string)))
    (setf (decoded-time-second time) 0
          (decoded-time-minute time) 0
          (decoded-time-hour time) 0)
    (time-to-days (encode-time time))))

(let ((database (split-string
                 (with-temp-buffer
                   (call-process-region
                    "" 0 "notmuch" nil t nil "address"
                    "--output=address"
                    "--output=recipients" "--output=sender"
                    "--deduplicate=address"
                    "*")
                   (buffer-string))))
      pending
      (current-day (time-to-days (current-time))))
  (maphash
   (lambda (key value)
     (let ((net (cdr (assq 'net value)))
           (date (cdr (assq 'last-modified value)))
           found)
       (unless (listp net)
         (setq net (list net)))
       (while (and (not found) net)
         (if (member (pop net) database)
             (setq found t)))
       (unless (or found (< (- current-day (decode-date date)) 20))
         (push (cons key value) pending))))
   lsdb-hash-table)
  pending
  (mapc #'lsdb-delete-record pending))

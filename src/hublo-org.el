;;; -*- lexical-binding: t;

(defun hb/load-org-file (meta)
  "Parse org-mode metadata and update metadata"
  (-each (-partition 2 (org-export--get-inbuffer-options))
    (lambda (x)
      (ht-set! meta (car x) (car (car (cdr x))))
      (when (equal (car x) :date)
        (let ((date (parse-time-string (car (car (cdr x))))))
          (ht-set! meta :year (format "%04d" (nth 5 date)))
          (ht-set! meta :month (format "%02d" (nth 4 date)))
          (ht-set! meta :day (format "%02d" (nth 3 date))))))))

(defun hb/org-publish (content)
  ""
  (with-temp-buffer
    (insert-string content)
    (goto-char (point-min))
    (let ((old-val org-export-show-temporary-export-buffer)
          (bf (current-buffer)))
      (org-html-export-as-html nil nil nil t nil)
      (setq org-export-show-temporary-export-buffer old-val)
      (switch-to-buffer "*Org HTML Export*")
      (let ((out (buffer-string)))
        (kill-buffer (current-buffer))
        (switch-to-buffer bf)
        out))))

(defun hb/org-metadata (item)
  (save-excursion
    (with-temp-buffer
      (insert-string (hb/item-payload item))
      (goto-char (point-min))
      (hb/load-org-file (hb/item-meta item)))))

(defun hb/org-content (item)
  (let* ((content (hb/file->string (hb/item-path item)))
         (payload (hb/org-publish content)))
    (setf (hb/item-payload item) payload)
    (ht-set (hb/item-meta item) :content payload)))

(hb/register-transform :org
                       :metadata 'hb/org-metadata
                       :content  'hb/org-content)

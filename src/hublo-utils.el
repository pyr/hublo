;;; -*- lexical-binding: t;

(defun hb/get-in (h keys)
  (let* ((ks (if (vectorp keys) (mapcar 'identity keys) keys))
         (k  (car ks)))
    (if k
      (let ((new-h (ht-get h k)))
        (when new-h
          (hb/get-in new-h (cdr ks))))
      h)))

(defun hb/walk-dir (dir)
  (-mapcat (lambda (e)
             (let ((path (concat dir "/" e)))
               (when (not (string-match "^\\." e))
                 (if (file-directory-p path)
                     (hb/walk-dir path)
                   (list path)))))
           (directory-files dir)))

(defun hb/file->string (path)
  (with-temp-buffer
    (insert-file-contents path)
    (buffer-string)))

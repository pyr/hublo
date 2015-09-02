;;; -*- lexical-binding: t;

(defvar *hb/handlers* nil)
(defvar *hb/transforms* (ht-create))

(defun hb/register-transform (name &rest phases)
  (ht-set *hb/transforms* name (-partition 2 phases)))

(defun hb/find-transform (name)
  (ht-get *hb/transforms* name))

(defun hb/register-handler (pattern transforms)
  (push (make-hb/handler
         :pattern pattern
         :transforms (-mapcat 'hb/find-transform transforms))
        *hb/handlers*))

(defun hb/find-handler-transforms (path)

  (-when-let (h (-first (lambda (h)
                          (string-match (hb/handler-pattern h)
                                        (file-name-nondirectory path)))
                        *hb/handlers*))
    (hb/handler-transforms h)))

;;
;; =============================

(defun hb/write-content (route content)
  (let ((path (format "%s/%s" (hb/config-output-dir *hb/config*) route)))
    (make-directory (file-name-directory path) t)
    (save-excursion
      (let ((cb (current-buffer)))
        (with-temp-buffer
          (insert-string content)
          (write-file path nil))))))

(defun hb/base-route (item)
  (let ((route (replace-regexp-in-string
                (format "^%s" (hb/config-source-dir *hb/config*))
                ""
                (hb/item-path item))))
    (setf (hb/item-route item) route)
    (ht-set! (hb/item-meta item) :route route)))

(defun hb/base-clean (item)
  (setq *hb/handlers* nil)
  (setq *hb/transforms* (ht-create))
  (setq *hb/pages* nil)
  (setq *hb/config* (hb/default-config)))

(defun hb/base-bootstrap (item)
  (setf (hb/item-payload item) (hb/file->string
                                (hb/item-path item))))

(defun hb/base-publish (item)
  (hb/write-content (hb/item-route item) (hb/item-payload item)))

(defun hb/metadata-transform (k v)
  (list :metadata (lambda (item) (ht-set (hb/item-meta item) k v))))

(defun hb/slug-transform ()
  (list
   :metadata
   (lambda (item)
     (let ((title (ht-get (hb/item-meta item) :title)))
       (when title
         (ht-set (hb/item-meta item) :slug
                 (replace-regexp-in-string
                  "[^a-z0-9-]" ""
                  (mapconcat
                   'identity
                   (remove-if-not
                    'identity
                    (subseq (split-string (downcase title) " ") 0 6))
                   "-"))))))))

(defun hb/group-transform (groupk)
  (list
   :metadata
   (lambda (item)
     (let* ((groups (ht-get (hb/config-context *hb/config*) :groups))
            (group  (ht-get groups groupk))
            (meta   (hb/item-meta item)))
       (ht-set groups groupk (if group (push meta group) (list meta)))))))

(hb/register-transform :noop)

(setq hb/base-transforms '((:bootstrap hb/base-bootstrap)
                           (:route     hb/base-route)
                           (:publish   hb/base-publish)
                           (:clean     hb/base-clean)))

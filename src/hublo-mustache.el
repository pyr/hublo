;;; -*- lexical-binding: t;

(defun hb/find-mustache-layout (layout)
  (let ((path (format "%s/layouts/%s.mustache"
                       (hb/config-source-dir *hb/config*)
                       layout)))
    (hb/file->string path)))

(defun hb/mustache-layout-transform (layout)
  (let ((tpl (hb/find-mustache-layout layout)))
    (list
     :content
     (lambda (item)
       (ht-set (hb/item-meta item) :payload (hb/item-payload item))
       (setf (hb/item-payload item)
             (let ((mustache-key-type 'keyword))
               (mustache-render tpl (hb/item-meta item))))
       (ht-remove (hb/item-meta item) :payload)))))

(defun hb/mustache-route-transform (route)
  (list
   :route
   (lambda (item)
     (let* ((mustache-key-type 'keyword)
            (route (mustache-render route (hb/item-meta item))))
       (setf (hb/item-route item) route)
       (ht-set! (hb/item-meta item) :route route)))))

(defun hb/mustache-transform (item)
  (let ((payload (substring-no-properties (hb/item-payload item))))
    (setf (hb/item-payload item)
          (let ((mustache-key-type 'keyword))
            (mustache-render payload
                             (hb/item-meta item))))))

(hb/register-transform :mustache
                       :content 'hb/mustache-transform)

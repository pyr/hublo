;;; -*- lexical-binding: t;

(defun hb/item-from (path)
  (make-hb/item
   :path       path
   :payload    ""
   :route      ""
   :transforms (hb/find-handler-transforms path)
   :meta       (ht-create)))

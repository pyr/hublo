;;; -*- lexical-binding: t;

(defun hb/item-from (path)
  (-when-let (transforms (hb/find-handler-transforms path))
    (make-hb/item
     :path       path
     :payload    ""
     :route      ""
     :transforms transforms
     :meta       (ht-create))))

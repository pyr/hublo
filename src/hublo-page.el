;;; -*- lexical-binding: t;

(defvar *hb/pages* nil)

(defun hb/register-page (pattern transforms)
  (push (make-hb/page :pattern pattern :transforms transforms) *hb/pages*))

;;; -*- lexical-binding: t;

(defun hb/dsl-source-dir (x)
  (setf (hb/config-source-dir *hb/config*) x))

(defun hb/dsl-output-dir (x)
  (setf (hb/config-output-dir *hb/config*) x))

(defun hb/dsl-context (k v)
  (ht-set! (hb/config-context *hb/config*) k v))

(defun hb/dsl-handle! (pattern &rest transforms)
  (hb/register-handler pattern transforms))

(defun hb/dsl-page (pattern &rest transforms)
  (hb/register-page pattern transforms))

(defun hb/dsl-title (title)
  (hb/metadata-transform :title title))

(defmacro hb/build-config (&rest body)
  "Execute body in the context of the configuration DSL."
  `(cl-letf (((symbol-function 'source-dir) #'hb/dsl-source-dir)
             ((symbol-function 'output-dir) #'hb/dsl-output-dir)
             ((symbol-function 'context)    #'hb/dsl-context)
             ((symbol-function 'handle!)    #'hb/dsl-handle!)
             ((symbol-function 'page)       #'hb/dsl-page)
             ((symbol-function 'layout)     #'hb/mustache-layout-transform)
             ((symbol-function 'route)      #'hb/mustache-route-transform)
             ((symbol-function 'title)      #'hb/dsl-title)
             ((symbol-function 'slugify)    #'hb/slug-transform)
             ((symbol-function 'meta)       #'hb/metadata-transform)
             ((symbol-function 'group-with) #'hb/group-transform))
     (progn ,@body)))

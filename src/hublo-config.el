;;; -*- lexical-binding: t;

(defun hb/default-config ()
  (make-hb/config
   :source-dir  (or (getenv "HUBLO_SOURCE_DIR") "sources")
   :output-dir  (or (getenv "HUBLO_OUTPUT_DIR") "output")
   :context     (ht (:groups (ht-create)))))

(defvar *hb/config*
  (hb/default-config))

(defun hb/load-config (config-file)
  "Load a configuration file in teh context of the configuration DSL."
  (hb/build-config
   (load-file config-file)))

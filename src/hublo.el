;;; -*- lexical-binding: t;

;; Our full dependency list
;; ========================
(require 'cl-lib)
(require 'ht)
(require 'dash)
(require 'mustache)
(require 'org)
(require 'ox-html)

;; Library functions
;; =================
(load "hublo-utils.el")
(load "hublo-types.el")
(load "hublo-config.el")
(load "hublo-transform.el")
(load "hublo-page.el")
(load "hublo-item.el")
(load "hublo-dsl.el")

;; Transform plugins
;; =================
(load "hublo-mustache.el")
(load "hublo-org.el")

(defun hb/item-match-p (page item)
  (let ((subpath (replace-regexp-in-string
                  (format "^%s" (hb/config-source-dir *hb/config*))
                  ""
                  (hb/item-path item))))
    (string-match-p (hb/page-pattern page) subpath)))

(defun hb/run-phases (&rest phases)

  (let ((candidates (->> (hb/walk-dir (hb/config-source-dir *hb/config*))
                         (-map 'hb/item-from)
                         (-remove (lambda (x) (equal x nil)))))
        (items      nil))

    (dolist (page *hb/pages*)
      (dolist (item (-filter (-partial 'hb/item-match-p page) candidates))
        (push (cons item (-concat hb/base-transforms
                                  (hb/item-transforms item)
                                  (hb/page-transforms page)))
              items)))

    (dolist (phase phases)
      (message "running through phase: %s" phase)
      (dolist (tuple items)
        (let ((item       (car tuple))
              (transforms (->> (cdr tuple)
                               (-filter (lambda (x) (equal (car x) phase)))
                               (-map 'cadr))))
          ;; always merge global and local context
          ;; this makes it easier for templates
          (ht-update! (hb/item-meta item) (hb/config-context *hb/config*))
          (dolist (transform transforms)
            (funcall transform item))))))
  (message "ran through all phases correctly"))

;; Now pull-in our CLI helpers
;; ===========================
(load "hublo-cli.el")

(provide 'hublo)

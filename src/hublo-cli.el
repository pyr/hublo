(setq hb/default-config-file "hublo.config")

(defun hb/noop ()
  "Shortcut for a no-op run"
  (message "running hublo dry run")
  (hb/run-phases :bootstrap :metadata :route :content :clean))

(defun hb/publish! ()
  "Shortcut for publishing"
  (message "running hublo publish")
  (hb/run-phases :bootstrap :metadata :route :content :publish :clean))

(defun hb/clean! ()
  (message "running hublo clean")
  (when (file-directory-p (hb/config-output-dir *hb/config*))
    (delete-directory (hb/config-output-dir *hb/config*) t nil)))

(defun hb/consume-args! (args)
  (let (seen flags command)
    (setq seen nil)
    (dolist (arg args)
      (cond
       (seen                      (progn (push (list seen arg) flags)
                                         (setq seen nil)))
       ((string-match-p "^-" arg) (setq seen arg))
       (t                         (push arg command))))
    (cons (reverse command) (reverse flags))))

(defun hb/usage ()
  (message "Usage: hublo [-f config-file] {noop|publish|clean|phases} <args>"))

(defun hb/run-script-from (args)
  (let* (config-file
         (command-and-flags  (hb/consume-args! args))
         (command            (caar command-and-flags))
         (command-args       (cdar command-and-flags))
         (flags              (cdr command-and-flags)))
    (-when-let (env-file (getenv "HUBLO_CONFIG"))
        (setq config-file env-file))
    (dolist (flag flags)
      (let ((flag-name (car  flag))
            (value     (cadr flag)))
        (cond
         ((equal flag-name "-f") (setq config-file value))
         (t                      (error "unknown flag: %s" flag-name))))
      nil)

    (when (not config-file)
      (message "no config file provided, using: hublo.config")
      (setq config-file hb/default-config-file))

    (message "using config: %s" config-file)

    (hb/load-config config-file)
    (setq config-file nil)

    (cond
     ((equal "clean" command)    (hb/clean!))
     ((equal "publish" command)  (hb/publish!))
     ((equal "noop"   command)   (hb/noop))
     ((equal "phases"  command)  (apply 'hb/run-phases command-args))
     (t                          (hb/usage))))
  nil)

(defun hb/run-script ()
  (let ((args command-line-args-left))
    (setq command-line-args-left '())
    (setq command-line-processed t)
    (hb/run-script-from args))

  (message "hublo script ran successfuly."))

;;; config-packages.el

(setq load-prefer-newer t
	  package--init-file-ensured t
	  package-enable-at-startup nil
	  package-user-dir (concat %package-dir emacs-version "/")
      package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")
        ("org" . "https://orgmode.org/elpa/")))

(require 'package)

(package-initialize)
(unless (file-directory-p package-user-dir)
  (make-directory package-user-dir t)
  (package-refresh-contents))

(defun +package-install-ensure-refreshed (&rest _)
  (package-refresh-contents)
  (advice-remove #'package-install #'+package-install-ensure-refreshed))
(advice-add #'package-install :before #'+package-install-ensure-refreshed)

;;
;; Use-Package

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package)
  (unless (package-installed-p 'use-package)
	(error "Unable to install `use-package'")))

(eval-when-compile (require 'use-package))
(require 'bind-key)

(setq use-package-always-defer t
	  use-package-always-ensure t
	  use-package-verbose %debug-mode
	  use-package-minimum-reported-time (if %debug-mode 0 0.1))

;;
;; :after-call
;;
;; This is an extension to `use-package' from Doom; it will automatically load a
;; package after the first invocation of a set of functions.

(defvar use-package--deferred-packages-alist '(t))
(add-to-list 'use-package-deferring-keywords :after-call nil #'eq)

(setq use-package-keywords
	  (use-package-list-insert :after-call use-package-keywords :after))

(defalias 'use-package-normalize/:after-call
  'use-package-normalize-symlist)

(defun use-package-handler/:after-call (name _keyword hooks rest state)
  (if (plist-get state :demand)
	  (use-package-process-keywords name rest state)
	(let ((fn (intern (format "use-package--transient-hook--load-%s" name))))
	  (use-package-concat
	   `((fset ',fn
		   (lambda (&rest _)
		 (message "Loading deferred package %s from %s" ',name ',fn)
		 (condition-case e (require ',name)
		   ((debug error)
			(message "Failed to load deferred package %s: %s" ',name e)))
		 (dolist (hook (cdr (assq ',name use-package--deferred-packages-alist)))
		   (if (functionp hook)
			   (advice-remove hook #',fn)
			 (remove-hook hook #',fn)))
		 (setq use-package--deferred-packages-alist
               (delq (assq ',name use-package--deferred-packages-alist)
			         use-package--deferred-packages-alist))
		 (fmakunbound ',fn))))
	   (let (forms)
	     (dolist (hook hooks forms)
	       (push (if (functionp hook)
			         `(advice-add #',hook :before #',fn)
		           `(add-hook #',hook #',fn))
		         forms)))
	   `((unless (assq ',name use-package--deferred-packages-alist)
	       (push '(,name) use-package--deferred-packages-alist))
	     (nconc (assq ',name use-package--deferred-packages-alist)
		        '(,@hooks)))
	   (use-package-process-keywords name rest state)))))

;;
;; :defer-incrementally
;;
;; This is another extension to `use-package' from Doom; it will incrementally
;; load defined packages during the idle timer. This is useful for large
;; packages such as `org' as it reduces noticable pauses during startup.

(setq use-package-keywords
      (use-package-list-insert :defer-incrementally
                               use-package-keywords
                               :after))

(defalias 'use-package-normalize/:defer-incrementally 'use-package-normalize-symlist)

(defun use-package-handler/:defer-incrementally (name _keyword targets rest state)
  (use-package-concat
   `((use-package--load-packages-incrementally
      ',(if (equal targets '(t))
            (list name)
          (append targets (list name)))))
   (use-package-process-keywords name rest state)))

(defvar use-package--incremental-packages-list '(t)
  "A list of packages to load incrementally after startup.")

(defvar use-package-incremental-first-idle-timer 2
  "How long (in idle seconds) until incremental loading starts.

Set this to nil to disable incremental loading.")

(defvar use-package-incremental-idle-timer 1.5
  "How long (in idle seconds) in between incrementally loading packages.")

(defun use-package--load-packages-incrementally (packages &optional now)
  "Registers packages to be loaded incrementally.

If NOW is non-nil, load PACKAGES incrementally, in
`use-package-incremental-idle-timer' intervals."
  (if (not now)
      (nconc use-package--incremental-packages-list packages)
    (when packages
      (let ((gc-cons-threshold (* 256 1024 1024))
            file-name-handler-alist)
        (let* ((reqs (cl-delete-if #'featurep packages))
               (req (ignore-errors (pop reqs))))
          (when req
            (when use-package-debug
              (message "Incrementally loading %s" req))
            (condition-case e
                (require req nil t)
              ((error debug)
               (message "Failed to load '%s' package incrementally:"
                        req e)))
            (if reqs
                (run-with-idle-timer use-package-incremental-idle-timer nil
                                     #'use-package--load-packages-incrementally
                                     reqs t)
              (message "Finished incremental loading"))))))))

(defun use-package-load-packages-incrementally ()
  "Begin incrementally loading packages in `use-package--incremental-packages'.

If this is a daemon session, load them all immediately."
  (if (daemonp)
      (mapc #'require (cdr use-package--incremental-packages))
    (when (integerp use-package-incremental-first-idle-timer)
      (run-with-idle-timer use-package-incremental-first-idle-timer
                           nil #'use-package--load-packages-incrementally
                           (cdr use-package--incremental-packages-list) t))))
(add-hook 'emacs-startup-hook #'use-package-load-packages-incrementally)

(provide 'config-packages)
;;; config-packages.el ends here

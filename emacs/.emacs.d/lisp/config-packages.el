;;; config-packages.el

;;
;; Straight.el
(defvar bootstrap-version 5)
(let ((bootstrap-file
       (expand-file-name
	"straight/repos/straight.el/bootstrap.el"
	user-emacs-directory)))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
	(url-retrieve-synchronously
	 "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
	 'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;;
;; Use-Package

(straight-use-package 'use-package)
(eval-when-compile (require 'use-package))
(require 'bind-key)

(setq use-package-always-defer t
      use-package-verbose t
      straight-use-package-by-default t)

(defmacro use-feature (name &rest args)
  "Like `use-package' but with `straight-use-package-by-default' disabled.

NAME and ARGS are as in `use-package'."
  (declare (indent defun))
  `(use-package ,name
     :straight nil
     ,@args))

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
		     (condition-case e
			 (let ((default-directory user-emacs-directory))
			   (require ',name))
		       ((debug error)
			(message "Failed to load deferred package %s: %s" ',name e)))
		     (when-let (deferral-list (assq ',name use-package--deferred-packages-alist))
		       (dolist (hook (cdr deferral-list))
			 (advice-remove hook #'fn)
			 (remove-hook hook #'fn))
		       (delq! deferral-list use-package--deferred-packages-alist)
		       (unintern ',fn nil)))))
	     (let (forms)
	       (dolist (hook hooks forms)
		 (push (if (string-match-p "-\\(?:functions\\|hook\\)$" (symbol-name hook))
			   `(add-hook ',hook #',fn)
			 `(advice-add #',hook :before #',fn))
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
            (when use-package-verbose
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

;;
;; `no-littering' overrides many common paths to keep .emacs.d clean
;;
;; Load it here so that it gets set early on in the init process.
(use-package no-littering
  :demand t
  :init
  (setq no-littering-etc-directory %etc-dir
	no-littering-var-directory %var-dir)
  :config
  (setq auto-save-file-name-transforms
	`((".*" ,(expand-file-name "auto-save" %var-dir) t)))
  (eval-when-compile
    (require 'recentf))
  (after! 'recentf
    (push no-littering-etc-directory recentf-exclude)
    (push no-littering-var-directory recentf-exclude)))

(provide 'config-packages)
;;; config-packages.el ends here

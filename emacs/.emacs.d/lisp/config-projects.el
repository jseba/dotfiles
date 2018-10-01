;;; config-projects.el

(use-package projectile
  :commands (projectile-mode projectile-project-root)
  :init
  (setq	projectile-enable-caching t
	    projectile-cache-file (concat %var-dir "projectile.cache")
	    projectile-indexing-method 'alien
	    projectile-find-dir-includes-top-level t)

  (defmacro +projectile-without-cache (&rest body)
    `(let ((projectile-project-root-cache (make-hash-table :test 'equal))
           projectile-project-name
           projectile-require-project-root)
       ,@body))

  (defun +projectile-project-root (&optional nocache)
    (if nocache
        (+projectile-without-cache
         (+projectile-project-root nil))
      (let (projectile-require-project-root)
        (or (projectile-project-root)
            default-directory))))

  (defun +projectile-project-name (&optional nocache)
    (if nocache
        (+projectile-without-cache
         (+projectile-project-name nil))
      (projectile-project-name)))

  (defun +projectile-project-p (&optional nocache)
    (if nocache
        (+projectile-without-cache
         (+projectile-project-p))
      (let ((projectile-require-project-root t))
        (and (projectile-project-p) t))))

  (defun +projectile-reload-project ()
    (interactive)
    (projectile-invalidate-cache nil)
    (projectile-reset-cached-project-root)
    (dolist (fn projectile-project-root-files-functions)
      (remhash (format "%s-%s" fn default-directory)
               projectile-project-root-cache)))

  (add-hook 'init-hook #'projectile-mode)

  :config
  (run-with-idle-timer 10 nil #'projectile-cleanup-known-projects))

(provide 'config-projects)
;;; config-projects.el ends here

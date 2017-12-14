;;; core/init-project.el -*- lexical-binding: t; -*-

(defvar nx-project-hook nil
  "A list of hooks run when a project is enabled.")

(use-package projectile
  :ensure t
  :demand t
  :config
  (setq projectile-cache-file (concat nx-cache-dir "projectile.cache")
	projectile-enable-caching t
	projectile-indexing-method 'alien
	projectile-known-projects-file (concat nx-cache-dir "projectile.projects")
	projectile-require-project-root nil
	projectile-globally-ignored-file-suffixes '(".elc" ".pyc" ".o"))
  (add-hook 'nx-init-hook #'projectile-mode)

  (push ".project" projectile-project-root-files-bottom-up)

  (defun nx*projectile-locate-dominating-file (orig-fn &rest args)
    "Don't traverse the file system if on a remote connection."
    (unless (file-remote-p default-directory)
      (apply orig-fn args)))
  (advice-add #'projectile-locate-dominate-file :around #'nx*projectile-locate-dominating-file)

  (defun nx*projectile-cache-current-file (orig-fn &rest args)
    "Don't cache ignored files."
    (unless (cl-loop for path in (projectile-ignored-directories)
		     if (string-prefix-p buffer-file-name (expand-file-name path))
		     return t)
      (apply orig-fn args)))
  (advice-add #'projectile-cache-current-file :around #'nx*projectile-cache-current-file))

(provide 'init-project)

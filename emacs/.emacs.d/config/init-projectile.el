(use-package projectile
  :ensure t
  :after helm
  :init
  (setq projectile-completion-system 'ivy
	projectile-enable-caching t
	projectile-cache-file "~/.emacs.d/projectile.cache"
	projectile-indexing-method 'alien
	projectile-find-dir-includes-top-level t)
  (setq-default projectile-mode-line
		'(:eval
		  (if (file-remote-p default-directory)
		      " Proj"
		    (format " Proj[%s]" (projectile-project-name)))))
  (run-with-idle-timer 10 nil #'projectile-cleanup-known-projects)
  (projectile-global-mode t)
  :diminish projectile-mode)

(provide 'init-projectile)

;;; config-projectile.el -- Projectile

;;; Commentary:
;;; Code:

(use-package projectile
  :ensure t
  :init
  (setq projectile-completion-system 'ivy
	    projectile-enable-caching t
	    projectile-cache-file (concat nx-cache-dir "projectile.cache")
	    projectile-indexing-method 'alien
	    projectile-find-dir-includes-top-level t)
  (run-with-idle-timer 10 nil #'projectile-cleanup-known-projects)
  (add-hook 'nx-post-init-hook #'projectile-mode)
  :diminish projectile-mode)

(provide 'config-projectile)
;;; config-projectile.el ends here

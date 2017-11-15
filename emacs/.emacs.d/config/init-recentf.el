(require 'init-files)

(use-package recentf
  :init
  (setq recentf-max-saved-items 200
	recentf-max-menu-items 15
	recentf-auto-cleanup 300
	recentf-exclude (list
			 "/tmp/"
			 "/ssh:"
			 "/\\.git/.*\\'"
			 "/elpa.*/.*\\"
			 #'ignoramus-boring-p))
  :config
  (recentf-mode))

(provide 'init-recentf)

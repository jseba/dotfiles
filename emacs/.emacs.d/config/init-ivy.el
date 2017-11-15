(require 'init-projectile)

(use-package flx
  :ensure t)

(use-package ivy
  :ensure t
  :diminish ivy-mode
  :init
  (defun sanityinc/enable-ivy-flx-matching ()
    "Make `ivy' matching work more like IDO."
    (interactive)
    (require 'flx)
    (setq-default ivy-re-builders-alist '((t . ivy--regex-fuzzy))))

  (setq-default ivy-use-virtual-buffers t
		ivy-virtual-abbreviate 'fullpath
		ivy-count-format ""
		projectile-completion-system 'ivy
		ivy-inital-inputs-alist '((man . "^")
					  (woman . "^")))
  (ivy-mode)
  :bind
  (:map ivy-minibuffer-map
	("<return>" . ivy-alt-done)
	("C-j" . ivy-immediate-done)
	("C-<return>" . ivy-immediate-done)))

(use-package counsel
  :ensure t
  :diminish counsel-mode
  :init
  (defun sanityinc/counsel-search-project (initial-input &optional use-current-dir)
    (interactive (list (thing-at-point 'symbol)
		       current-prefix-arg))
    (let ((current-prefix-arg)
	  (dir (if use-current-dir
		   default-directory
		 (condition-case err
		     (projectile-project-root)
		   (error default-directory)))))
      (counsel-ag initial-input dir)))
  (setq-default counsel-mode-override-describe-bindings t)
  (counsel-mode)
  :bind
  (("M-?" . sanityinc/counsel-search-project)))

(use-package swiper
  :ensure t
  :init
  (defun sanityinc/swiper-at-point (sym)
    "Use `swiper' to search for the symbol at point."
    (interactive (list (thing-at-point 'symbol)))
    (swiper sym))
  :bind
  (:map ivy-mode-map
	("M-s /" . sanityinc/swiper-at-point)))

(use-package ivy-historian
  :ensure t
  :after ivy
  :init
  (ivy-historian-mode t))

(provide 'init-ivy)

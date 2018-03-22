;;; ivy.el -- Ivy

;;; Commentary:
;;; Code:

(use-package ivy
  :ensure t
  :diminish ivy-mode
  :init
  (defun nx/enable-ivy-flx-matching ()
    "Make `ivy' matching work more like IDO."
    (interactive)
    (require 'flx)
    (setq-default ivy-re-builders-alist '((t . ivy--regex-fuzzy))))
  (setq ivy-use-virtual-buffers t
        ivy-virtual-abbreviate 'fullpath
        ivy-count-format ""
        ivy-height 12
        ivy-wrap t
        ivy-do-completion-in-region t
        ivy-fixed-height-minibuffer t
        ivy-initial-inputs-alist nil
        ivy-format-function #'ivy-format-function-line)
  (add-hook 'nx-post-init-hook #'ivy-mode))

(use-package counsel
  :ensure t
  :diminish counsel-mode
  :init
  (defun nx/counsel-search-project (initial-input &optional use-current-dir)
    (interactive (list (thing-at-point 'symbol)
                       current-prefix-arg))
    (let ((current-prefix-arg)
          (dir (if use-current-dir
                   default-directory
                 (condition-case _
                     (projectile-project-root)
                   (error default-directory)))))
      (counsel-ag initial-input dir)))
  (setq-default counsel-mode-override-describe-bindings t)
  (counsel-mode))

(use-package counsel-projectile
  :ensure t
  :after (counsel projectile))

(use-package swiper
  :ensure t
  :commands (swiper swiper-all)
  :init
  (defun nx/swiper-at-point (sym)
    "Use `swiper' to search for the symbol at point."
    (interactive (list (thing-at-point 'symbol)))
    (swiper sym)))

(use-package ivy-historian
  :ensure t
  :after ivy
  :init
  (ivy-historian-mode t))

(use-package smex
  :ensure t
  :after ivy
  :commands (smex smex-major-mode-commands)
  :init
  (setq-default smex-save-file (concat nx-cache-dir "smex-items"))
  (setq smex-completion-method 'ivy)
  (smex-initialize))

(provide 'config-ivy)
;;; config-ivy.el ends here

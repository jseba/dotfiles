;;; lisp/init-ivy.el -*- lexical-binding: t; -*-

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

  (setq ivy-use-virtual-buffers t
        ivy-virtual-abbreviate 'fullpath
        ivy-count-format ""
        ivy-height 12
        ivy-wrap t
        ivy-do-completion-in-region t
        ivy-fixed-height-minibuffer t
        ivy-initial-inputs-alist nil
        ivy-format-function #'ivy-format-function-line)
        ;; projectile-completion-system 'ivy

  (add-hook 'nx-post-init-hook #'ivy-mode)
  :bind
  (:map ivy-mode-map
    ([remap switch-to-buffer] . ivy-switch-buffer)
    ([remap imenu-anywhere]   . ivy-imenu-everywhere)
   :map ivy-minibuffer-map
	("<return>" . ivy-alt-done)
	("C-j" . ivy-immediate-done)
	("C-<return>" . ivy-immediate-done)))

(use-package ivy-hydra
  :ensure t
  :after (ivy hydra))

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
  (:map ivy-mode-map
    ([remap apropos] . counsel-apropos)
    ([remap describe-face] . counsel-describe-face)
    ([remap find-file] . counsel-find-file)
    ([remap recentf-open-files] . counsel-recentf)
    ([remap imenu] . counsel-imenu)
    ([remap bookmark-jump] . counsel-bookmark)
    ([remap execute-extended-command] . counsel-M-x)
    ([remap describe-function] . counsel-describe-function)
    ([remap describe-variable] . counsel-describe-variable)))

(use-package counsel-projectile
  :ensure t
  :after counsel)

(use-package swiper
  :ensure t
  :commands (swiper swiper-all)
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

(use-package smex
  :ensure t
  :after ivy
  :commands (smex smex-major-mode-commands)
  :init
  (setq-default smex-save-file (concat nx-cache-dir "smex-items"))
  (setq smex-completion-method 'ivy)
  (smex-initialize)
  :bind
  (([remap execute-extended-command] . smex)))

(provide 'init-ivy)

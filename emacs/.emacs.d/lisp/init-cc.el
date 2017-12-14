(use-package cc-mode
  ;; built-in
  :after (company projectile)
  :init
  (defun my-c++-mode-hook ()
    (electric-indent-mode)
    (diminish 'electric-mode)
    (c-set-style "stroustrup")
    (c-set-offset 'innamespace '0))

  (setq c-default-style "stroustrup"
	c-basic-offset 4
	c-toggle-hungry-state 1)
  (add-to-list 'auto-mode-alist '("\\.n\\'" . c++-mode))
  (add-hook 'c-mode-common-hook #'hs-minor-mode)
  (add-hook 'c++-mode-hook #'my-c++-mode-hook)
  ;;(c-auto-toggle-newline 1)
  :bind
  (:map c-mode-map
	("C-c m a" . projectile-find-other-file)
	("C-c m A" . projectile-find-other-file-other-window)
   :map c++-mode-map
	("C-c m a" . projectile-find-other-file)
	("C-c m A" . projectile-find-other-file-other-window)))

(use-package company-c-headers
  :ensure t
  :init
  (add-hook 'c-mode-common-hook (lambda ()
				  (sanityinc/local-push-company-backend 'company-c-headers))))

(use-package modern-cpp-font-lock
  :ensure t
  :init
  (add-hook 'c++-mode-hook #'modern-c++-font-lock-mode)
  :diminish
  modern-c++-font-lock-mode)


(provide 'init-cc)

(require 'init-company)

(use-package python
  ;; built-in
  :init
  (let ((ipython (executable-find "ipython")))
    (if ipython
	(setq python-shell-interpreter ipython)
      (warn "IPython is missing, falling back to default python")))

  (add-hook 'python-mode-hook #'subword-mode)
  (add-hook 'python-mode-hook (lambda () (setq fill-column 79))))

(use-package anaconda
  :ensure t
  :after python
  :init
  (add-hook 'python-mode-hook #'anaconda-mode)
  (add-hook 'python-mode-hook #'anaconda-eldoc-mode))

(use-package company-anaconda
  :ensure t
  :after (company anaconda)
  :init
  (add-hook 'pythond-mode-hook (lambda ()
				 (sanityinc/local-push-company-backend 'company-anaconda))))

(use-package pip-requirements
  :ensure t
  :after python)

(provide 'init-python)

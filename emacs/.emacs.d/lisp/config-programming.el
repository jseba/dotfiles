;;; config-programming.el -- Base programming configuration

;;; Commentary:
;;; Code:

(use-package prog-mode
  :init
  (setq font-lock-maximum-decoration 2))

(use-package which-func
  :commands which-function-mode
  :init
  (setq mode-line-misc-info (assq-delete-all 'which-func-mode mode-line-misc-info)
	which-func-unknown "n/a")
  (add-hook 'prog-mode-hook #'which-function-mode))

(use-package rainbow-delimiters
  :ensure t
  :commands rainbow-delimiters-mode
  :init
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(use-package symbol-overlay
  :ensure t
  :commands (symbol-overlay-mode symbol-overlay-jump-next symbol-overlay-jump-prev)
  :init
  (add-hook 'prog-mode-hook #'symbol-overlay-mode)
  :diminish symbol-overlay-mode)

(use-package highlight-escape-sequences
  :ensure t
  :commands hes-mode
  :init
  (add-hook 'prog-mode-hook #'hes-mode)
  (add-hook 'text-mode-hook #'hes-mode))

(use-package flycheck
  :ensure t
  :commands (flycheck-mode flycheck-pos-tip-mode)
  :init
  (add-hook #'prog-mode-hook #'flycheck-mode)
  (add-hook #'flycheck-mode #'flycheck-pos-tip-mode)
  :diminish flycheck-mode)

(use-package electric
  ;; built-in
  :init
  (add-hook #'prog-mode-hook #'electric-indent-mode)
  :diminish electric-mode)

(provide 'config-programming)
;;; config-programming.el ends here

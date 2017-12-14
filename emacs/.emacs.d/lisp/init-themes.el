(use-package ample-theme
  :ensure t)

(add-hook 'window-setup-hook 'clear-background-term)
(load-theme 'ample t)

(provide 'init-themes)



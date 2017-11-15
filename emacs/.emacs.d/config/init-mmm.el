(use-package mmm-mode
  :ensure t
  :init
  (require 'mmm-auto)
  (setq mmm-global-mode 'buffers-with-submode-classes
	mmm-submode-decoration-level 2))

(provide 'init-mmm)

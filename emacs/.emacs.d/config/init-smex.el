(use-package smex
  :ensure t
  :init
  (setq-default smex-save-file "~/.emacs.d/smex-items")
  :bind
  (([remap execute-extended-command] . smex)))

(provide 'init-smex)

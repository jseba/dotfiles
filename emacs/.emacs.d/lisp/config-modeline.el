;;; config-modeline.el

(use-package doom-modeline
  :hook (init-ui . doom-modeline-mode)
  :init
  (setq doom-modeline-buffer-file-name-style 'relative-from-project
	doom-modeline-icon (display-graphic-p)
	doom-modeline-major-mode-icon nil)
  (doom-modeline-def-modeline 'nx-mode-line
    '(bar matches buffer-info remote-host buffer-position selection-info)
    '(misc-info debug lsp buffer-encoding major-mode process checker bar))
  (add-hook doom-modeline-mode-hook
	    (lambda () (doom-modeline-set-modeline 'nx-mode-line default))))

(provide 'config-modeline)
;;; config-modeline.el ends here

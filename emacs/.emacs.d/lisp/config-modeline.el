;;; config-modeline.el

(use-package doom-modeline
  :preface
  (setq-default mode-line-format nil)
  (setq projectile-dynamic-mode-line nil)
  :init
  (setq doom-modeline-bar-width 3
        doom-modeline-github nil
        doom-modeline-mu4e nil
        doom-modeline-persp-name nil
        doom-modeline-checker-simple-format nil
        doom-modeline-minor-modes nil
        doom-modeline-major-mode-icon nil
        doom-modeline-icon nil
        doom-modeline-buffer-file-name-style 'relative-from-project)
  (add-hook 'doom-modeline-mode-hook #'size-indication-mode)
  (add-hook 'doom-modeline-mode-hook #'column-number-mode)
  (add-hook 'init-ui-hook #'doom-modeline-mode)

  :config
  (add-hook 'load-theme-hook #'doom-modeline-refresh-bars)

  (doom-modeline-def-modeline 'main
    '(bar matches buffer-info remote-host buffer-position selection-info)
    '(misc-info debug input-method buffer-encoding lsp major-mode process vcs checker))

  (doom-modeline-def-modeline 'special
    '(bar matches buffer-info-simple buffer-position selection-info)
    '(misc-info debug input-method buffer-encoding lsp major-mode process checker))

  (doom-modeline-def-modeline 'project
    '(bar buffer-default-directory)
    '(misc-info debug fancy-battery " " major-mode)))

(use-package anzu
  :after-call isearch-mode)

(use-package evil-anzu
  :after-call (evil-ex-start-search evil-ex-start-word-search))

(provide 'config-modeline)
;;; config-modeline.el ends here

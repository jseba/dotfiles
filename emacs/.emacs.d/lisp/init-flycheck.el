(use-package flycheck
  :enssure t
  :commands (flycheck-mode flycheck-list-errors flycheck-buffer)
  :init
  (setq flycheck-check-syntax-automatically '(save-mode-enabled))
  :config
  (autoload 'pkg-info-version-info "pkg-info"))

(use-package flycheck-pos-tip
  :ensure t
  :after flycheck
  :init
  (setq flycheck-pos-tip-timeout 10
        flycheck-display-errors-delay 0.5)
  (flyceck-pos-tip-mode))

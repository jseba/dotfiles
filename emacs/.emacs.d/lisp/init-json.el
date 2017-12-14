(use-package json-mode
  :ensure t
  :init
  (add-hook 'json-mode-hook
    ;; Fix JSON mode indentation
    (lambda () (setq-local js-indent-level 4))))

(use-package json-reformat
  :ensure t
  :bind
  (("C-c x j" . json-reformat-region)))

(provide 'init-json)

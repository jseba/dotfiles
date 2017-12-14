(use-package prog-mode
  ;; built-in
  :init
  (setq font-lock-maximum-decoration 2)
  :bind
  (("C-c t p" . prettify-symbols-mode)))

(use-package which-func
  ;; built-in
  :init
  (setq-default header-line-format '((which-func-mode ("" which-func-format " "))))
  (setq mode-line-misc-info (assq-delete-all 'which-func-mode mode-line-misc-info)
	which-func-unknown "n/a")
  (which-function-mode))

(use-package rainbow-delimiters
  :ensure t
  :init
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(use-package symbol-overlay
  :ensure t
  :init
  (dolist (hook '(prog-mode-hook))
    (add-hook hook 'symbol-overlay-mode))
  :bind
  (:map symbol-overlay-mode-map
	("M-n" . symbol-overlay-jump-next)
	("M-p" . symbol-overlay-jump-prev))
  :diminish symbol-overlay-mode)

(use-package highlight-escape-sequences
  :ensure t
  :init
  (hes-mode))


(provide 'init-programming)

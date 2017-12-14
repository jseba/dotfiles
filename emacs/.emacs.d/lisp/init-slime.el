(use-package slime
  :ensure t
  :init
  (setq slime-protocol-version #'ignore
	slime-net-coding-system #'utf-8-unix
	slime-complete-symbol*-fancy t
	slime-complete-symbol-function #'slime-fuzzy-complete-symbol)
  (slime-setup '(slime-repl slime-fuzzy)))

(use-package slime-company
  :ensure t
  :after (company slime))

(provide 'init-slime)

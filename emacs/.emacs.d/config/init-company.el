'(use-package company
  :ensure t
  :diminish company-mode
  :init
  (defun sanityinc/local-push-company-backend (backend)
    "Add BACKEND to a buffer-local version of `company-backends'."
    (make-local-variable 'company-backends)
    (push backend company-backends))

  (setq-default company-backends '((company-capf company-dabbrev-code) company-dabbrev)
		company-dabbrev-other-buffers 'all
		company-tooltip-align-annotation t
		completion-cycle-threshold 2)
  (setq tab-always-indent 'complete
	company-idle-delay 0.5
	company-show-numbers t
	company-tooltip-limit 10
	company-tooltip-flip-when-above t)
  (global-company-mode)
  :bind
  (([remap dabbrev-expand] . company-complete)
   ([remap completion-at-point] . company-complete)
   :map company-mode-map
   ("M-/" . company-complete)
   :map company-active-map
   ("M-/" . company-select-next)
   ("C-n" . company-select-next)
   ("C-p" . company-select-prev)))

(use-package company-quickhelp
  :ensure t
  :after company
  :init (company-quickhelp-mode))

(use-package company-statistics
  :ensure t
  :after company
  :if window-system
  :init (company-statistics-mode))

(use-package company-math
  :ensure t
  :after company
  :init
  (add-to-list 'company-backends 'company-math-symbols-unicode))

(provide 'init-company)

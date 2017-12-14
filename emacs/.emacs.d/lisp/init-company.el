(use-package company
  :ensure t
  :commands (company-mode global-company-mode company-complete
             company-complete-common company-manual-begin
             company-grab-line)
  :init
  (defun sanityinc/local-push-company-backend (backend)
    "Add BACKEND to a buffer-local version of `company-backends'."
    (make-local-variable 'company-backends)
    (push backend company-backends))

  (setq company-backends '(company-capf company-dabbrev-code company-dabbrev company-spell)
        company-frontends '(company-pseudo-tooltip-frontend company-echo-metadata-frontend)
        company-transformers '(company-sort-by-occurrence)
        company-global-modes '(not eshell-mode comint-mode erc-mode message-mode help-mode gud-mode)
        company-dabbrev-other-buffers 'all
        company-dabbrev-downcase nil
        company-dabbrev-ignore-case nil
        company-tooltip-align-annotation t
        company-idle-delay 0.5
        company-show-numbers t
        company-tooltip-limit 10
        company-tooltip-flip-when-above t
        company-tooltip-align-annotations t
        company-require-match 'never
        completion-cycle-threshold 2
        tab-always-indent 'complete)
  (global-company-mode)
  :bind
  (([remap dabbrev-expand] . company-complete)
   ([remap completion-at-point] . company-complete)
   :map company-mode-map
   ("M-/" . company-complete)
   :map company-active-map
   ("M-/" . company-select-next)
   ("C-n" . company-select-next)
   ("C-p" . company-select-prev))
  :diminish company-mode)

(use-package company-quickhelp
  :ensure t
  :after company
  :init (company-quickhelp-mode))

(use-package company-statistics
  :ensure t
  :after company
  :if window-system
  :init
  (setq company-statistics-file (concat nx-cache-dir "company-stats-cache.el"))
  (company-statistics-mode))

(use-package company-dict
  :ensure t
  :commands company-dict)

(use-package company-math
  :ensure t
  :after company
  :init
  (add-to-list 'company-backends 'company-math-symbols-unicode))

(provide 'init-company)

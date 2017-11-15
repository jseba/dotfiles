(use-package dired
  :defer t
  :config
  (setq dired-auto-revert-buffer t
        dired-listing-switches "-alhF --group-directories-first -v"
        dired-ls-F-marks-symlinks t
        dired-recursive-copies 'always
        dired-recursive-deletes 'top    ;; Only ask for top dir when deleting
        dired-dwim-target t))           ;; Use the dired buffer in other window if it exists

;; Dired enhancements
(use-package dired-x
  ;; built-in
  :init
  (add-hook 'dired-mode-hook #'dired-omit-mode)
  :after dired
  :config
  (put 'dired-find-alternate-file 'disabled nil)  ;; Re-use current buffer when pressing 'a'
  (setq dired-dwim-target t)
  ;; this is a hack to correctly diminish `dired-omit-mode'
  (add-function :after (symbol-function 'dired-omit-startup)
                (lambda () (diminish 'dired-omit-mode))
                '((name . dired-omit-mode-diminish))))
(use-package dired-sort
  :ensure t
  :after dired)

(provide 'init-dired)

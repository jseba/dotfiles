;;; config-cc.el -- C/C++ configuration

;;; Commentary:
;;; Code:

(defun nx|c++-mode-hook ()
  "Personal C++ style."
  (c-set-style "stroustrup")
  (c-set-offset 'innamespace '0))

(defun nx|irony-mode-hook ()
  "Remap completion keys in `irony-mode'."
  (define-key irony-mode-map [remap completion-at-point] #'irony-completion-at-point-async)
  (define-key irony-mode-map [remap complete-symbol] #'irony-completion-at-point-async))

(use-package cc-mode
  ;; built-in
  :after (company projectile flycheck)
  :init
  (setq c-default-style "stroustrup"
		c-basic-offset 4
		c-toggle-hungry-state 1)
  (add-to-list 'auto-mode-alist '("\\.n\\'" . c++-mode))
  (add-hook 'c-mode-common-hook #'hs-minor-mode)
  (add-hook 'c++-mode-hook #'nx|c++-mode-hook)
  :bind
  (:map c-mode-map
   ("C-c m a" . projectile-find-other-file)
   ("C-c m A" . projectile-find-other-file-other-window)
   :map c++-mode-map
   ("C-c m a" . projectile-find-other-file)
   ("C-c m A" . projectile-find-other-file-other-window)))

(use-package company-c-headers
  :ensure t
  :init
  (add-hook 'c-mode-common-hook (lambda ()
                                  (nx--local-push-company-backend 'company-c-headers))))

(use-package irony
  :ensure t
  :commands irony-mode
  :init
  (add-hook 'c-mode-common-hook #'irony-mode)
  :config
  (irony-cdb-autosetup-compile-options))

(use-package company-irony
  :ensure t
  :after (company-irony-c-headers)
  :init
  (when (boundp 'w32-pipe-read-delay)
    (setq w32-pipe-read-delay 0))
  (when (boundp 'w32-pipe-buffer-size)
    (setq irony-server-w32-pipe-buffer-size (* 64 1024)))
  (add-hook #'irony-mode-hook #'nx|irony-mode-hook)
  (add-hook #'irony-mode-hook #'company-irony-setup-begin-commands)
  :config
  (add-to-list 'company-backends '(company-irony-c-headers company-irony))
  :diminish irony-mode)

(use-package company-irony-c-headers
  :ensure t
  :after (company irony)
  ;; see company-irony for :config
  )

(use-package flycheck-irony
  :ensure t
  :after (flycheck irony)
  :commands flycheck-irony-setup
  :init
  (add-hook #'irony-mode-hook #'flycheck-irony-setup))

(use-package modern-cpp-font-lock
  :ensure t
  :commands modern-c++-font-lock-mode
  :init
  (add-hook 'c++-mode-hook #'modern-c++-font-lock-mode)
  :diminish
  modern-c++-font-lock-mode)

(provide 'config-cc)
;;; config-cc.el ends here

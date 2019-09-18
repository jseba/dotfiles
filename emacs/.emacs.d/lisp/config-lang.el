;;; config-lang.el

(use-feature electric
  :init
  (defvar-local +electric-indent-words '()
    "A list of 'electric' words. Typing these will trigger reindentation of the
current line.")

  (setq-default electric-indent-chars '(?\n ?\^?))

  (defun +electric-char (_c)
    (when (and (eolp) +electric-indent-words)
      (save-excursion
	(backward-word)
	(looking-at-p (concat "\\<" (regexp-opt +electric-indent-words))))))
  (add-to-list 'electric-indent-functions #'+electric-char nil #'eq)

  (defun +electric-set (modes &rest plist)
    "Declare :words (list of strings) or :chars (lists of chars) in MODES that
will trigger electric reindentation."
    (declare (indent defun))
    (dolist (mode (enlist modes))
      (let ((hook (intern (format "%s-hook" mode)))
            (fn   (intern (format "+electric-init-%s" mode))))
	(cond ((null (car-safe plist))
               (remove-hook hook fn)
               (unintern fn nil))
              ((fset fn
                     (lambda!
                      (cl-destructuring-bind (&key chars words) plist
			(electric-indent-local-mode +1)
			(if chars (setq electric-indent-chars chars))
			(if words (setq +electric-indent-words words)))))
               (add-hook hook fn)))))))

(use-package highlight-numbers
  :hook (prog-mode . highlight-numbers-mode)
  :config
  (setq highlight-numbers-generic-regexp "\\_<[[:digit:]]+.*\\_>"))

(use-package highlight-escape-sequences
  :hook ((prog-mode conf-mode) . hes-mode))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode)
  :config
  (setq rainbow-delimiters-max-face-count 3))

(use-package flycheck
  :commands (flycheck-list-errors flycheck-buffer)
  :after-call (enter-buffer-hook after-find-file)
  :config
  (setq flycheck-check-syntax-automatically '(save idle-change mode-enabled)))

(use-package flycheck-popup-tip
  :commands (flycheck-popup-tip-show-popup flycheck-popup-tip-delete-popup)
  :init
  (defun +flycheck-show-popup (errors)
    "TODO"
    (flycheck-popup-tip-show-popup errors))
  
  (defun +flycheck-cleanup-popup ()
    "TODO"
    (when (display-graphic-p)
      (flycheck-popup-tip-delete-popup)))

  (define-minor-mode +flycheck-popup-mode
    "TODO"
    :lighter nil
    :group 'flycheck
    (require 'flycheck-popup-tip)
    (let ((hooks '(post-command-hook focus-out-hook)))
      (cond
       ((and +flycheck-popup-mode
             (not (eq flycheck-display-errors-function
                      #'+flycheck-show-popup)))
        (setq flycheck-popup-tip-old-display-function
              flycheck-display-errors-function
              flycheck-display-errors-function
              #'+flycheck-show-popup)
        (dolist (hook hooks)
          (add-hook hook #'+flycheck-cleanup-popup nil t)))
       ((and (not +flycheck-popup-mode)
             (eq flycheck-display-errors-function
                 #'+flycheck-show-popup))
        (setq flycheck-display-errors-function
              flycheck-popup-tip-old-display-function
              flycheck-popup-tip-old-display-function nil)
        (dolist (hook hooks)
          (remove-hook hook #'+flycheck-cleanup-popup t))))))
  (add-hook 'flycheck-mode-hook #'+flycheck-popup-mode)

  :config
  (setq flycheck-popup-tip-error-prefix "x "))

(use-package lsp-mode
  :bind
  (:map lsp-mode-map
        ("C-c l =" . lsp-format-buffer)
        ("C-c l a" . lsp-execute-code-action)
        ("C-c l r" . lsp-rename)
        ("C-c l R" . lsp-workspace-restart))
  :init
  (setq lsp-prefer-flymake nil
        lsp-session-file (no-littering-expand-var-file-name "lsp-session.el")
        lsp-keep-workspace-alive nil
	    lsp-enable-snippet nil)

  (add-hook 'lsp-mode-hook #'disable-bold-faces)
  (setq-hook! 'kill-emacs-hook lsp-restart 'ignore))

(use-package lsp-ui
  :after lsp-mode
  :hook (lsp-mode . lsp-ui-mode)
  :bind
  (:map lsp-ui-mode-map
	    ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
	    ([remap xref-find-references]  . lsp-ui-peek-find-references)
        ("C-c l l" . lsp-ui-sideline-mode)
        ("C-c l d" . lsp-ui-doc-mode)
        ("C-c l e" . lsp-ui-flycheck-list)
        ("C-c l ?" . lsp-ui-peek-find-workspace-symbol))
  :config
  (setq lsp-ui-sideline-enable nil
        lsp-ui-doc-enable t
	    lsp-ui-doc-position 'top
        lsp-ui-doc-max-height 8
        lsp-ui-doc-max-width 35
	    lsp-ui-peek-enable t
	    lsp-ui-peek-list-width 60
	    lsp-ui-peek-peek-height 25))

(use-package company-lsp
  :after lsp-mode
  :config
  (setq company-lsp-async t
	company-lsp-cache-candidates nil)
  (+company-set-backends 'lsp-mode 'company-lsp))

(provide 'config-lang)
;;; config-lang.el ends here

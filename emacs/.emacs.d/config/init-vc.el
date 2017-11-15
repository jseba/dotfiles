(use-package vc-git
  ;; built-in
  :bind
  (:map vc-prefix-map
	("f" . vc-git-grep))
  )

(use-package vc-hooks
  ;; built-in
  :init
  (setq vc-follow-symlinks t))

(use-package magit
  :ensure t
  :init
  (setq magit-save-repository-buffers 'dontask
	magit-refs-show-commit-count 'all
	magit-log-buffer-file-locked t
	magit-revision-show-gravatars nil)
  (setq-default magit-diff-refine-hunk t
		magit-stage-all-confirm nil)
  (add-hook 'magit-mode-hook 'magit-load-config-extensions)
  ;;(add-hook 'after-save-hook 'magit-after-save-refresh-status)
  :bind
  (("C-c g l" . magit-log)
   ("C-c g f" . magit-file-log)
   ("C-c g b" . magit-blame-mode)
   ("C-c g B" . magit-branch)
   ("C-c g c" . magit-checkout)
   ("C-c g d" . magit-ediff-show-working-tree)
   ("C-c g s" . magit-status)
   ("C-c g S" . magit-stage-file)
   ("C-c g r" . magit-rebase)
   ("C-c g U" . magit-unstage-file)))

(use-package git-timemachine
  :ensure t
  :bind
  (("C-c g t" . git-timemachine)))

(use-package git-messenger
  :ensure t
  :init
  (setq git-messenger:show-detail t)
  :bind
  (("C-c g p" . git-messenger:popup-message)
   :map git-messenger-map
   ("C-g" . git-messenger:popup-close)))

(use-package gitignore-mode
  :ensure t)

(use-package gitconfig-mode
  :ensure t)

(use-package git-commit
  :ensure t
  :init
  (remove-hook 'git-commit-finish-query-functions #'git-commit-check-style-conventions)
  ;; (add-hook 'git-commit-mode-hook 'fci-mode)
  )

(use-package gitattributes-mode
  :ensure t)

(use-package diff-hl
  :ensure t
  :init
  (unless (display-graphic-p)
    (diff-hl-margin-mode))
  (add-hook 'dired-mode-hook 'diff-hl-dired-mode)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  (add-hook 'prog-mode-hook 'diff-hl-mode)
  (add-hook 'vc-dir-mode-hook 'diff-hl-mode)
  :bind
  (:map diff-hl-mode-map
	("<left-fringe> <mouse-1>" . diff-hl-diff-goto-hunk)))

(use-package browse-at-remote
  :ensure t)

(provide 'init-vc)

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
  :after magit
  :commands (git-timemachine git-timemachine-toggle)
  :config
  (require 'magit-blame)
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
  :ensure t
  :mode "/\\.gitignore$")

(use-package gitconfig-mode
  :ensure t
  :mode "/\\.?git/?config$"
  :mode "/\\.gitmodules$")


(use-package git-commit
  :ensure t
  :init
  ;; (add-hook 'git-commit-mode-hook 'fci-mode)
  (remove-hook 'git-commit-finish-query-functions #'git-commit-check-style-conventions))

(use-package gitattributes-mode
  :ensure t)

(use-package git-gutter-fringe
  :ensure t
  :commands git-gutter-mode
  :init
  (defun +git|git-gutter-maybe ()
    "Enable `git-gutter-mode' in non-remote buffers."
    (when (and (buffer-file-name)
               (not (file-remote-p (buffer-file-name))))
      (git-gutter-mode)))
  (dolist (mode '(text-mode prog-mode conf-mode))
    (add-hook mode #'+git|git-gutter-maybe)))

(use-package browse-at-remote
  :ensure t)

(provide 'init-vc)

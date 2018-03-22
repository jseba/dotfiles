;;; config-vc.el -- Version control

;;; Commentary:
;;; Code:

(defun +git|git-gutter-maybe ()
  "Enable `git-gutter-mode' in non-remote buffers."
  (when (and (buffer-file-name)
             (not (file-remote-p (buffer-file-name))))
    (git-gutter-mode)))

(use-package vc-git
  :demand t)

(use-package vc-hooks
  :demand t
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
  ;;(add-hook 'after-save-hook 'magit-after-save-refresh-status)
  (add-hook 'magit-mode-hook 'magit-load-config-extensions))

(use-package git-timemachine
  :ensure t
  :after magit
  :commands (git-timemachine git-timemachine-toggle)
  :config
  (require 'magit-blame))

(use-package git-messenger
  :ensure t
  :init
  (setq git-messenger:show-detail t))

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
  (remove-hook 'git-commit-finish-query-functions #'git-commit-check-style-conventions))

(use-package gitattributes-mode
  :ensure t)

(use-package git-gutter-fringe
  :ensure t
  :commands git-gutter-mode
  :init
  (dolist (mode '(text-mode prog-mode conf-mode))
    (add-hook mode #'+git|git-gutter-maybe)))

(use-package browse-at-remote
  :ensure t)

(provide 'config-vc)
;;; config-vc.el ends here

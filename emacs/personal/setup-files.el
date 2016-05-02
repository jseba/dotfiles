(provide 'setup-files)

(setq large-file-warning-threshold 1000000000)

;; Backup settings
(defvar backup-directory "~/.emacs.d/backups")
(if (not (file-exists-p backup-directory))
    (make-directory backup-directory t))
(setq make-backup-files t
      backup-directory-alist `((".*" . ,backup-directory))
      backup-by-copying t
      version-control t
      delete-old-versions t
      kept-old-versions 6
      kept-new-versions 9
      auto-save-default t
      auto-save-timeout 20
      auto-save-interval 200)

;; Dired settings
(setq dired-dwim-target t
      dired-recursive-copies 'always  ; don't ask for copies
      dired-recursive-deletes 'top    ; only ask for the top directory
      dired-listing-switches "-lha")
(add-hook 'dired-mode-hook 'auto-revert-mode) ; automatically refresh dired on changes

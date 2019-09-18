;;; config-vc.el

;;
;; Disable VC, using Magit instead
(use-feature vc-hooks
  :config
  (setq vc-handled-backends nil))

(use-package smerge-mode)

(use-package gitconfig-mode)

(use-package gitignore-mode)

(use-package magit
  :defer-incrementally (dash f s with-editor git-commit package magit)
  :init
  (setq magit-auto-revert-mode nil)
  :config
  (setq git-commit-summary-max-length 50
        git-commit-style-convention-checks '(overlong-summary-line
                                             non-empty-second-line)
	magit-completing-read-function #'magit-builtin-completing-read
        magit-revision-show-gravatars '("^Author:    " . "^:Commit:    ")
        magit-diff-refine-hunk t
        magit-display-buffer-function #'+magit-display-buffer
        magit-popup-display-buffer-action '((+magit-display-popup-buffer)))

  (+popup-set-rule "^\\(?:\\*magit\\|magit:\\)" :ignore t)

  (defun +magit-display-buffer (buffer)
    "Like `magit-display-buffer-fullframe-status-v' with two differences.

1. Magit sub-buffers that aren't spawned from a status screen are opened as popups.
2. The status screen isn't buried when viewing diffs or logs from the status screen."
    (let ((buffer-mode (buffer-local-value 'major-mode buffer)))
      (display-buffer
       buffer
       (cond ((or (derived-mode-p 'eshell-mode)
                  (eq (window-dedicated-p) 'side))
              '(display-buffer-same-window))
             ((or (bound-and-true-p git-commit-mode)
                  (derived-mode-p 'magit-mode))
              (let ((size (if (eq buffer-mode 'magit-process-mode)
                              0.35
                            0.7)))
                `(display-buffer-below-selected
                  . ((window-height . ,(truncate (* (window-height) size)))))))
             ((memq buffer-mode '(magit-process-mode
                                  magit-log-mode
                                  magit-stash-mode))
              '(display-buffer-below-selected))
             ('(display-buffer-same-window))))))

  (defun +magit-display-popup-buffer (buffer &optional alist)
    "TODO"
    (cond ((eq (window-dedicated-p) 'side)
           (if (fboundp '+popup-display-buffer-stacked-side-window)
               (+popup-display-buffer-stacked-side-window buffer alist)
             (display-buffer-in-side-window buffer alist)))
          ((derived-mode-p 'magit-mode)
           (display-buffer-below-selected buffer alist))
          ((display-buffer-in-side-window buffer-alist))))

  (defun +magit--kill-buffer (buffer)
    "TODO"
    (when (and (bufferp buffer) (buffer-live-p buffer))
      (let ((process (get-buffer-process buffer)))
        (if (not (processp process))
            (kill-buffer buffer)
          (with-current-buffer buffer
            (if (process-live-p process)
                (run-with-timer 5 nil #'+magit--kill-buffer buffer)
              (kill-process process)
              (kill-buffer buffer)))))))
  
  (defun +magit-quit (&optional _kill-buffer)
    "Clean up `magit' buffers after quitting `magit-status' and refresh version
control in buffers."
    (interactive)
    (mapc #'+magit--kill-buffer (magit-mode-get-buffers))
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (fboundp 'vc-refresh-state)
          (vc-refresh-state))
        (when (fboundp +vc-update-git-gutter)
          (+vc-update-git-gutter)))))

  (defun +magit-buffer-p (buffer)
    (with-current-buffer buffer
      (and (derived-mode-p 'magit-mode)
           (not (eq major-mode 'magit-process-mode)))))
  (add-to-list 'real-buffer-functions #'+magit-buffer-p nil #'eq)

  (add-hook! '(magit-mode-hook magit-popup-mode-hook)
	     #'hide-mode-line-mode)

  (define-key magit-status-mode-map
    [remap magit-mode-bury-buffer] #'+magit-quit))

(provide 'config-vc.el)
;;; config-vc.el ends here

;;; config-vc.el

(use-package vc
  :ensure nil ;; built-in
  :init
  (setq vc-make-backup-files nil))

(use-package vc-annotate
  :ensure nil
  :config
  (+popup-set-rules
   '(("^\\vc-d" :select nil)
     ("^\\vc-c" :select t))))

(use-package smerge-mode
  :init
  (defhydra +vc-smerge-hydra
    (:hint
     nil
     :pre (if (not smerge-mode) (smerge-mode +1))
     :post (smerge-auto-leave))
    "
                                                         [smerge]
  Movement   Keep           Diff              Other
  ╭─────────────────────────────────────────────────────────╯
     ^_g_^       [_b_] base       [_<_] upper/base    [_C_] Combine
     ^_C-k_^     [_u_] upper      [_=_] upper/lower   [_r_] resolve
     ^_k_ ↑^     [_l_] lower      [_>_] base/lower    [_R_] remove
     ^_j_ ↓^     [_a_] all        [_H_] hightlight
     ^_C-j_^     [_RET_] current  [_E_] ediff                 ╭──────────
     ^_G_^                                                │ [_q_] quit
"
    ("g" (progn (goto-char (point-min)) (smerge-next)))
    ("G" (progn (goto-char (point-max)) (smerge-prev)))
    ("C-j" smerge-next)
    ("C-k" smerge-prev)
    ("j" next-line)
    ("k" previous-line)
    ("b" smerge-keep-base)
    ("u" smerge-keep-upper)
    ("l" smerge-keep-lower)
    ("a" smerge-keep-all)
    ("RET" smerge-keep-current)
    ("\C-m" smerge-keep-current)
    ("<" smerge-diff-base-upper)
    ("=" smerge-diff-upper-lower)
    (">" smerge-diff-base-lower)
    ("H" smerge-refine)
    ("E" smerge-ediff)
    ("C" smerge-combine-with-next)
    ("r" smerge-resolve)
    ("R" smerge-kill-current)
    ("q" nil :color blue)))

(use-package git-commit-mode
  :ensure nil
  :init
  (defun +vc-enforce-git-commit-conventions ()
    (setq fill-column 72
          git-commit-summary-max-length 50
          git-commit-style-convention-checks '(overlong-summary-line
                                               non-empty-second-line)))
  (add-hook 'git-commit-mode-hook #'+vc-enforce-git-commit-conventions)

  (defun +vc-start-in-insert-mode-maybe ()
    "Start `git-commit-mode' in insert state if in a blank commit message,
otherwise in default state."
    (when (and (bound-and-true-p evil-mode)
               (bobp) (eolp))
      (evil-insert-state)))
  (add-hook 'git-commit-setup-hook #'+vc-start-in-insert-mode-maybe))

(use-package gitconfig-mode)

(use-package gitignore-mode)

(use-package git-timemachine
  :init
  (setq git-timemachine-show-minibuffer-details t)
  
  (defun +vc-update-header-line (revision)
    "Show revision details in the header-line, instead of the minibuffer."
    (let* ((date-relative (nth 3 revision))
           (date-full (nth 4 revision))
           (author (if git-timemachine-show-author
                       (concat (nth 6 revision) ": ")
                     ""))
           (sha-or-subject (if (eq git-timemachine-minibuffer-detail 'commit)
                               (car revision)
                             (nth 5 revision))))
      (setq header-line-format
            (format "%s%s [%s (%s)]"
                    (propertize
                     author
                     'face 'git-timemachine-minibuffer-author-face)
                    (propertize
                     sha-or-subject
                     'face 'git-timemachine-minibuffer-detail-face)
                    date-full
                    date-relative))))
  (advice-add #'git-timemachine--show-minibuffer-details
              :override #'+vc-update-header-line)

  (after! evil
    (add-hook 'git-timemachine-mode-hook #'evil-normalize-keymaps))

  (after! magit
    (add-transient-hook! 'git-timemachine-blame
      (require 'magit-blame))))

(use-package magit
  :commands magit-file-delete
  :defer-incrementally (dash f s with-editor git-commit package magit)
  :init
  (setq magit-auto-revert-mode nil)
  :config
  (setq magit-completing-read-function #'magit-builtin-completing-read
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

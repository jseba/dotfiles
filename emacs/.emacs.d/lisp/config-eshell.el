;;; config-eshell.el

(use-package eshell
  :ensure nil ;; built-in
  :init
  (defvar eshell-buffer-name "*eshell*")

  (defvar +eshell-new-shell-on-split t
    "If non-nil, spawn a new `eshell' session after splitting an `eshell'
buffer.")
  (defvar +eshell-kill-window-on-exit nil
    "If non-nil, `eshell' will close windows along with its buffers.")
  (defvar +eshell-buffers (make-ring 25)
    "List of open `eshell' buffers.")

  (defvar +eshell--last-buffer nil)
  
  (defun +eshell--add-buffer (buf)
    (ring-remove+insert+extend +eshell-buffers buf 'grow))

  (defun +eshell--remove-buffer (buf)
    (when-let* ((idx (ring-member +eshell-buffers buf)))
      (ring-remove +eshell-buffers idx)
      t))

  (defun +eshell--bury-buffer (&optional dedicatedp)
    (unless (switch-to-prev-buffer nil 'bury)
      (switch-to-buffer (fallback-buffer)))
    (when (eq major-mode 'eshell-mode)
      (switch-to-buffer (fallback-buffer)))
    (when +eshell-new-shell-on-split
      (when-let* ((win (get-buffer-window (+eshell-open t))))
        (set-window-dedicated-p win dedicatedp))))

  (defun +eshell--setup-window (window &optional flag)
    (when (window-live-p window)
      (set-window-parameter window 'no-other-window flag)
      (set-window-parameter window 'visible flag)))

  (defun +eshell--unused-buffer (&optional newp)
    (or (unless newp
          (cl-loop for buf in (+eshell-buffers)
                   if (and (buffer-live-p buf)
                           (not (get-buffer-window buf t)))
                   return buf))
        (generate-new-buffer eshell-buffer-name)))

  (defun +eshell-last-buffer (&optional noerror)
    "Returns the last opened `eshell' buffer."
    (let ((buffer (cl-find-if #'buffer-live-p (+eshell-buffers))))
      (cond (buffer)
            (noerror nil)
            ((user-error "No live Eshell buffers remaining")))))

  (defun +eshell-buffers ()
    "Returns the list of `eshell' buffers."
    (ring-elements +eshell-buffers))

  (defun +eshell-run-command (command &optional buffer)
    (let ((buffer (or buffer
                      (if (eq major-mode 'eshell-mode)
                          (current-buffer)
                        (cl-find-if #'buffer-live-p (+eshell-buffers))))))
      (unless buffer
        (user-error "No living Eshell buffers available"))
      (unless (buffer-live-p buffer)
        (user-error "Cannot operate on a dead buffer"))
      (with-current-buffer buffer
        (goto-char eshell-last-output-end)
        (goto-char (line-end-position))
        (insert command)
        (eshell-send-input nil t))))

  (defun +eshell-open (arg &optional command)
    "Open `eshell' in the current buffer."
    (interactive "P")
    (when (eq major-mode 'eshell-mode)
      (user-error "Already in an eshell buffer"))
    (let* ((default-directory (if arg
                                  default-directory
                                (+projectile-project-root)))
           (buf (+eshell--unused-buffer)))
      (with-current-buffer (switch-to-buffer buf)
        (if (eq major-mode 'eshell-mode)
            (run-hooks 'eshell-mode-hook)
          (eshell-mode))
        (if command (+eshell-run-command command buf)))
      buf))

  (defun +eshell-open-popup (arg &optional command)
    "Open `eshell' in a popup buffer."
    (interactive "P")
    (let* ((default-directory (if arg
                                  default-directory
                                (+projectile-project-root)))
           (buf (+eshell--unused-buffer)))
      (with-current-buffer (pop-to-buffer buf)
        (if (eq major-mode 'eshell-mode)
            (run-hooks 'eshell-mode-hook)
          (eshell-mode))
        (if command (+eshell-run-command command buf)))
      buf))

  (defun +eshell-open-fullscreen (arg &optional command)
    "Open `eshell' in a separate workspace."
    (interactive "P")
    (let ((default-directory (if arg
                                 default-directory
                               (+projectile-project-root)))
          (buf (+eshell--unused-buf 'new)))
      (set-frame-parameter nil 'saved-wconf (current-window-configuration))
      (delete-other-windows)
      (with-current-buffer (switch-to-buffer buf)
        (eshell-mode)
        (if command (+eshell-run-command command buf)))
      buf))

  (defun +eshell-search-history ()
    "Search the `eshell' command history with `helm'."
    (interactive)
    (helm-eshell-history))

  (defun +eshell-pcomplete ()
    "Use `pcomplete' with completion-in-region backend instead of popup window
at bottom. This ties `pcomplete' into `helm'."
    (interactive)
    (require 'pcomplete)
    (ignore-errors (pcomplete-std-complete)))

  (defun +eshell-quit-or-delete-char (arg)
    "Delete a character ahead of cursor or quit `eshell' if there is nothing to
delete."
    (interactive "p")
    (if (and (eolp)
             (looking-back eshell-prompt-regexp nil))
        (eshell-life-is-too-much)
      (delete-char arg)))

  (defun +eshell-split-below ()
    "Create a new `eshell' window below the current one."
    (interactive)
    (let ((ignore-window-parameters t)
          (dedicatedp (window-dedicated-p))
          (+eshell-new-shell-on-split (or +eshell-new-shell-on-split
                                          (frame-parameter nil 'saved-wconf))))
      (select-window (split-window-vertically))
      (+eshell--bury-buffer dedicatedp)))

  (defun +eshell-split-right ()
    "Create a new `eshell' window to the right of the current one."
    (interactive)
    (let ((ignore-window-parameters t)
          (dedicatedp (window-dedicated-p))
          (+eshell-new-shell-on-split (or +eshell-new-shell-on-split
                                          (frame-parameter nil 'saved-wconf))))
      (select-window (split-window-horizontally))
      (+eshell--bury-buffer dedicatedp)))

  (defun +eshell-switch-to-next ()
    "Switch to the next `eshell' buffer."
    (interactive)
    (when (ring-empty-p +eshell-buffers)
      (user-error "No eshell buffers available"))
    (switch-to-buffer (ring-next +eshell-buffers (current-buffer))))
  
  (defun +eshell-switch-to-previous ()
    "Switch to the last `eshell' buffer that was open and is still alive."
    (interactive)
    (unless (buffer-live-p +eshell--last-buffer)
      (setq +eshell--last-buffer nil)
      (user-error "No last eshell buffer to jump to"))
    (switch-to-buffer +eshell--last-buffer))

  (defun +eshell-switch-to (buffer)
    "Interactively switch to another `eshell' buffer."
    (interactive
     (let ((buffers (list-buffers-in-mode
                     'eshell-mode (delq (current-buffer)
                                        (+eshell-buffers)))))
       (if (not buffers)
           (user-error "No eshell buffers are available")
         (list (completing-read "Eshell buffers"
                                (mapcar #'buffer-name buffers)
                                #'get-buffer
                                'require-match
                                nil
                                nil
                                (when (eq major-mode 'eshell-mode)
                                  (buffer-name (current-buffer))))))))
    (if-let* ((window (get-buffer-window buffer)))
        (select-window window)
      (switch-to-buffer buffer)))

  (defun +eshell-kill-and-close ()
    "Kill the current `eshell' buffer and close its window."
    (interactive)
    (unless (eq major-mode 'eshell-mode)
      (user-error "Not in an eshell buffer"))
    (let ((+eshell-kill-window-on-exit t))
      (kill-this-buffer)))

  (defun +eshell-init ()
    "Initialize and track this `eshell' buffer in `+eshell-buffers'."
    (let ((current-buffer (current-buffer)))
      (dolist (buf (+eshell-buffers))
        (unless (buffer-live-p buf)
          (+eshell--remove-buffer buf)))
      (+eshell--setup-window (get-buffer-window current-buffer))
      (+eshell--add-buffer current-buffer)
      (setq +eshell--last-buffer current-buffer)))

  (defun +eshell-cleanup ()
    "Close window or workspace on quit."
    (let ((buf (current-buffer)))
      (when (+eshell--remove-buffer buf)
        (when-let* ((win (get-buffer-window buf)))
          (+eshell--setup-window win nil)
          (cond ((and (one-window-p t)
                      (window-configuration-p (frame-parameter nil 'saved-wconf)))
                 (set-window-configuration (frame-parameter nil 'saved-wconf))
                 (set-frame-parameter win 'saved-wconf nil))
                ((one-window-p)
                 (let ((prev (save-window-excursion (previous-buffer))))
                   (unless (and prev (real-buffer-p prev))
                     (switch-to-buffer (fallback-buffer)))))
                ((or (window-dedicated-p win)
                     +eshell-kill-windows-on-exit)
                 (let ((ignore-window-parameters t)
                       (popupp (window-dedicated-p win)))
                   (delete-window win)
                   (when popupp
                     (cl-loop for win in (window-list)
                              for buf = (window-buffer win)
                              for mode = (buffer-local-value 'major-mode buf)
                              if (eq mode 'eshell-mode)
                              return (select-window win))))))))))

    (after! evil
      (defun +eshell-init-evil ()
        "Replaces `evil-collection-eshell-next-prompt-on-insert' with
`+eshell-goto-prompt-on-insert' which ensures the point is on the prompt
when changing to insert mode."
        (dolist (hook '(evil-replace-state-entry-hook evil-insert-state-entry-hook))
          (remove-hook hook 'evil-collection-eshell-next-prompt-on-insert t)
          (add-hook hook '+eshell-goto-prompt-on-insert nil t)))
      (add-hook 'eshell-mode-hook #'+eshell-init-evil)

      (defun +eshell-goto-prompt-on-insert ()
        "Move cursor to the prompt when switching to insert mode if point is not
already there."
        (when (< (point) eshell-last-output-end)
          (goto-char
           (if (memq this-command '(evil-append evil-append-line))
               (point-max)
             eshell-last-output-end))))

      (defun +eshell-goto-end-of-prompt ()
        "Move cursor to the prompt end when switching to insert mode."
        (interactive)
        (goto-char (point-max))
        (evil-append 1))

      (evil-define-command +eshell-run (command bang)
                           "TODO"
                           (interactive "<fsh><!>")
                           (let ((buffer (+eshell-last-buffer))
                                 (command (+evil-resolve-vim-path command)))
                             (cond (buffer
                                    (select-window (get-buffer-window buffer))
                                    (+eshell-run-command command buffer))
                                   (bang (+eshell-open nil command))
                                   ((+eshell-open-popup nil command)))))

      (evil-define-operator +eshell-evil-change (beg end type register yank-handler
                                                     delete-func)
                            "Like `evil-change' but will not delete or copy the prompt."
                            (interactive "<R><x><y>")
                            (save-restriction
                              (narrow-to-region eshell-last-output-end (point-max))
                              (evil-change (max beg (point-min))
                                           (if (eq type 'line)
                                               (point-max)
                                             (min (or end (point-max))
                                                  (point-max)))
                                           type
                                           register
                                           yank-handler
                                           delete-func)))

      (evil-define-operator +eshell-evil-change-line (beg end type register
                                                          yank-handler)
                            "Change to end of line."
                            :motion evil-end-of-line
                            (interactive "<R><x><y>")
                            (+eshell-evil-change beg end type register yank-handler #'evil-delete-line))

      (evil-define-operator +eshell-evil-delete (beg end type register yank-handler)
                            "Like `evil-delete' but will not delete or copy the prompt."
                            (interactive "<R><x><y>")
                            (save-restriction
                              (narrow-to-region eshell-last-output-end (point-max))
                              (evil-delete (if beg (max beg (point-min)) (point-min))
                                           (if (eq type 'line)
                                               (point-max)
                                             (min (or end (point-max))
                                                  (point-max)))
                                           type
                                           register
                                           yank-handler)))

      (evil-define-operator +eshell-evil-delete-line (_beg end type register
                                                           yank-handler)
                            "Change to end of line."
                            :motion nil
                            :keep-visual t
                            (interactive "<R><x>")
                            (+eshell-evil-delete (point) end type register yank-handler)))

  (defun +eshell-cd-to-project ()
    "Change to the project root of the current directory."
    (let* ((default-directory (eshell/pwd))
           (project-root (+projectile-project-root 'nocache)))
      (eshell/cd project-root)))

  (defun +eshell-quit-and-close (&rest _)
    "Quit the current `eshell' buffer and close the window it is in."
    (setq-local +eshell-kill-window-on-exit t)
    (throw 'eshell-terminal t))

  (defun eshell-mkdir-and-cd (dir)
    "Create a directory and change into it."
    (make-directory dir t)
    (eshell/cd dir))

  ;; TODO: customize the prompt

  (defface +eshell-prompt-pwd '((t :inherit font-lock-constant-face))
    "TODO"
    :group 'eshell)

  (defface +eshell-prompt-git-branch '((t :inherit font-lock-builtin-face))
    "TODO"
    :group 'eshell)

  (defun +eshell--current-git-branch ()
    (let ((branch (car (cl-loop for match in (split-string
                                              (shell-command-to-string
                                               "git-branch")
                                              "\n")
                                if (string-match-p "^\*" match)
                                collect match))))
      (if (not (eq branch nil))
          (format " [%s]" (substring branch 2))
        "")))

  (defun +eshell-prompt ()
    "Generate the prompt string for `eshell'. Use for `eshell-prompt-function'."
    (require 'shrink-path)
    (concat (if (bobp) "" "\n")
            (let ((pwd (eshell/pwd)))
              (propertize (if (equal pwd "~")
                              pwd
                            (abbreviate-file-name (shrink-path-file pwd)))
                          'face '+eshell-prompt-pwd))
            (propertize (+eshell--current-git-branch)
                        'face '+eshell-prompt-git-branch)
            (propertize " λ" 'face (if (zerop eshell-last-command-status)
                                       'success
                                     'error))
            " "))

  (defun +eshell-define-alias (&rest aliases)
    "Define aliases for `eshell'."
    (or (cl-evenp (length aliases))
        (signal 'wrong-number-of-arguments
                (list 'even (length aliases))))
    (after! eshell
      (while aliases
        (let ((alias (pop aliases))
              (command (pop aliases)))
          (if-let* ((oldval (assoc alias +eshell-aliases)))
              (setcdr oldval (list command))
            (push (list alias command) +eshell-aliases))))
      (when (bound-p 'eshell-command-aliases-list)
        (if +eshell--default-aliases
            (setq eshell-command-aliases-list
                  (append +eshell--default-aliases
                          +eshell-aliases))
          (setq eshell-command-aliases-list +eshell-aliases)))))
  
  (setq eshell-directory-name (concat %etc-dir "eshell/")
        eshell-banner-message
        '(format "%s %s\n"
                 (propertize (format " %s " (string-trim (buffer-name)))
                             'face 'mode-line-highlight)
                 (propertize (current-time-string)
                             'face 'font-lock-keyword-face))
        eshell-scroll-to-bottom-on-input 'all
        eshell-scroll-to-bottom-on-output 'all
        eshell-buffer-shorthand t
        eshell-kill-processes-on-exit t
        eshell-hist-ignoredups nil
        eshell-input-filter #'eshell-input-filter-initial-space
        eshell-prompt-regexp "^.* λ "
        eshell-prompt-function #'+eshell-prompt
        eshell-glob-case-insensitive t
        eshell-error-if-no-glob t)

  (add-hook 'eshell-mode-hook #'mark-buffer-real)
  (add-hook 'eshell-mode-hook #'+eshell-init)
  (add-hook 'eshell-exit-hook #'+eshell-cleanup)
  (add-hook 'eshell-mode-hook #'smartparens-mode)
  (add-hook 'eshell-mode-hook #'hide-mode-line-mode)

  (defun +eshell-remove-fringes ()
    (set-window-fringes nil 0 0)
    (set-window-margins nil 1 nil))
  (add-hook 'eshell-mode-hook #'+eshell-remove-fringes)

  (defun +eshell-enable-text-wrapping ()
    (visual-line-mode +1)
    (set-display-table-slot standard-display-table 0 ?\ ))
  (add-hook 'eshell-mode-hook #'+eshell-enable-text-wrapping)

  (advice-add #'eshell-write-aliases-list :override #'ignore)

  (after! em-term
    (dolist (cmd '("tmux" "htop" "bash" "zsh" "vim" "nvim"))
      (add-to-list 'eshell-visual-commands cmd)))

  (defvar +eshell--default-aliases nil)
  (defvar +eshell-aliases
    '(("q" "exit")
      ("clear" "clear-scrollback")
      ("l" "ls -lh")
      ("ll" "ls -lah")
      ("rg" "rg --color=always")
      ("ag" "ag --color=always")))
  
  (defun +eshell-init-aliases ()
    (setq +eshell--default-aliases eshell-command-aliases-list
          eshell-command-aliases-list
          (append eshell-command-aliases-list
                  +eshell-aliases)))
  (add-hook 'eshell-alias-load-hook #'+eshell-init-aliases)

  (defun +eshell-init-keymap ()
    (after! evil
      (evil-define-key* 'normal eshell-mode-map
                        [return] #'+eshell-goto-end-of-prompt
                        "c"      #'+eshell-evil-change
                        "C"      #'+eshell-evil-change-line
                        "d"      #'+eshell-evil-delete
                        "D"      #'+eshell-evil-delete-line)
      (evil-define-key* 'insert eshell-mode-map
                        [tab]    #'+eshell-pcomplete
                        "\C-j"   #'evil-window-down
                        "\C-k"   #'evil-window-up
                        "\C-h"   #'evil-window-left
                        "\C-l"   #'evil-window-right
                        "\C-d"   #'+eshell-quit-or-delete-char
                        "\C-p"   #'eshell-previous-input
                        "\C-n"   #'eshell-next-input))
    (define-key! eshell-mode-map
      (kbd "C-s")   #'+eshell-search-history
      (kbd "C-c s") #'+eshell-split-below
      (kbd "C-c v") #'+eshell-split-right
      (kbd "C-c x") #'+eshell-kill-and-close
      [remap split-window-below] #'+eshell-split-below
      [remap split-window-right] #'+eshell-split-right
      [remap backward-to-bol-or-indent] #'eshell-bol
      [remap backward-kill-to-bol-or-indent] #'eshell-kill-input
      [remap evil-window-split] #'+eshell-split-below
      [remap evil-window-vsplit] #'+eshell-split-right))
  (add-hook 'eshell-first-time-mode-hook #'+eshell-init-keymap))

(use-package eshell-z
  :after eshell
  :config
  (unless (file-exists-p eshell-z-freq-dir-hash-table-file-name)
    (setq eshell-z-freq-dir-hash-table-file-name
          (expand-file-name "z" eshell-directory-name))))

(provide 'config-eshell)
;;; config-eshell.el ends here

;;; init --- JSeba's init.el file
;;; Commentary:
;;; Code:
(add-to-list 'load-path "~/.emacs.d/user")

;; Set interface settings as early as possible
(when window-system
  (if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
  (if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
  (if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
  (if (fboundp 'horizontal-scroll-bar-mode) (horizontal-scroll-bar-mode -1)))
(setq scroll-margin 5
      scroll-conservatively 1000
      scroll-preserve-screen-position 1
      scroll-step 1
      frame-title-format '("" invocation-name " - " (:eval (if (buffer-file-name)
                                                               (abbreviate-file-name (buffer-file-name))
                                                             "%b")))
      inhibit-startup-screen t
      initial-scratch-message ""
      visible-bell nil)
(size-indication-mode t)
(set-face-attribute 'default t :font "xos4 Terminess Powerline 9")
(set-face-attribute 'default nil :font "xos4 Terminess Powerline 9")
(defalias 'yes-or-no-p 'y-or-n-p)

;; Setup package manager
(require 'package)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))
(setq package-enable-at-startup nil)
(or (file-exists-p package-user-dir)
    (package-refresh-contents))
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
(require 'use-package))
(require 'bind-key)
(require 'diminish)

(use-package base16-theme
  :ensure t
  :init
  (load-theme 'base16-bright-dark t))
;; (use-package evil
;;   :ensure t
;;   :bind
;;   (:map evil-normal-state-map ((";" . evil-ex)))
;;   :init
;;   (setq evil-emacs-state-cursor    '("red" box)
;;         evil-normal-state-cursor   '("blue" box)
;;         evil-visual-state-cursor   '("orange" box)
;;         evil-insert-state-cursor   '("green" bar)
;;         evil-replace-state-cursor  '("red" bar)
;;         evil-operator-state-cursor '("red" hollow))
;;   (evil-mode))
;; (use-package evil-leader
;;   :ensure t
;;   :diminish evil-leader-mode
;;   :init
;;   (global-evil-leader-mode)
;;   (evil-leader/set-leader ","))
;; (use-package evil-tabs
;;   :ensure t
;;   :diminish evil-tabs-mode
;;   :init
;;   (global-evil-tabs-mode))
;; (use-package key-chord
;;   :ensure t
;;   :diminish key-chord-mode
;;   :init
;;   (setq key-chord-two-keys-delay 0.5)
;;   (key-chord-mode 1)
;;   :config
;;   (key-chord-define evil-insert-state-map "jj" 'evil-normal-state))
(use-package helm
  :ensure t
  :diminish helm-mode
  :init
  (require 'helm-mode)
  (setq helm-candidate-number-limit 100
        helm-idle-delay 0.0
        helm-input-idle-delay 0.01
        helm-quick-update t
        helm-autoresize-mode nil
        helm-autoresize-max-height 30
        helm-autoresize-min-height 30
        helm-ff-skip-boring-files t
        helm-mode-fuzzy-match t
        helm-completion-in-region-fuzzy-match t)
  (helm-mode)
  :bind
  (("C-c h" . helm-mini)
   ("C-h a" . helm-apropos)
   ("C-x C-b" . helm-buffers-list)
   ("C-x b" . helm-buffers-list)
   ("C-x C-f" . helm-find-files)
   ("M-x" . helm-M-x)
   ("M-y" . helm-show-kill-ring)
   ("C-x c o" . helm-occur)
   ("C-x c s" . helm-swoop))
  :config
  (ido-mode -1))
(use-package projectile
  :ensure t
  :init
  (projectile-global-mode 1)
  :config
  (setq projectile-indexing-method 'native
        projectile-enable-caching t))
(use-package helm-projectile
  :ensure t
  :bind
  (("C-c C-f" . helm-projectile-find-file)
   ("C-c C-d" . helm-projectile-find-dir))
  :init
  (setq helm-projectile-fuzzy-match t))
(use-package helm-gtags
  :ensure t
  :init
  (setq helm-gtags-fuzzy-match nil
        helm-gtags-direct-helm-completing t
        helm-gtags-display-style 'detail
        helm-gtags-ignore-case t
        helm-gtags-prefix-key "\C-t"
        helm-gtags-suggested-key-mapping t))
;; (use-package irony
;;   :ensure t
;;   :init
;;   (add-hook 'c++-mode-hook 'irony-mode)
;;   (add-hook 'c-mode-hook 'irony-mode)
;;   :config
;;   (defun my-irony-mode-hook ()
;;     (define-key irony-mode-map [remap completion-at-point] 'irony-completion-at-point-async)
;;     (define-key irony-mode-map [remap complete-symbol] 'irony-completion-at-point-async))
;;   (add-hook 'irony-mode-hook 'my-irony-mode-hook)
;;   (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options))
(use-package company
  :ensure t
  :bind
  ("C-;" . company-complete-common)
  :init
  (add-hook 'after-init-hook 'global-company-mode)
  :config
  (setq company-idle-delay nil
        company-minimum-prefix-length 2
        company-show-numbers nil
        company-tooltip-limit 10
        company-abbrev-downcase nil
        company-backends '(company-gtags)))
;; (use-package company-irony
;;   :ensure t
;;   :config
;;   (add-to-list 'company-backends 'company-irony))
(use-package rtags
  :ensure t)
(use-package cmake-ide
  :ensure t)
(use-package magit
  :ensure t
  :bind
  (("C-c g l" . magit-log)
   ("C-c g f" . magit-file-log)
   ("C-c g b" . magit-blame-mode)
   ("C-c g b" . magit-branch)
   ("C-c g c" . magit-checkout)
   ("C-c g s" . magit-status)
   ("C-c g r" . magit-rebase)
   ("C-c g t" . magit-tag))
  :init
  (add-hook 'magit-mode-hook 'magit-load-config-extensions)
  :config
  (set-default 'magit-stage-all-confirm nil)
  (defadvice magit-status (around magit-fullscreen activate)
    (window-configuration-to-register :magit-fullscreen)
    ad-do-it
    (delete-other-windows)))
(use-package flycheck
  :ensure t
  :diminish flycheck-mode
  :init
  (global-flycheck-mode))
(use-package flycheck-tip
  :ensure t
  :config
  (flycheck-tip-use-timer 'verbose))
(use-package volatile-highlights
  :ensure t
  :diminish volatile-highlights-mode
  :config
  (volatile-highlights-mode 1))
(use-package smartparens
  :ensure t
  :init
  (sp-use-paredit-bindings)
  :config
  (require 'smartparens-config)
  (setq sp-base-key-bindings 'paredit
        sp-autoskip-closing-pair 'always
        sp-hybrid-kill-entire-symbol nil))
(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :init
  (global-undo-tree-mode))
(use-package golden-ratio
  :ensure t
  :diminish golden-ratio-mode
  :init
  (golden-ratio-mode)
  :config
  (setq golden-ratio-exclude-modes '("ediff-mode"
                                     "gud-mode"
                                     "gdb-locals-mode"
                                     "gdb-registers-mode"
                                     "gdb-breakpoints-mode"
                                     "gdb-threads-mode"
                                     "gdb-frames-mode"
                                     "gdb-inferior-io-mode"
                                     "gdb-disassembly-mode"
                                     "gdb-memory-mode"
                                     "magit-log-mode"
                                     "magit-reflog-mode"
                                     "magit-status-mode"
                                     "IELM"
                                     "eshell-mode"
                                     "dired-mode")))
(use-package expand-region
  :ensure t
  :bind ("M-m" . er/expand-region))
(use-package highlight-numbers
  :ensure t
  :init
  (add-hook 'prog-mode-hook 'highlight-numbers-mode))
(use-package cc-mode
  :ensure t
  :bind (:map c-mode-base-map
              ("RET" . newline-and-indent))
  :init
  (setq c-default-style "stroustrup"
        c-basic-offset 4
        c-toggle-hungry-state 1)
  (setq-default indent-tabs-mode nil
                tab-width 4)
  (add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
  (defun my-c++-mode-hook ()
    (electric-indent-mode)
    (diminish 'electric-mode)
    (c-set-style "stroustrup")
    (c-set-offset 'innamespace '0) ;; don't indent when in namespace
    )
  (add-hook 'c-mode-hook 'helm-gtags-mode)
  (add-hook 'c++-mode-hook 'helm-gtags-mode)
  (add-hook 'c++-mode-hook 'my-c++-mode-hook))
(use-package fiplr
  :ensure t
  :bind
  ("C-c f" . fiplr-find-file)
  :config
  (setq fiplr-root-markers '(".git" ".svn")
        fiplr-ignored-globs '((directories (".git" ".svn")))))
(use-package sr-speedbar
  :ensure t
  :bind
  ("C-c e" . sr-speedbar-toggle)
  :init
  (setq speedbar-show-unknown-files)
  :config
  (setq speedbar-use-images nil))

;;; Functions
;; Smarter move to beginning of line
(defun smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between first non-whitespace character and
the beginning of the line."

  (interactive "^p")
  (setq arg (or arg 1))

  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))
  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

(defun close-and-kill-this-pane ()
  "If there are multiple windows, then close this pane and kill the buffer in it also."
  (interactive)
  (kill-this-buffer)
  (if (not (one-window-p))
    (delete-window)))

;;;; Editing
;;; Basics
(setq mode-require-final-newline t)              ; make sure the file ends in '\n'
(setq-default indent-tabs-mode nil               ; use spaces instead of tabs
              tab-width 4                        ; use 4 spaces for a tab character
              tab-always-indent 'complete        ; make tab indent then complete
              indent-tabs-mode nil               ; don't replace spaces with tabs when formatting
              ibuffer-use-other-window t)        ; always display ibuffer in other window
(set-terminal-coding-system 'utf-8)              ; use UTF-8 by default
(set-keyboard-coding-system 'utf-8)
(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)
(delete-selection-mode)                          ; when typing over a selected region, delete then insert

;; Replace dabbrev-expand with hippie-expand
(global-set-key (kbd "M-/") 'hippie-expand)
(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev                      ; expand from current buffer
        try-expand-dabbrev-from-all-buffers     ; expand from all other buffers
        try-expand-dabbrev-from-kill            ; expand from the kill ring
        try-complete-file-name-partially        ; complete text as file name
        try-complete-file-name
        try-expand-all-abbrevs                  ; expand word before point from all abbrev tables
        try-expand-list                         ; expand current line to entire line in buffer
        try-expand-line
        try-complete-lisp-symbol-partially      ; complete LISP symbol
        try-complete-lisp-symbol))

;; Highlight current-line
(global-hl-line-mode)

;; Toggle whitespace

;; Easier window navigation: Shift-{left,right,up,down}
(windmove-default-keybindings)

;; Keymappings
(global-set-key [remap move-beginning-of-line]
                'smarter-move-beginning-of-line)
(global-set-key (kbd "C-a") 'smarter-move-beginning-of-line)
(global-set-key (kbd "C-k") 'close-and-kill-this-pane)
(define-key prog-mode-map (kbd "C-c c") 'comment-region)
(define-key prog-mode-map (kbd "C-c u") 'uncomment-region)

;; Backups
(defvar backup-directory "~/.emacs.d/backups/")
(unless (file-exists-p backup-directory)
  (make-directory backup-directory t))
(setq savehist-additional-variables '(search ring regexg-search-ring)
      savehist-autosave-interval 60
      make-backup-files t
      backup-directory-alist `(("." . ,backup-directory))
      backup-by-copying t
      version-control t
      delete-old-versions t
      kept-old-versions 6
      kept-new-versions 9
      auto-save-default t
      auto-save-timeout 20
      auto-save-interval 200)
(winner-mode 1)

;; Modeline
(column-number-mode 1)

;; Mouse
(when (boundp 'mouse-wheel-scroll-amount)
  (setq mouse-wheel-scroll-amount '(0.01)))

;;; Setup programming modes
(add-hook 'prog-mode-hook 'linum-mode)
(add-hook 'prog-mode-hook '(lambda ()
                             (interactive)
                             (setq show-trailing-whitespace 1)))
(define-key prog-mode-map (kbd "C-c w") 'whitespace-mode)

;;; Semantic parsing
(semantic-mode 1)

;;; Diff mode
(add-hook 'diff-mode-hook (lambda ()
							(setq-local whitespace-style
										'(face
										  tabs
										  tab-mark
										  spaces
										  space-mark
										  trailing
										  indentation::space
										  indentation::tab
										  newline
										  newline-mark))
							(whitespace-mode 1)))

;;; Dired settings
(setq dired-dwim-target t
      dired-recursive-copies 'always   ; don't ask for copies
      dired-recursive-deletes 'top     ; only ask for the top directory
      dired-listing-switches "-lha")
(add-hook 'dired-mode-hook 'auto-revert-mode)
(add-hook 'dired-mode-hook 'helm-gtags-mode)

;;; GDB settings
(setq gdb-many-windows t
      gdb-show-main t)

;;; Load local machine settings
(require 'bats)

(provide 'init)
;;; init.el ends here

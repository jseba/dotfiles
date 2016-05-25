;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;;  ~~ JSeba's .emacs setup --
;;;
;;;   Currently setup primarily for a C++ workflow
;;;
;;;   Keybinding policy:
;;;     * C-x: primary, system level commands (mostly just overrides with better subsystems)
;;;     * C-c: secondary, user level commands (most packages have bindings here)
;;;     * C-.: tertiary commands (mostly temporary/testing)
;;;
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Begin

(add-to-list 'load-path "~/.emacs.d/site")

;; Set core interface settings as early as possible
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'horizontal-scroll-bar-mode) (horizontal-scroll-bar-mode -1))
(setq inhibit-startup-screen t
      initial-scratch-message ""
      visible-bell nil)
(size-indication-mode t)
(set-face-attribute 'default t :font "Source Code Pro 12")
(set-face-attribute 'default nil :font "Source Code Pro 12")

;; Sane defaults
(defalias 'yes-or-no-p 'y-or-n-p)

;; Setup package manager and use-package
(require 'package)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
;; TODO: make it easy to prefer stable vs latest melpa
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

(use-package benchmark-init
  :ensure t
  :init
  (benchmark-init/activate))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Appearance
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Turn off blinking cursor
(blink-cursor-mode -1)

;; Turn off bell ring
(setq ring-bell-function 'ignore)

;; Improve scrolling
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

;; Mode line settings
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

;; Better frame titles (show file name if buffer is file, or buffer name otherwise)
(setq frame-title-format '("" invocation-name " - " (:eval (if (buffer-file-name)
                                                               (abbreviate-file-name (buffer-file-name))
                                                             "%b"))))

;; Set the theme
(use-package material-theme
  :ensure t
  :init
  (load-theme 'material-light t))

;; Use smart-mode-line
(use-package smart-mode-line
  :ensure t
  :init
  (setq sml/no-confirm-load-theme t
        sml/theme nil)
  (add-hook 'after-init-hook #'sml/setup))

;; Flash the cursor after a large movement
(use-package beacon
  :ensure t
  :config
  (beacon-mode +1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Editing
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Use major modes to configure indentation
(setq-default indent-tabs-mode nil
              tab-width 4)

;; Indent before completing
(setq-default tab-always-indent 'complete)

;; Always display the ibuffer in other window
(setq-default ibuffer-use-other-window t)

;; Newline at end of file
(setq require-final-newline t)

;; When typing over a selected region, delete then insert
(delete-selection-mode)

;; Use UTF-8 by default
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)

;; Extra highlighting
(use-package volatile-highlights
  :ensure t
  :diminish volatile-highlights-mode
  :config
  (volatile-highlights-mode))

;; AFTER volatile-highlights!
;; add cutting the current line without marking i
(use-package rect
  :init
  (defadvice kill-region (before smart-cut activate compile)
    "When called interactively with no active region, kill a single line instead."
    (interactive
     (if mark-active (list (region-beginning) (region-end) rectangle-mark-mode)
       (list (line-beginning-position)
             (line-beginning-position 2))))))

;; tramp (for sudo)
;; (use-package tramp
;;   :init
;;   (setq tramp-default-method "ssh"))

;; flyspell spell-checker
(use-package flyspell
  ;; built-in
  :init
  (setq ispell-program-name "aspell" ;; use aspell instead of ispell
        ispell-extra-args '("--sug-mode=ultra"))
  :config
  (defun enable-flyspell ()
    (flyspell-mode +1))
  (when (executable-find ispell-program-name)
    (add-hook 'text-mode-hook 'enable-flyspell)))

;; keep an eye on whitespace misuse
(use-package whitespace
  ;; built-in
  :init
  (setq whitespace-line-column 120
        whitespace-style '(face tabs empty trailing lines-tail))
  (defun enable-whitespace ()
    (whitespace-mode +1))
  :config
  (add-hook 'text-mode-hook 'enable-whitespace)
  (add-hook 'before-save-hook 'whitespace-cleanup))

;; enable change region case commands
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; expand-region
(use-package expand-region
  :ensure t
  :bind
  ("C-=" . er/expand-region))

;; Projectile
(use-package projectile
  :ensure t
  :init
  (setq projectile-indexing-method 'native
        projectile-enable-caching t
        projectile-find-dir-includes-top-level t)
  :config
  (run-with-idle-timer 10 nil #'projectile-cleanup-known-projects)
  (projectile-global-mode))

;; Use avy for quick navigation to things
(use-package avy
  :ensure t
  :bind
  (("C-c j" . avy-goto-word-or-subword-1))
  :init
  (setq avy-background t
        avy-style 'at-full))

;; Use anzu for enhanced isearch & query-replace
(use-package anzu
  :ensure t
  :diminish anzu-mode
  :bind
  (("M-%" . anzu-query-replace)
   ("C-M-%" . anzu-query-replace-regexp))
  :config
  (global-anzu-mode))

;; Dired enhancements
(use-package dired-x
  ;; built-in
  :init
  (put 'dired-find-alternate-file 'disabled nil)  ;; Re-use current buffer when pressing 'a'
  (setq dired-recursive-copies 'always ;; Don't ask when copying
        dired-recursive-deletes 'top   ;; Only ask for top dir when deleting
        dired-dwim-target t))          ;; Use the dired buffer in other window if it exists

;; Ediff enhancements
(use-package ediff
  ;; built-in
  :init
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)) ;; don't start another window

;; Clean up old buffers automatically
(use-package midnight
  ;; built-in
  )

;; Smarter kill-ring navigation
(use-package browse-kill-ring
  :ensure t
  :bind
  ("M-y" . browse-kill-ring)
  :init
  (browse-kill-ring-default-keybindings))

;; Saner regex syntax
(use-package re-builder
  ;; built-in
  :init
  (setq reb-re-syntax 'string))

;; eshell
(use-package eshell
  ;; built-in
  :init
  ;; create a new shell even if there is one already
  (defun new-eshell ()
    (interactive)
    (eshell t))
  (setq eshell-directory-name (expand-file-name "eshell" user-emacs-directory))
  :bind
  (("C-c t" . eshell)
   ("C-c T" . new-eshell)))

;; Semanticdb
(use-package semantic
  ;; built-in
  :init
  (setq semanticdb-default-save-directory (expand-file-name "semanticdb" user-emacs-directory)))

;; Compile mode
(use-package compile
  ;; built-in
  :init
  (setq compilation-ask-about-save nil  ;; just save before compiling, don't ask
        compilation-always-kill t       ;; automatically kill old compile processes before starting a new one
        compilation-scroll-output 'first-error ;; automatically scroll to first error
        ))

;; Colorize compile mode
(use-package ansi-color
  ;; built-in
  :init
  (defun colorize-compilation-buffer ()
    "Colorize a compilation mode buffer."
    (interactive)
    ;; don't mess with child modes
    (when (eq major-mode 'compilation-mode)
      (let ((inhibit-read-only t))
        (ansi-color-apply-on-region (point-min) (point-max)))))
  (add-hook 'compilation-filter-hook #'colorize-compilation-buffer))

;; Better undo
(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :config
  (global-undo-tree-mode))

;; Enable winner mode
(use-package winner
  ;; built-in
  :config
  (winner-mode +1))

;; Highlighting for diffs
(use-package diff-hl
  :ensure t
  :init
  (add-hook 'dired-mode-hook 'diff-hl-dired-mode)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  :config
  (global-diff-hl-mode +1))

;; Enable intuitive repeating commands
(use-package smartrep
  :ensure t)

;; Operate on numbers
(use-package operate-on-number
  :ensure t
  :init
  (smartrep-define-key global-map "C-c ."
                       '(("+" . apply-operation-to-number-at-point)
                         ("-" . apply-operation-to-number-at-point)
                         ("*" . apply-operation-to-number-at-point)
                         ("/" . apply-operation-to-number-at-point)
                         ("\\" . apply-operation-to-number-at-point)
                         ("^" . apply-operation-to-number-at-point)
                         ("<" . apply-operation-to-number-at-point)
                         (">" . apply-operation-to-number-at-point)
                         ("#" . apply-operation-to-number-at-point)
                         ("%" . apply-operation-to-number-at-point)
                         ("'" . apply-operation-to-number-at-point))))


(use-package helm-gtags
  :ensure t
  :init
  (setq helm-gtags-fuzzy-match nil
        helm-gtags-direct-helm-completing t
        helm-gtags-display-style 'detail
        helm-gtags-ignore-case t
        helm-gtags-prefix-key "\C-t"
        helm-gtags-suggested-key-mapping t))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Autocompletion
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package company
  :ensure t
  :bind
  ("C-;" . company-complete-common)
  :init
  (add-hook 'after-init-hook 'global-company-mode)
  :config
  (setq company-idle-delay 0.5
        company-minimum-prefix-length 2
        company-show-numbers nil
        company-tooltip-limit 10
        company-abbrev-downcase nil
        ;; invert the navigation direction if pop-up is above point
        company-tooltip-flip-when-above t))

;; Replace dabbrev with hippie-expand
(use-package hippie-exp
  ;; built-in
  :bind
  ("M-/" . hippie-expand)
  :init
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
        try-complete-lisp-symbol)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; IDO
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package ido
  ;; built-in
  :init
  (setq ido-enable-prefix nil
        ido-enable-flex-matching t
        ido-create-new-buffer 'always
        ido-use-filename-at-point 'guess
        ido-max-prospects 10
        ido-save-directory-list-file (expand-file-name "ido.hist" user-emacs-directory)
        ido-default-file-method 'selected-window
        ido-auto-merge-work-directories-length -1)
  :config
  (ido-mode +1))

;; Use ido in more places
(use-package ido-ubiquitous
  :ensure t
  :config
  (ido-ubiquitous-mode +1))

;; smarter fuzzy matching for ido
(use-package flx-ido
  :ensure t
  :init
  ;; disable ido faces to use flx highlights
  (setq ido-use-faces nil)
  :config
  (flx-ido-mode +1))

;; Smarter M-x - smex
(use-package smex
  :ensure t
  :bind
  (("M-x" . smex)
   ("M-X" . smex-major-mode-commands))
  :init
  ;; remember most frequently used items
  (setq smex-save-file (expand-file-name ".smex-mru" user-emacs-directory))
  (smex-initialize))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Programming (General)
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Base config for all programming modes
(use-package prog-mode
  ;; built-in
  :init
  ;; Hook functions
  (defun font-lock-comment-annotations ()
    "Highlight a bunch of common comment annotations."
    (font-lock-add-keywords
     nil '(("\\<\\(\\(FIX\\(ME\\)?\\|TODO\\|OPTIMIZE\\|HACK\\|REFACTOR\\):\\)"
            1 font-lock-warning-face t))))
  (set (make-local-variable 'comment-auto-fill-only-comments) t)
  :config
  (when (executable-find ispell-program-name)
    (flyspell-prog-mode))
  (add-hook 'prog-mode-hook #'font-lock-comment-annotations))

;; Show the name of the current function in the modeline
(use-package which-func
  ;; built-in
  :init
  (add-hook 'prog-mode-hook #'which-func-mode))

;; Use flycheck for as-you-type syntax checking
(use-package flycheck
  ;; built-in
  :config
  (if (fboundp 'global-flycheck-mode)
      (global-flycheck-mode +1)
    (add-hook 'prog-mode-hook 'flycheck-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Programming - C/C++
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

(use-package ggtags
  :ensure t
  :bind
  :init
  (add-hook 'c-mode-common-hook
            (lambda ()
              (when (derived-mode-p 'c-mode 'c++-mode)
                (ggtags-mode +1)))))

(use-package rtags
  :ensure t
  :init
  (require 'company)
  (use-package flycheck-rtags
    :init
    (setq rtags-autostart-diagnostics t
          flycheck-disabled-checkers '(c/c++-clang c/c++-gcc)))
  (use-package company-rtags
    :init
    (defun my-company-rtags-hook ()
      (setq company-rtags-begin-after-member-access t
            rtags-autostart-diagnostics t
            rtags-completions-enabled t
            company-backends (delete 'company-clang company-backends)))
    (add-to-list 'company-backends 'company-rtags)
    (add-hook 'c-mode-common-hook #'my-company-rtags-hook))

  (defun my-flycheck-rtags-hook ()
    (flycheck-select-checker 'rtags)
    (setq-local flycheck-highlighting-mode nil)
    (setq-local flycheck-check-syntax-automatically nil))
  (add-hook 'c-mode-common-hook #'rtags-start-process-unless-running)
  (add-hook 'c-mode-common-hook #'my-flycheck-rtags-hook))

;(use-package irony
;  :ensure t
;  :init
;  (defun my-irony-mode-hook ()
;    (define-key irony-mode-map [remap completion-at-point] #'irony-completion-at-point-async)
;    (define-key irony-mode-map [remap complete-symbol] #'irony-completion-at-point-async))
;  (add-hook 'irony-mode-hook #'my-irony-mode-hook)
; (add-hook 'irony-mode-hook #'irony-cdb-autosetup-compile-options)
; (add-hook 'c-mode-hook (lambda () (setq irony-additional-clang-options '("-std=c11"))))
; (add-hook 'c++-mode-hook (lambda () (setq irony-addition-clang-options '("-std=c++11")))))

(use-package cmake-ide
  :ensure t
  :init
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Version Control
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Magit needs no explanation
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
  (setq magit-save-repository-buffers 'dontask
        magit-refs-show-commit-count 'all
        ;; Use separate buffers for one-file logs to prevent
        ;; having to reset the filter in the full log view
        magit-log-buffer-file-locked t
        magit-revision-show-gravatars nil))

;; gitconfig mode
(use-package gitconfig-mode
  :ensure t)

;; gitignore mode
(use-package gitignore-mode
  :ensure t)

(use-package smartparens
  :ensure t
  :init
  (sp-use-paredit-bindings)
  :config
  (require 'smartparens-config)
  (setq sp-base-key-bindings 'paredit
        sp-autoskip-closing-pair 'always
        sp-hybrid-kill-entire-symbol nil))
(use-package highlight-numbers
  :ensure t
  :init
  (add-hook 'prog-mode-hook 'highlight-numbers-mode))


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

(provide 'init)
;;; init.el ends here

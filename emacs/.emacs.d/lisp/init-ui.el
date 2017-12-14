;;; core/init-ui.el -*- lexical-binding: t; -*-

(defvar nx-after-init-ui-hook nil
  "A list of hooks to run after initializing the UI.")
(defvar nx-after-make-console-frame-hook nil
  "A list of hooks to run after making a console frame.")
(defvar nx-after-make-system-frame-hook nil
  "A list of hooks to run after making a window-system frame.")

(defvar nx-theme 'ample
  "Default theme.")
(defvar nx-font (font-spec :family "Source Code Pro" :size 12)
  "The default font. Should be a FONT-SPEC.")
(defvar nx-big-font (font-spec :family "Source Code Pro" :size 14)
  "The default big font. Should be a FONT-SPEC.")
(defvar nx-variable-pitch-font nil
  "The default font to use for variable-pitch text. Should be a FONT-SPEC.")

(defvar nx-major-mode-names '((emacs-lisp-mode . "Elisp"))
  "An alist mapping major modes symbols to strings or functions
that will return a string).")

(defconst nx--initial-frame (selected-frame)
  "The frame (if any) active during Emacs initialization.")

(setq-default
 minibuffer-prompt-properties '(read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt)
 idle-update-delay 2
 bidi-display-reordering nil
 blink-matching-paren nil
 cursor-in-non-selected-windows nil
 display-line-numbers-width 3
 frame-inhibit-implied-resize t)

(let ((no-border '(internal-border-width . 0)))
  (add-to-list 'default-frame-alist no-border)
  (add-to-list 'initial-frame-alist no-border))

(setq use-file-dialog nil
      use-dialog-box nil
      frame-title-format '("" invocation-name " - "
			   (:eval (if (buffer-file-name)
                          (abbreviate-file-name (buffer-file-name "%b"))))))

(defun nx|init-ui (&optional frame)
  "Set the theme and load the font (in that order)."
  (when nx-theme
    (load-theme nx-theme t))
  (condition-case-unless-debug ex
      (when (display-graphic-p)
        (when (fontp nx-font)
          (set-frame-font nx-font nil (if frame (list frame) t))
          (set-face-attribute 'fixed-pitch frame :font nx-font))
        (when (fontp nx-unicode-font)
          (set-fontset-font t 'unicode nx-unicode-font frame))
        (when (fontp nx-variable-pitch-font)
          (set-face-attribute 'variable-pitch frame :font nx-variable-pitch-font)))
    ('error
     (lwarn 'nx-ui :error
            "Failed to set fonts because %s"
            (error-message-string ex))))
  (dolist (hook nx-after-init-ui-hook)
    (run-hook-wrapped hook #'nx--try-run-hook hook)))
(add-hook 'nx-post-init-hook #'nx|init-ui)
(add-hook 'after-make-frame-functions #'nx|init-ui)

(defun nx|run-after-make-frame-hooks (frame)
  "Run configured hooks in response to the newly created FRAME."
  (with-selected-frame frame
    (dolist (hook (if window-system
		      nx-after-make-system-frame-hook
		    nx-after-make-console-frame-hook))
      (run-hook-wrapped hook #'nx--try-run-hook hook))))
(add-hook 'after-make-frame-functions #'nx|run-after-make-frame-hooks)

(defun nx|run-after-make-initial-frame-hooks ()
  "Run configured hooks on the initial frame."
  (nx|run-after-make-frame-hooks nx--initial-frame))
(add-hook 'nx-post-init-hook #'nx|run-after-make-initial-frame-hooks)

(defun nx|disable-line-spacing ()
  "Turn off line spacing."
  (setq line-spacing 0))
(add-hook 'term-mode-hook #'nx|disable-line-spacing)

(autoload 'mwheel-install "mwheel")
(defun nx|console-frame-setup ()
  "Setup a console frame."
  (xterm-mouse-mode 1)
  (mwheel-install))
(add-hook 'nx-after-make-console-frame-hook #'nx|console-frame-setup)

(use-package winner
  :demand t
  :init
  (setq winner-dont-bind-my-keys t)
  :config
  (add-hook 'nx-after-init-ui-hook #'winner-mode))

(use-package paren
  :demand t
  :init
  (setq show-paren-delay 0.2
        show-paren-highlight-openparen t
        show-paren-when-point-inside-paren t)
  (add-hook 'nx-after-init-ui-hook #'show-paren-mode))

(use-package frame
  :demand t
  :init
  (setq-default window-divider-default-places t
                window-divider-default-bottom-width 0
                window-divider-default-right-width 1)
  (add-hook 'nx-after-init-ui-hook #'window-divider-mode))

(defun nx|set-mode-name ()
  "Set the major mode's `mode-name' as seen in `nx-major-mode-names'."
  (-when-let (name (cdr (assq major-mode nx-major-mode-names)))
            (setq mode-name
                  (cond ((functionp name) (funcall name))
                        ((stringp name) name)
                        (t (error "'%s' isn't a valid name for %s" name major-mode))))))
(add-hook 'after-change-major-mode-hook #'nx|set-mode-name)

(use-package solaire-mode
  :ensure t
  :commands (solaire-mode turn-on-solaire-mode turn-off-solaire-mode)
  :init
  (defun +nx*reset-solaire-mode (&rest _) (solaire-mode-reset))

  ;; (setq solaire-mode-real-buffer-fn #'nx-real-buffer-p)
  (add-hook 'nx-after-init-ui-hook #'solaire-mode-swap-bg t)
  (add-hook 'after-change-major-mode-hook #'turn-on-solaire-mode)
  (add-hook 'nx-popup-mode-hook #'turn-off-solaire-mode)

  (advice-add #'load-theme :after #'+nx*reset-solaire-mode)
  (add-hook 'nx-after-init-ui-hook #'solaire-mode-reset))

(use-package ample-theme
  :ensure t)

(use-package hideshow
  :ensure t)

(use-package all-the-icons
  :ensure t)

(use-package unicode-fonts
  :ensure t
  :init
  (defun +unicode|init-fonts (&optional frame)
    "Initialize `unicode-fonts' (GUI only)."
    (with-selected-frame frame
      (require 'unicode-fonts)
      (unicode-fonts-setup)))
  (setq nx-unicode-font nil)
  (add-hook 'nx-post-init-hook
            (lambda ()
              (if initial-window-system
                  (+unicode|init-fonts nx--initial-frame)
                (add-hook 'nx-after-make-system-frame-hook #'+unicode|init-fonts)))))

(use-package hl-todo
  :ensure t
  :commands hl-todo-mode
  :init
  (setq hl-todo-keyword-faces `(("TODO"  . ,(face-foreground 'warning))
                                ("FIXME" . ,(face-foreground 'error))
                                ("NOTE"  . ,(face-foreground 'success))))
  (add-hook 'prog-mode-hook #'hl-todo-mode))

(use-package ace-window
  :ensure t
  :commands (ace-window ace-swap-window ace-delete-window
                        ace-select-window ace-delete-other-windows)
  :init
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)
        aw-scope 'frame
        aw-background t)
  :bind
  (([remap other-window] . ace-window)))

(use-package autoloads-buffers)

(provide 'init-ui)

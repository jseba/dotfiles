;;; config-interface.el

(defvar %theme nil
  "Default theme.")
(defvar %font (font-spec :family "Iosevka" :size 12)
  "Default font. Should be a FONT-SPEC.")

(setq-default
 ansi-color-for-comint-mode t
 bidi-display-reordering nil
 blink-matching-paren nil
 compilation-always-kill t
 compilation-ask-about-save nil
 compilation-scroll-output 'first-error
 confirm-nonexistent-file-or-buffer t
 cursor-in-non-selected-windows nil
 custom-theme-directory (concat %emacs-dir "themes/")
 display-line-numbers-width 3
 enable-recursive-minibuffers nil
 frame-inhibit-implied-resize t
 fringe-indicator-alist (delq (assq 'continuation fringe-indicator-alist)
                              fringe-indicator-alist)
 highlight-nonselected-windows nil
 image-animate-loop t
 indicate-buffer-boundaries nil
 indicate-empty-lines nil
 inhibit-compacting-font-caches t
 max-mini-window-height 0.3
 mode-line-default-help-echo nil
 mouse-yank-at-point t
 resize-mini-windows 'grow-only
 show-help-function nil
 split-width-threshold 160
 uniquify-buffer-name-style 'forward
 use-file-dialog nil
 use-dialog-box nil
 visible-cursor nil
 x-stretch-cursor nil
 ring-bell-function #'ignore
 visible-bell nil
 window-resize-pixelwise t
 frame-resize-pixelwise t)

(defun silence-motion-errors (orig-fn &rest args)
  (if (not (minibufferp))
      (apply orig-fn args)
    (ignore-errors (apply orig-fn args))
    (when (<= (point) (minibuffer-prompt-end))
      (goto-char (minibuffer-prompt-end)))))
(advice-add #'left-char :around #'silence-motion-errors)
(advice-add #'right-char :around #'silence-motion-errors)
(advice-add #'delete-backward-char :around #'silence-motion-errors)
(advice-add #'backward-kill-sentence :around #'silence-motion-errors)

(use-package hide-mode-line
  :init
  (add-hook 'completion-list-mode-hook #'hide-mode-line-mode)
  (add-hook 'Man-mode-hook #'hide-mode-line-mode))

(use-package avy
  :init
  (setq avy-all-windows nil
        avy-background t))

(use-package doom-themes
  :init
  (setq %theme 'doom-one)
  :config
  (after! org
    (doom-themes-org-config)))

(use-package solaire-mode
  :init
  (defvar +solaire-themes
    '((doom-city-lights . t)
      (doom-dracula . t)
      (doom-molokai . t)
      (doom-nord . t)
      (doom-nord-light . t)
      (doom-nova)
      (doom-one . t)
      (doom-one-light . t)
      (doom-opera . t)
      (doom-solarized-light)
      (doom-spacegrey)
      (doom-vibrant)
      (doom-tomorrow-night)
      (doom-challenger-deep . t))
    "A list of themes supporting `solaire-mode'.")

  (defun +solaire-swap-bg-maybe ()
    (when-let* ((rule (assq %theme +solaire-themes)))
      (require 'solaire-mode)
      (if (cdr rule) (solaire-mode-swap-bg))))
  (add-hook 'load-theme-hook #'+solaire-swap-bg-maybe t)

  :config
  (add-hook 'change-major-mode-hook #'turn-on-solaire-mode)
  (add-hook 'focus-in-hook #'solaire-mode-reset)
  (add-hook 'load-theme-hook #'solaire-mode-reset t)
  (add-hook 'org-capture-mode-hook #'turn-on-solaire-mode))

(use-package ace-window
  :defer t
  :init
  (define-key global-map [remap other-window] #'ace-window)
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)
        aw-scope 'frame
        aw-background t))

(use-package hl-todo
  :hook (prog-mode . hl-todo-mode)
  :config
  (setq hl-todo-keyword-faces
        `(("TODO"  . ,(face-foreground 'warning))
          ("FIXME" . ,(face-foreground 'error))
          ("NOTE"  . ,(face-foreground 'success)))))

(use-package restart-emacs
  :init
  (setq restart-emacs--args '("--restore")))

(use-package visual-fill-column
  :init
  (setq visual-fill-column-center-text t
        visual-fill-column-width
        (+ (if (boundp 'display-line-numbers) 6 0)
           fill-column)))

(use-package posframe)

(use-package winner
  :defer 1
  :preface
  (defvar winner-dont-bind-my-keys t)
  :config
  (winner-mode +1))

(use-package paren
  :after-call after-find-file
  :init
  (defun disable-show-paren-mode ()
    (set (make-local-variable 'show-paren-mode) nil))
  :config
  (setq show-paren-delay 0.1
        show-paren-highlight-openparen t
        show-paren-when-point-inside-paren t)
  (show-paren-mode +1))

;;
;; Window Divider mode
(setq-default window-divider-default-places t
              window-divider-default-bottom-width 1
              window-divider-default-right-width 1)
(add-hook 'init-ui-hook #'window-divider-mode)

;;
;; Whitespace mode
(setq whitespace-line-column nil
      whitespace-style '(face
                         indentation
                         tabs
                         tab-mark
                         spaces
                         space-mark
                         newline
                         newline-mark
                         trailing
                         lines-tail)
      whitespace-display-mappings '((tab-mark ?\t [?> ?\t])
                                    (newline-mark ?\n [?- ?\n])
                                    (space-mark ?\ [?.] [?.])))

;;
;; Fringes
(defun disable-fringes-in-minibuffer (&rest _)
  (set-window-fringes (minibuffer-window) 0 0 nil))
(add-hook! '(init-ui-hook minibuffer-setup-hook window-configuration-change-hook)
  #'disable-fringes-in-minibuffer)

(after! which-key
  (defun disable-fringes-in-which-key-buffer (&rest _)
    (disable-fringes-in-minibuffer)
    (set-window-fringes (get-buffer-window which-key--buffer) 0 0 nil))
  (advice-add #'which-key--show-buffer-side-window
              :after #'disable-fringes-in-which-key-buffer))

;;
;; Load theme/font
(defun setup-fonts ()
  (condition-case ex
      (progn
        (when (fontp %font)
          (let ((xlfd (font-xlfd-name %font)))
            (add-to-list 'default-frame-alist (cons 'font xlfd)))))
    ((debug error)
     (if (string-prefix-p "Font not available: " (error-message-string ex))
         (lwarn 'config-interface :warning
                "Could not find the '%s' font on your system, falling back to system font"
                (font-get (caddr ex) :family))
       (lwarn 'config-interface :error
              "Unexpected error while initializing fonts: %s"
              (error-message-string ex))))))

(defun setup-theme ()
  (when (and %theme (not (memq %theme custom-enabled-themes)))
    (load-theme %theme t)))

(defvar last-window-system (if (daemonp) 'daemon initial-window-system))

(defun reload-theme-in-frame-maybe (frame)
  (when (and (framep frame)
             (not (eq last-window-system (framep-on-display frame))))
    (with-selected-frame frame
      (load-theme %theme t))
    (setq last-window-system (framep-on-display frame))))

(defun reload-theme-maybe (_frame)
  (unless (cl-find last-window-system (frame-list) :key #'framep-on-display)
    (setq last-window-system)
    (reload-theme-in-frame (selected-frame))))

(defun switch-theme (theme)
  (interactive
   (list (completing-read
          "Load theme: "
          (mapcar #'symbol-name
                  (custom-available-themes)))))
  (condition-case ex
      (progn
        (mapc #'disable-theme custom-enabled-themes)
        (load-theme (intern theme) t))
    (error "Problem loading theme: %s" x)))

(defun run-load-theme-hooks (theme &rest _)
  (setq %theme theme)
  (run-hooks 'load-theme-hook))
(advice-add #'load-theme :after #'run-load-theme-hooks)

(add-hook! 'init-ui-hook #'(setup-fonts setup-theme blink-cursor-mode))
(setq-hook! 'init-ui-hook frame-title-format "%b - Emacs")

(add-hook 'after-make-frame-functions #'reload-theme-in-frame-maybe)
(add-hook 'after-delete-frame-functions #'reload-theme-maybe)

;; TODO: remove this?
(add-hook! (prog-mode text-mode conf-mode) #'display-line-numbers-mode)

(defun apply-ansi-color-to-compilation-buffer ()
  (with-silent-modifications
    (ansi-color-apply-on-region compilation-filter-start (point))))

(defun setup-ui ()
  (add-hook 'compilation-filter-hook #'apply-ansi-color-to-compilation-buffer)
  (run-hook-wrapped 'init-ui-hook #'try-run-hook))
(add-hook 'init-hook #'setup-ui t)

(provide 'config-interface)
;;; config-interface.el ends here

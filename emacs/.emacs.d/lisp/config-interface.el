;;; config-interface.el

(defvar %theme nil
  "Default theme.")
(defvar %font (font-spec :family "Fira Code Retina" :size 12)
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

(use-package ace-window
  :defer t
  :bind
  (([remap other-window] . ace-window))
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
          ("NOTE"  . ,(face-foreground 'success))
          ("XXX"   . ,(face-foreground 'shadow)))))

(use-package visual-fill-column
  :init
  (setq visual-fill-column-center-text t
        visual-fill-column-width
        (+ (if (boundp 'display-line-numbers) 6 0)
           fill-column)))

(use-feature winner
  :defer 1
  :preface
  (defvar winner-dont-bind-my-keys t)
  :config
  (winner-mode +1))

(use-feature paren
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
;; Fancy mode-line
(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :init
  (setq doom-modeline-buffer-file-name-style 'relative-from-project
	doom-modeline-icon (display-graphic-p)
	doom-modeline-major-mode-icon nil)
  :config
  (add-hook 'doom-modeline-mode-hook #'size-indication-mode)
  (add-hook 'doom-modeline-mode-hook #'column-number-mode)
  (add-hook 'load-theme-hook #'doom-modeline-refresh-bars)
  
  (doom-modeline-def-modeline 'main
    '(bar matches buffer-info remote-host buffer-position selection-info)
    '(misc-info debug lsp buffer-encoding major-mode process checker)))

(use-package doom-themes
  :init
  (setq %theme 'doom-one
        doom-themes-enable-bold t))

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

(defun disable-bold-faces ()
  (mapc (lambda (face)
          (set-face-attribute face nil :weight 'normal))
        (face-list)))
(add-hook 'minibuffer-setup-hook #'disable-bold-faces)
(add-hook 'change-major-mode-after-body-hook #'disable-bold-faces)

;;
;; Load theme/font
(defun setup-fonts ()
  (condition-case ex
      (progn
        (cond (%font
	           (add-to-list
		        'default-frame-alist
		        (cons
		         'font
		         (cond ((stringp %font) %font)
		               ((fontp %font) (font-xlfd-name %font))
		               ((signal 'wrong-type-argument (list '(fontp stringp) %font)))))))
	          ((display-graphic-p)
	           (setq %font (face-attribute 'default :font)))))
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

(defun apply-ansi-color-to-compilation-buffer ()
  (with-silent-modifications
    (ansi-color-apply-on-region compilation-filter-start (point))))
(add-hook 'compilation-filter-hook #'apply-ansi-color-to-compilation-buffer)

(defun setup-ui ()
  (run-hook-wrapped 'init-ui-hook #'try-run-hook))
(add-hook 'after-init-hook #'setup-ui)

(defun +split-window-vertically-other-buffer (&optional size)
  "Split the selected window vertically and switch to the new window."
  (interactive "P")
  (let ((target-window (split-window-vertically size)))
    (set-window-buffer target-window (other-window 1))
    (select-window target-window)))
(advice-add #'split-window-vertically :override #'+split-window-vertically-other-buffer)

(defun +split-window-horizontally-other-buffer (&optional size)
  "Split the selected window horizontally and switch to the new window."
  (interactive "P")
  (let ((target-window (split-window-horizontally size)))
    (set-window-buffer target-window (other-window 1))
    (select-window target-window)))
(advice-add #'split-window-horizontally :override #'+split-window-horizontally-other-buffer)

(defun maximize-window ()
  "Maximize and isolate the current window. Activate again to undo.

If the window changes before then, the undo expires."
  (interactive)
  (if (and (one-window-p)
           (assoc ?_ register-alist))
      (jump-to-register ?_)
    (window-configuration-to-register ?_)
    (delete-other-windows)))

(provide 'config-interface)
;;; config-interface.el ends here

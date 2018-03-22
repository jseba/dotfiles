;;; config-interface.el -- Interface settings

;;; Commentary:
;;; Code:

(defvar nx-after-make-console-frame-hook nil
  "A list of hooks to run after making a console frame.")
(defvar nx-after-make-system-frame-hook nil
  "A list of hooks to run after making a window system frame.")
(defvar nx-theme 'ample
  "Default theme.")
(defvar nx-font (font-spec :family "Source Code Pro" :size 12)
  "The default font. Should be a FONT-SPEC.")
(defvar nx-big-font (font-spec :family "Source Code Pro" :size 14)
  "The default big font. Should be a FONT-SPEC.")
(defvar nx-variable-pitch-font nil
  "The default font to use for variable-pitch text. Should be a FONT-SPEC.")
(defvar nx-unicode-font nil
  "The default font to use for unicode text. Should be a FONT-SPEC.")
(defvar nx-major-mode-names '((emacs-lisp-mode . "Elisp"))
  "An alist mapping major modes symbols to strings or functions that will return a string).")
(defconst nx--initial-frame (selected-frame)
  "The frame (if any) active during Emacs initialization.")
(defconst nx--do-not-kill-buffer-names '("*scratch*" "*Messages*")
  "Names of buffers that should not be killed.")

(setq-default minibuffer-prompt-properties '(read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt)
              idle-update-delay 2
              bidi-display-reordering nil
              blink-matching-paren nil
              cursor-in-non-selected-windows nil
              display-line-numbers-width 3
              frame-inhibit-implied-resize t)

(setq use-file-dialog nil
      use-dialog-box nil
      frame-title-format '("" invocation-name " - "
			               (:eval (if (buffer-file-name)
                                      (abbreviate-file-name (buffer-file-name "%b")))))
      view-read-only t
      help-window-select t
      display-buffer-alist
      ;; magit status window in a fullscreen window
      `((,(rx "*magit: ")
         (display-buffer-fullframe)
         (reusable-frames . nil))
        ;; Put REPLs and error lists into the bottom side window
        (,(rx bos
              (or "*Help"
                  "*Warnings*"
                  "*Compile-Log*"
                  "*compilation*"
                  "*Flycheck errors*"
                  "*shell"
                  "*sbt"
                  "*ensime-update*"
                  "*SQL"
                  "*Cargo"
                  (and (1+ nonl) " output*")))
         (display-buffer-reuse-window
          display-buffer-in-side-window)
         (side . bottom)
         (reusable-frames . visible)
         (window-height . 0.33))
        ("." nil (reusable-frames . visible))))

(let ((no-border '(internal-border-width . 0)))
  (add-to-list 'default-frame-alist no-border)
  (add-to-list 'initial-frame-alist no-border))

(defun nx|init-ui (&optional frame)
  "Set the theme and load the font on FRAME (in that order)."
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
            (error-message-string ex)))))

(defun nx|run-after-make-frame-hooks (frame)
  "Run configured hooks in response to the newly created FRAME."
  (with-selected-frame frame
    (dolist (hook (if window-system
		              nx-after-make-system-frame-hook
		            nx-after-make-console-frame-hook))
      (run-hook-wrapped hook #'nx--try-run-hook hook))))

(defun nx|run-after-make-initial-frame-hooks ()
  "Run configured hooks on the initial frame."
  (nx|run-after-make-frame-hooks nx--initial-frame))

(defun nx|disable-line-spacing ()
  "Turn off line spacing."
  (setq line-spacing 0))

(autoload 'mwheel-install "mwheel")
(defun nx|console-frame-setup ()
  "Setup a console frame."
  (xterm-mouse-mode 1)
  (mwheel-install))

(defun nx|set-mode-name ()
  "Set the major mode's `mode-name' as seen in `nx-major-mode-names'."
  (-when-let (name (cdr (assq major-mode nx-major-mode-names)))
    (setq mode-name
          (cond ((functionp name) (funcall name))
                ((stringp name) name)
                (t (error "'%s' isn't a valid name for %s" name major-mode))))))

(defun nx|clear-background-term ()
  "Clear terminal background."
  (unless (display-graphic-p (selected-frame))
    (set-face-background 'default "unspecified-bg" (selected-frame))))

(defun nx/toggle-fullscreen ()
  "Toggle fullscreen Emacs."
  (interactive)
  (set-frame-parameter
   nil
   'fullscreen
   (unless (frame-parameter nil 'fullscreen)
     'fullboth)))

(defun nx/toggle-line-numbers (&optional arg)
  "Toggle `linum-mode'.

ARG controls the package used to display line numbers."
  (interactive "P")
  (cond ((boundp 'display-line-numbers)
	     (setq display-line-numbers
	           (pcase arg
		         ('(4) 'relative)
		         (1 t)
		         (-1 nil)
		         (_ (not display-line-numbers)))))
	    ((featurep 'nlinum)
	     (nlinum-mode (or arg (if nlinum-mode -1 +1))))
	    (t
	     (error "No line number plugin detected"))))

(defun nx-resize-window (window new-size &optional horizontal force-p)
  "Resize WINDOW to NEW-SIZE. If HORIZONTAL, do it width-wise.

If FORCE-P is non-nil, do it even if the window is a fixed size."
  (with-selected-window (or window (selected-window))
    (let ((window-size-fixed (unless force-p window-size-fixed)))
      (enlarge-window (- new-size (if horizontal (window-width) (window-height)))
		              horizontal))))

(defun nx/window-zoom ()
  "Maximize and isolate the current buffer. Activate again to undo.

If the window changes before then, the undo expires."
  (interactive)
  (if (and (one-window-p)
	       (assoc ?_ register-alist))
      (jump-to-register ?_)
    (window-configuration-to-register ?_)
    (delete-other-windows)))

(defvar nx--window-enlargened-p nil)
(defun nx/window-enlargen ()
  "Enlargen the current window. Activate again to undo."
  (interactive)
  (setq nx--window-enlargened-p
	    (if (and nx--window-enlargened-p
		         (assoc ?_ register-alist))
	        (ignore (jump-to-register ?_))
	      (window-configuration-to-register ?_)
	      (nx-resize-window nil (truncate (/ (frame-width) 1.2)) t)
	      (nx-resize-window nil (truncate (/ (frame-heigth) 1.2)))
	      t)))

(defun nx/delete-frame ()
  "Delete the current-frame, but confirm if it isn't empty."
  (interactive)
  (if (cdr (frame-alist))
      (when (nx-quit-p "Close frame?")
	    (delete-frame))
    (save-buffers-kill-emacs)))

(defun nx/split-window-horizontally-with-other-buffer (&optional arg)
  "Split this window horizontally and switch to the new window unless ARG is provided."
  (interactive "P")
  (split-window-horizontally)
  (let ((target-window (next-window)))
    (set-window-buffer target-window (other-window 1))
    (unless arg
      (select-window target-window))))

(defun nx/split-window-vertically-with-other-buffer (&optional arg)
  "Split this window vertically and switch to the new window unless ARG is provided."
  (interactive "P")
  (split-window-vertically)
  (let ((target-window (next-window)))
    (set-window-buffer target-window (other-buffer 1))
    (unless arg
      (select-window target-window))))

(defun nx/toggle-delete-other-windows ()
  "Delete other windows in frame (if any) or restore previous window config."
  (interactive)
  (if (and winner-mode
	       (equal (selected-window) (next-window)))
      (winner-undo)
    (delete-other-windows)))

(defun nx/split-window-horizontally-instead ()
  "Change window split to horizontal."
  (interactive)
  (save-excursion
    (delete-other-windows)
    (nx/split-window-horizontally-with-other-buffer)))

(defun nx/split-window-vertically-instead ()
  "Change window split to vertical."
  (interactive)
  (save-excursion
    (delete-other-windows)
    (nx/split-window-vertically-with-other-buffer)))

(defun nx/split-window ()
  "Split the window to see the most recent buffer in the other window.
Call a second time to restore the orignal window configuration."
  (interactive)
  (if (eq last-command 'nx/split-window)
      (progn
	    (jump-to-register :nx/split-window)
	    (setq this-command 'nx/unsplit-window))
    (window-configuration-to-register :nx/split-window)
    (switch-to-buffer-other-window nil)))

(defun nx/toggle-current-window-dedication ()
  "Toggle whether the current window is dedicated to its current buffer."
  (interactive)
  (let* ((window (selected-window))
	     (was-dedicated (window-dedicated-p window)))
    (set-window-dedicated-p window (not was-dedicated))
    (message "Window %sdedicated to %s"
	         (if was-dedicated "no longer " "")
	         (buffer-name))))

(define-minor-mode nx-big-font-mode
  "A global mdoe that resizes the font."
  :init-value nil
  :lighter " BIG"
  :global t
  (unless (fontp nx-big-font)
    (user-error "`nx-big-font is not set to a valid font"))
  (if nx-big-font-mode
      (set-frame-font nx-big-font t t)
    (set-frame-font nx-font t t)))

(defun nx|maybe-adjust-visual-fill-column ()
  "Readjust visual fill column when the global font size is modified."
  (if visual-fill-column-mode
	  (add-hook 'after-setting-font-hook 'visual-fill-column--adjust-window nil t)
    (remove-hook 'after-setting-font-hook 'visual-fill-column--adjust-window t)))

(defun nx|do-not-kill-important-buffers ()
  "Inhibit killing of important buffers."
  (if (not (member (buffer-name) nx--do-not-kill-buffer-names))
	  t
    (message "Not allowed to kill %s, burying instead" (buffer-name))
    (bury-buffer)
    nil))

(defun lunaryorn/kill-this-buffer ()
  "Kill the current buffer."
  (interactive)
  (kill-buffer (current-buffer)))

(add-hook 'nx-post-init-hook #'nx|init-ui)
(add-hook 'after-make-frame-functions #'nx|init-ui)
(add-hook 'after-make-frame-functions #'nx|run-after-make-frame-hooks)
(add-hook 'nx-post-init-hook #'nx|run-after-make-initial-frame-hooks)
(add-hook 'nx-after-make-console-frame-hook #'nx|console-frame-setup)
(add-hook 'nx-after-make-console-frame-hook #'nx|clear-background-term)
(add-hook 'term-mode-hook #'nx|disable-line-spacing)
(add-hook 'after-change-major-mode-hook #'nx|set-mode-name)
(add-hook 'visual-fill-column-mode-hook #'nx|maybe-adjust-visual-fill-column)
(add-hook 'kill-buffer-query-functions #'nx|do-not-kill-important-buffers)

(use-package winner
  :demand t
  :config
  (setq winner-dont-bind-my-keys t)
  (add-hook 'nx-post-init-hook #'winner-mode))

(use-package paren
  :demand t
  :config
  (setq show-paren-delay 0.2
        show-paren-highlight-openparen t
        show-paren-when-point-inside-paren t)
  (add-hook 'nx-post-init-hook #'show-paren-mode))

(use-package frame
  :demand t
  :config
  (setq-default window-divider-default-places t
                window-divider-default-bottom-width 0
                window-divider-default-right-width 1)
  (add-hook 'nx-post-init-hook #'window-divider-mode))

(use-package windmove
  :demand t
  :bind
  (([remap split-window-vertically] . nx/split-window-vertically-with-other-buffer)
   ([remap split-window-horizontally] . nx/split-window-horizontally-with-other-buffer)
   ([remap delete-other-windows] . nx/toggle-delete-other-windows)
   ("C-c w |" . nx/split-window-horizontally-instead)
   ("C-c w _" . nx/split-window-vertically-instead)
   ("C-c w S" . nx/split-window)
   ("C-c w d" . nx/toggle-current-window-dedication)))

(use-package uniquify
  :demand t
  :config
  (setq uniquify-buffer-name-style 'forward))

(use-package ibuffer
  :init
  (defun +ibuffer|set-up-preferred-filters ()
    (ibuffer-vc-set-filter-groups-by-vc-root)
    (unless (eq ibuffer-sorting-mode 'filename/process)
      (ibuffer-do-sort-by-filename/process)))
  (add-hook 'ibuffer-hook #'+ibuffer|set-up-preferred-filters)
  ;; show version control status in ibuffer
  (setq ibuffer-formats
        '((mark modified read-only vc-status-mini " "
                (name 18 18 :left :elide)
                " "
                (size 9 -1 :right)
                " "
                (mode 16 16 :left :elide)
                " "
                (vc-status 16 16 :left)
                " "
                filename-and-process)
          (mark modified read-only " "
                (name 18 18 :left :elide)
                " "
                (size 9 -1 :right)
                " "
                (mode 16 16 :left :elide)
                " "
                filename-and-process)
          (mark " " (name 16 -1) " " filename))
	    ibuffer-filter-group-face 'font-lock-doc-face)
  (setq-default ibuffer-show-empty-filter-groups nil)
  :config
  (define-ibuffer-column size-h
    (:name "Size" :inline t)
    (cond
     ((> (buffer-size) 1000000) (format "%7.1fM" (/ (buffer-size) 1000000.0)))
     ((> (buffer-size) 1000) (format "%7.1fk" (/ (buffer-size) 1000.0)))
     (t (format ("%8d" (buffer-size))))))
  :bind
  (([remap list-buffers] . ibuffer)))

(use-package ibuffer-vc
  :ensure t
  :init
  (defun +ibuffer-vc|group-by-project-and-status ()
    "Group buffers by project and status."
    (ibuffer-vc-set-filter-groups-by-vc-root)
    (unless (eq ibuffer-sorting-mode 'alphabetic)
      (ibuffer-do-sort-by-alphabetic)))
  (add-hook  'ibuffer-hook #'ibuffer-vc|group-by-project-and-status))


(use-package hideshow
  :diminish hs-minor-mode)

(use-package default-text-scale
  :ensure t
  :init
  (add-hook 'nx-post-init-hook #'default-text-scale-mode))

(use-package visual-fill-column
  :ensure t
  :init
  (add-hook 'nx-post-init-hook #'visual-fill-column-mode))

(use-package ample-theme
  :ensure t)

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

(use-package telephone-line
  :ensure t
  :init
  (add-hook 'nx-post-init-hook #'telephone-line-mode))

(use-package flx
  :ensure t)

(provide 'config-interface)
;;; config-interface.el ends here

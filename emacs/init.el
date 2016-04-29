; Helper functions
(defun ensure-package-installed (&rest packages)
  "Ensures that all specified packages are installed and prompts to install if not.
   Returns a list of all packages installed this way or nil for skipped/already installed."

  (mapcar
    (lambda (package)
      (if (package-installed-p package)
        nil
        (if (y-or-n-p (format "Package %s is not installed. Install it? " package))
          (package-install package)
          package)))
	packages))

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


; Setup basic editing
(setq mode-require-final-newline t)              ;; make sure the file ends in '\n'
(setq-default tab-width 4)                       ;; use 4 spaces for a tab character
(set-terminal-coding-system 'utf-8)              ;; use UTF-8 by default
(set-keyboard-coding-system 'utf-8)
(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)
(setq-default indent-tabs-mode nil)              ;; don't replace spaces with tabs when formatting
(delete-selection-mode)                          ;; when typing over a selected region, delete then insert
(global-set-key (kbd "RET") 'newline-and-indent) ;; automatically indent on newline

; Diff mode
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

; Setup package manager
(require 'package)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))
(setq package-enable-at-startup nil)
(or (file-exists-p package-user-dir)
    (package-refresh-contents))
(package-initialize)

; Specify packages
(ensure-package-installed 'evil 'magit 'helm 'base16-theme)

; Setup evil-mode
; TODO: remove this at some point?
(require 'evil)
(evil-mode t)

; Setup interface
(set-face-attribute 'default t :font "Source Code Pro 9")
(set-face-attribute 'default nil :font "Source Code Pro 9")
(load-theme 'base16-atelierforest-dark t)
(if window-system
  (progn
    (tool-bar-mode 0)
    (menu-bar-mode 0)
    (scroll-bar-mode 0)
  )
)

; Setup keymappings
(global-set-key [remap move-beginning-of-line]
                'smarter-move-beginning-of-line)
(global-set-key (kbd "C-a") 'smarter-move-beginning-of-line)

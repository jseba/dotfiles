;;; core/init-popups.el -*- lexical-binding: t; -*-

(defvar nx-popup-history nil
  "A list of popups that were last closed.")
(defvar nx-popup-other-window nil
  "The last window selected before a popup was opened.")
(defvar nx-popup-no-fringes t
  "If non-nil, disable fringes in popup windows.")
(defvar nx-popup-windows ()
  "A list of open popup windows.")
(defvar-local nx-popup-rules nil
  "The `shackle' rule that caused this buffer to be recognized
as a popup.")
(defvar nx-popup-window-parameters
  '(:noexec :modeline :autokill :autoclose :autofit :static)
  "A list of window parameters that are set (and cleared) on
entering/leaving `nx-popup-mode'.")
(defvar nx-popup-remember-history t
  "If non-nil, remember the last popup(s) open in `nx-popup-history'.")
(defvar nx-popup-inhibit-autokill nil
  "If non-nil no buffers will be killed when their popup windows are closed.
This overrides their `:autokill' property.")

(defun nx--popup-rule-add (&rest rules)
  "Prepend a new popup rule to `shackle-rules'."
  (if (cl-every #'listp (mapcar #'nx-unquote rules))
      (setq shackle-rules (nconc (list rules) shackle-rules))
    (push (list rules) shackle-rules)))

(use-package shackle
  :demand t
  :config
  (setq shackle-default-alignment 'below
	shackle-default-size 8
	shackle-rules
        '(("^\\*eww" :regexp t :size 0.5 :select t :autokill t :noesc t)
          ("^\\*ftp " :noselect t :autokill t :noesc t)
          ("^ ?\\*doom " :regexp t :noselect t :autokill t :autoclose t :autofit t)
          ;; built-in (emacs)
          ("*compilation*" :size 0.25 :noselect t :autokill t :autoclose t)
          ("*ert*" :same t :modeline t)
          ("*info*" :size 0.5 :select t :autokill t)
          ("*Backtrace*" :size 20 :noselect t)
          ("*Warnings*"  :size 12 :noselect t :autofit t)
          ("*Messages*"  :size 12 :noselect t)
          ("*Help*" :size 0.3)
          ("^\\*.*Shell Command.*\\*$" :regexp t :size 20 :noselect t :autokill t)
          (apropos-mode :size 0.3 :autokill t :autoclose t)
          (Buffer-menu-mode :size 20 :autokill t)
          (comint-mode :noesc t)
          (grep-mode :size 25 :noselect t :autokill t)
          (profiler-report-mode :size 0.3 :regexp t :autokill t :modeline minimal)
          (tabulated-list-mode :noesc t)
	  ("^ ?\\*" :regexp t :size 15 :noselect t :autokill t :autoclose t)))
  (add-hook 'nx-init-hook #'shackle-mode)
  (add-hook 'nx-popup-mode-hook #'nx|hide-modeline-in-popup)
  
  (advice-add #'shackle--match :filter-return #'nx*shackle-always-align)
  (advice-add #'shackle-display-buffer :around #'nx*popup-init)
  (advice-add #'balance-windows :around #'nx*popups-save)
  (advice-add #'delete-window :before #nx*delete-popup-window)

  (dolist (param `(popup ,@nx-popup-window-param))
    (push (cons param 'writable) window-persistent-parameters))

  (defvar nx-popup-mode-map
    (let ((map (make-sparse-keymap)))
      (define-key map [escape]    #'doom/popup-close-maybe)
      (define-key map (kbd "ESC") #'doom/popup-close-maybe)
      (define-key map [remap quit-window] #'doom/popup-close-maybe)
      (define-key map [remap doom/kill-this-buffer] #'delete-window)
      (define-key map [remap split-window-right] #'ignore)
      (define-key map [remap split-window-below] #'ignore)
      (define-key map [remap split-window-horizontally] #'ignore)
      (define-key map [remap split-window-vertically] #'ignore)
      (define-key map [remap mouse-split-window-horizontally] #'ignore)
      (define-key map [remap mouse-split-window-vertically] #'ignore)
      map)
    "Active keymap in popup windows."))

(use-package nx-popups
  :load-path "lisp"
  :defer t)

(defun nx*suppress-pop-to-buffer-same-window (orig-fn &rest args)
  (cl-letf (((symbol-function 'pop-to-buffer-same-window)
	     (symbol-function 'pop-to-buffer)))
    (apply orig-fn args)))
(advice-add #'info :around #'nx*suppress-pop-to-buffer-same-window)
(advice-add #'eww :around #'nx*suppress-pop-to-buffer-same-window)
(advice-add #'eww-browse-url :around #'nx*suppress-pop-to-buffer-same-window)

(defun nx*popup-buffer-menu (&optional arg)
  "Open `buffer-menu' in a popup window."
  (interactive "P")
  (with-selected-window (nx-popup-buffer (list-buffers-noselect arg))
    (setq mode-line-format "Commands: d, s, x, u; f, o, 1, 2, m, v; ~, %, q to quit; ?  for help.")))
(advice-add #'buffer-menu :override #'nx*popup-buffer-menu)

(provide 'init-popups)

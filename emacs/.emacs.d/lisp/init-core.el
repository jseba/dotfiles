;;; lisp/init-core.el -*- lexical-binding: t; -*-

(defvar nx-debug-mode (or (getenv "DEBUG") init-file-debug)
  "If non-nil, make functions more verbose.")

;; Directories
(defvar nx-etc-dir (concat nx-emacs-dir "etc/")
  "Path to non-volatile storage.")
(defvar nx-cache-dir (concat nx-emacs-dir "cache/")
  "Path to volatile storage.")
(defvar nx-package-dir (concat nx-emacs-dir "elpa/")
  "Path to installed packages.")

;; Better hooks
(defvar nx-init-hook nil
  "A list of hooks to run when Emacs is initialized.")
(defvar nx-post-init-hook nil
  "A list of hooks to run after Emacs is initialized (after `nx-init-hook').")

(defvar nx-init-p nil
  "If non-nil, Emacs has been fully initialized.")

(defvar nx-custom-file (concat nx-etc-dir "custom.el")
  "The path to the custom.el file.")
(setq custom-file nx-custom-file)
(load custom-file t t)

;; Make startup more quiet
(setq inhibit-startup-screen t
      inhibit-startup-message t
      inhibit-startup-echo-area-message user-login-name
      inhibit-default-init t
      initial-major-mode 'fundamental-mode
      initial-scratch-message nil
      mode-line-format nil
      visible-bell nil)
(advice-add #'display-startup-echo-area-message :override #'ignore)

;; Remove unwanted UI elements early
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(when (fboundp 'menu-bar-mode)   (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode)   (tool-bar-mode -1))
(when (fboundp 'horizontal-scroll-bar-mode) (horizontal-scroll-bar-mode -1))

;; Basic quality of life improvement
(fset #'yes-or-no-p #'y-or-n-p)

(setq-default
 ad-redefinition-action 'accept
 apropos-do-all t
 compilation-always-kill t
 compilation-ask-about-save nil
 compilation-scroll-output t
 confirm-non-existent-file-or-buffer t
 enable-recursive-minibuffers nil
 debug-on-error nx-debug-mode
 history-length 500

 abbrev-file-name (concat nx-etc-dir "abbrev.el")
 pcache-directory (concat nx-cache-dir "pcache/")
 mc/list-file (concat nx-etc-dir "mc-lists.el")
 server-auth-dir (concat nx-cache-dir "server/")

 ;; UI
 minibuffer-prompt-properties '(read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt)
 idle-update-delay 2
 bidi-display-reordering nil
 blink-matching-paren nil
 cursor-in-non-selected-windows nil
 display-line-numbers-width 3
 frame-inhibit-implied-resize t)

(defun nx--try-run-hook (fn hook)
  "Runs a hook wrapped in a `condition-case-unless-debug' block in order to
provide more information in the error message and still provide for invoking
the debugger in debug mode."
  (condition-case-unless-debug ex
      (funcall fn)
    ('error
     (lwarn hook :error
	    "%s in '%s' -> %s"
	    (car ex) fn (error-message-string ex))))
  nil)

(defun nx|finalize ()
  "Finalize initialization."
  (unless nx-init-p
    (dolist (hook '(nx-init-hook nx-post-init-hook))
      (run-hook-wrapped hook #'nx--try-run-hook hook))
    (setq nx-init-p t))
  (setq gc-cons-threshold (* 16 1024 1024)
	gc-cons-percentage 0.1))
(add-hook 'emacs-startup-hook #'nx|finalize)

(provide 'init-core)

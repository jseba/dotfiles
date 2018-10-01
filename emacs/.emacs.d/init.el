;; init.el

(eval-when-compile
  (and (version< emacs-version "25")
	   (error "Detected Emacs %s; required version is 25.1 or higher"
			  emacs-version)))

(unless (bound-and-true-p early-init-file)
  (load (concat (file-name-directory load-file-name) "early-init")
		nil t))

(defvar %debug-mode (or (getenv "DEBUG") init-file-debug)
  "If non-nil, enable more verbose output.")

(defvar %emacs-dir (expand-file-name user-emacs-directory)
  "The path to the .emacs.d directory.")
(defvar %lisp-dir (concat %emacs-dir "lisp/")
  "Path to configuration files.")
(defvar %etc-dir (concat %emacs-dir "etc/")
  "Path to non-volatile storage.")
(defvar %var-dir (concat %emacs-dir "var/")
  "Path to volatile storage.")
(defvar %package-dir (concat %emacs-dir "elpa/")
  "Path to installed packages.")
(defvar %custom-file (concat %etc-dir "custom.el")
  "The path to the custom.el file.")

(eval-and-compile
  (defun require-init (feature)
    (load (concat %lisp-dir (format "%s" feature))))
  (defun try-run-hook (hook)
    (when %debug-mode
      (message "Running hook: %s" hook))
    (condition-case ex
        (funcall hook)
      ((debug error)
       (signal 'try-run-hook-error (list hook ex))))
    nil))

(let ((gc-cons-threshold (* 256 1024 1024))
      (gc-cons-percentage 1.0)
      (debug-on-error t)
      (byte-compile-debug t)
      file-name-handler-alist)
  (require-init 'config-core)
  (require-init 'config-util)
  (require-init 'config-packages)
  (require-init 'config-system)
  (require-init 'config-keybinds)
  (require-init 'config-projects)
  (require-init 'config-popups)
  (require-init 'config-buffers)
  (require-init 'config-evil)
  (require-init 'config-workspaces)
  (require-init 'config-editor)
  (require-init 'config-interface)
  (require-init 'config-modeline)
  (require-init 'config-helm)
  (require-init 'config-spell)
  (require-init 'config-lang)
  (require-init 'config-company)
  (require-init 'config-imenu)
  (require-init 'config-cc)
  (require-init 'config-vc)
  (require-init 'config-eshell)
  (require-init 'config-elisp)
  (require-init 'config-snippets)
  (require-init 'config-pdf)
  ;;(require-init 'config-org)

  (run-hook-wrapped 'init-hook #'try-run-hook))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (hl-todo ace-window highlight-quoted company-box company-prescient company-dict company-tng swiper-helm posframe helm-projectile helm-describe-modes helm-company helm-c-yasnippet helm-ag helm visual-fill-column restart-emacs rainbow-delimiters highlight-escape-sequences highlight-numbers solaire-mode doom-themes avy evil-anzu shrink-path hide-mode-line anzu all-the-icons persp-mode evil-visualstar which-key use-package projectile hydra evil-vimish-fold evil-numbers evil-matchit evil-indent-plus evil-escape evil-embrace evil-commentary evil-args))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(fixed-pitch ((t (:font "-*-Fira Code Retina-*-*-*-*-12-*-*-*-*-*-*-*")))))
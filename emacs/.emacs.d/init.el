;;; init.el -- JSeba's Emacs

;;; Commentary:
;;; Code:

(let ((minver "25.0"))
  (when (version< emacs-version minver)
    (error "Current Emacs is too old -- this config requires v%s or higher" minver)))

(setq user-full-name "Josh Seba"
      user-mail-address "sebajosh@outlook.com")

(defconst nx--is-linux (eq system-type 'gnu/linux))
(defconst nx--is-macos (eq system-type 'darwin))
(defconst nx--is-win32 (eq system-type 'windows-nt))

;; TODO: use more of this
(defvar nx-debug-mode (or (getenv "DEBUG") init-file-debug)
  "If non-nil, make functions more verbose.")

;; Directories
(defvar nx-emacs-dir (expand-file-name user-emacs-directory)
  "The path to the .emacs.d directory.")
(defvar nx-lisp-dir (concat nx-emacs-dir "lisp/")
  "The path to the configuration files.")
(defvar nx-etc-dir (concat nx-emacs-dir "etc/")
  "Path to non-volatile storage.")
(defvar nx-cache-dir (concat nx-emacs-dir "cache/")
  "Path to volatile storage.")
(defvar nx-package-dir (concat nx-emacs-dir "elpa/")
  "Path to installed packages.")
(defvar nx-custom-file (concat nx-etc-dir "custom.el")
  "The path to the custom.el file.")

(dolist (dir (list nx-etc-dir nx-cache-dir nx-package-dir)) (make-directory dir t))

(setq custom-file nx-custom-file)
(load custom-file t t)

;; Increase memory limits during init
(defvar nx--file-name-handler-alist file-name-handler-alist)
(setq gc-cons-threshold (* 384 1024 1024)
      gc-cons-percentage 0.6
      file-name-handler-alist nil
      byte-compile-warnings '(not free-vars unresolved noruntime lexical make-local))

(add-to-list 'load-path nx-lisp-dir)

(require 'core)
(require 'core-util)
(require 'core-packages)

(require 'config-core)
(require 'config-interface)
(require 'config-editor)
(require 'config-files)
(require 'config-ivy)
(require 'config-grep)
(require 'config-projectile)
(require 'config-company)
(require 'config-programming)
(require 'config-cc)
(require 'config-vc)
(require 'config-keybindings)

(provide 'init)
;;; init.el ends here

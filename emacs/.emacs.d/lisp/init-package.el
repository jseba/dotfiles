;;; core/init-package.el -*- lexical-binding: t; -*-

;; Scope packages by Emacs version to reduce bytecode incompatibility issues
(defconst nx-emacs-version (format "%s.%s"
                                   emacs-major-version
                                   emacs-minor-version)
  "Version string to append to package directory.")

(defconst nx--no-ssl-p (and (memq system-type '(windows-nt ms-dos))
                         (not (gnutls-available-p)))
  "Whether or not SSL/TLS is available.")

(defvar nx--refreshed-p nil)

;; Configure package.el and use-package.el
(setq load-prefer-newer nil
      package-user-dir (concat nx-package-dir nx-emacs-version "/")
      package-enable-at-startup nil
      use-package-always-defer t
      use-package-always-ensure nil
      use-package-debug nil
      use-package-verbose nx-debug-mode
      use-package-minimum-reported-time (if nx-debug-mode 0 0.1)

      byte-compile-verbose nx-debug-mode
      byte-compile-warnings '(not free-vars unresolved noruntime lexical make-local))

(add-to-list 'package-archives `("melpa" . ,(if nx--no-ssl-p
					       "http://melpa.org/packages/"
					     "https://melpa.org/packages/")))
(unless nx--no-ssl-p
  (setcdr (assoc "gnu" package-archives) "https://elpa.gnu.org/packages/"))

(require 'package)
(package-initialize) ;; TODO: manage load-path ourselves?
(unless (file-exists-p package-user-dir)
  (package-refresh-contents))
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (let ((inhibit-message t))
    (package-install 'use-package))
  (if (package-installed-p 'use-package)
      (message "Installed `use-package'")
    (error "Couldn't install `use-package'")))
(require 'use-package)

(provide 'init-package)

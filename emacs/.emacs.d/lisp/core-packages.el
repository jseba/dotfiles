;;; packages.el -- Package setup

;;; Commentary:
;;; Code:

(eval-and-compile
  (setq load-prefer-newer t
        package-user-dir (concat nx-package-dir emacs-version "/")
        package-enable-at-startup nil
        package--init-file-ensured t))

(require 'package)
(add-to-list 'package-archives `("melpa" . ,(if nx--no-ssl-p
                  "http://melpa.org/packages/"
                "https://melpa.org/packages/")))
(unless nx--no-ssl-p
  (setcdr (assoc "gnu" package-archives) "https://elpa.gnu.org/packages/"))

(package-initialize)
(unless (file-directory-p package-user-dir)
  (make-directory package-user-dir t)
  (package-refresh-contents))
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (let ((inhibit-message t))
    (package-install 'use-package)
    (unless (package-installed-p 'use-packge)
      (error "Couldn't install `use-package'"))))
(require 'use-package)
(setq use-package-always-ensure nil
      use-package-always-defer t
      use-package-debug nil
      use-package-verbose nx-debug-mode
      use-package-minimum-reported-time (if nx-debug-mode 0 0.1))

(defun recompile-packages ()
  "Recompile all packages."
  (interactive)
  (byte-recompile-directory package-user-dir nil 'force))

(provide 'core-packages)
;;; core-packages.el ends here

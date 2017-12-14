;; Setup package manager and use-package

(require 'package)

;; Install into separate package dirs for each Emacs version (prevents bytecode incompatibility)
(let ((versioned-package-dir (format "~/.emacs.d/elpa-%s.%s" emacs-major-version emacs-minor-version)))
  (setq package-user-dir versioned-package-dir))

;; Package repositories
(defconst sanityinc/no-ssl (and (memq system-type '(windows-nt ms-dos))
                                (not (gnutls-available-p))))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives
	     `("melpa" . ,(if sanityinc/no-ssl
			      "http://melpa.org/packages/"
			    "https://melpa.org/packages/")))
(unless sanityinc/no-ssl
  ;; Force SSL for GNU ELPA
  (setcdr (assoc "gnu" package-archives) "https://elpa.gnu.org/packages/"))

(setq package-enable-at-startup nil)
(or (file-exists-p package-user-dir)
    (package-refresh-contents))
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile (require 'use-package))
(require 'bind-key)
(require 'diminish)

(provide 'init-elpa)

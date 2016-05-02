; Setup package manager
(require 'package)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))
(setq package-enable-at-startup nil)
(or (file-exists-p package-user-dir)
    (package-refresh-contents))
(package-initialize)

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

(ensure-package-installed 'evil
                          'magit
                          'helm
                          'base16-theme
                          'volatile-highlights
                          'company
                          'smartparens
                          'undo-tree
                          'highlight-numbers
                          'golden-ratio
                          'flycheck
                          'flycheck-tip
			  'expand-region
                          )

(add-to-list 'load-path "~/.emacs.d/personal/")
(require 'setup-applications)
(require 'setup-convenience)
(require 'setup-editing)
(require 'setup-environment)
(require 'setup-faces-and-ui)
(require 'setup-files)
(require 'setup-helm)
(require 'setup-programming)

;; ; Helper functions


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



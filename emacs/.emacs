;(package-initialize)

(let ((minver "25.0"))
  (when (version< emacs-version minver)
    (error "Current Emacs is too old -- this config requires v%s or higher" minver)))

(add-to-list 'load-path "~/.emacs.d/config")
(add-to-list 'load-path "~/.emacs.d/site")

(require 'init-benchmarking)

;; Tweak garbage collection thresholds during startup
(let ((normal-gc-cons-threshold (* 20 1024 1024))
      (init-gc-cons-threshold (* 128 1024 1024)))
  (setq gc-cons-threshold init-gc-cons-threshold)
  (add-hook 'after-init-hook
	    (lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))

;; basic quality of life improvements
(fset 'yes-or-no-p #'y-or-n-p)
(fset 'display-startup-echo-area-message #'ignore)

;; Bootstrap config
(setq custom-file "~/.emacs.d/custom.el")
(require 'init-utils)
(require 'init-site) ;; _before_ ELPA/MELPA
(require 'init-elpa)

;; Set core interface settings as early as possible
(require 'init-frame-hooks)
(require 'init-gui-frames)
(require 'init-xterm)
(require 'init-themes)
(require 'init-isearch)
(require 'init-ibuffer)
(require 'init-dired)
(require 'init-files)
(require 'init-grep)
(require 'init-recentf)
(require 'init-smex)
(require 'init-ivy)
;(require 'init-helm)
(require 'init-projectile)
(require 'init-company)
(require 'init-windows)
(require 'init-sessions)
(require 'init-fonts)
(require 'init-mmm)
(require 'init-editing)
(require 'init-programming)
(require 'init-cc)
(require 'init-tags)
(require 'init-vc)

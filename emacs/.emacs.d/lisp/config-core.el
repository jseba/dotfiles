;;; config-core.el

(defvar init-hook nil
  "Hooks run after loading configuration files.")
(defvar init-ui-hook nil
  "Hooks run when the UI has been initialized.")
(defvar load-theme-hook nil
  "Hooks run after calling `load-theme'.")
(defvar global-escape-hook nil
  "Hooks run when `global-escape' is called.")
(defvar cleanup-hook nil)

(defconst %EMACS26+ (eval-when-compile
					  (not (version< emacs-version "26"))))
(defconst %EMACS27+ (eval-when-compile
					  (not (version< emacs-version "27"))))

(defconst %IS-LINUX (eq system-type 'gnu/linux))
(defconst %IS-MACOS (eq system-type 'darwin))
(defconst %IS-WIN32 (eq system-type 'windows-nt))

;;
;; Built-in libraries

(eval-when-compile (require 'cl-lib))
(require 'map)
(require 'subr-x)

(provide 'config-core)
;;; config-core.el ends here

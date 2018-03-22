;;; config-core.el -- Core configuration

;;; Commentary:
;;; Code:

(setq-default ad-redefinition-action 'accept
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
              server-auth-dir (concat nx-cache-dir "server/"))

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

(use-package dash
  :demand t
  :ensure t)

(use-package persistent-soft
  :demand t
  :ensure t)

(use-package diminish
  :demand t
  :ensure t)

(use-package bind-key
  :demand t
  :ensure t)

(provide 'config-core)
;;; config-core.el ends here

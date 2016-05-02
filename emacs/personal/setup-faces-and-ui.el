;;; setup-faces-and-ui --- Setup UI
;;; Commentary:
;;; Code:

;; Setup fonts
(set-face-attribute 'default t :font "Source Code Pro 9")
(set-face-attribute 'default nil :font "Source Code Pro 9")

;; Setup colors
(load-theme 'base16-atelierforest-dark t)

;; Disable tool-bars and such
(when window-system
  (if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
  (if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
  (if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
  (if (fboundp 'horizontal-scroll-bar-mode) (horizontal-scroll-bar-mode -1)))
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)
(size-indication-mode t)
(setq frame-title-format
      '("" invocation-name " - " (:eval (if (buffer-file-name)
                                                    (abbreviate-file-name (buffer-file-name))
                                          "%b"))))

;; Other UI tweaks
(setq initial-scratch-message ""
      visible-bell t)

;; Highlight numeric literals in code
(require 'highlight-numbers)
(add-hook 'prog-mode-hook 'highlight-numbers-mode)

(provide 'setup-faces-and-ui)
;;; setup-faces-and-ui.el ends here

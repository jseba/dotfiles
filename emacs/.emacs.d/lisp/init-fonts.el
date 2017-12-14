(use-package default-text-scale
  :ensure t
  :init
  (default-text-scale-mode))

(use-package visual-fill-column
  :ensure t
  :init
  (defun sanityinc/maybe-adjust-visual-fill-column ()
    "Readjust visual fill column when the global font size is modified.
This is helpful in writeroom-mode in particular."
    (if visual-fill-column-mode
	(add-hook 'after-setting-font-hook 'visual-fill-column--adjust-window nil t)
      (remove-hook 'after-setting-font-hook 'visual-fill-column--adjust-window t)))
  (add-hook 'visual-fill-column-mode-hook 'sanityinc/maybe-adjust-visual-fill-column))

(use-package font
  :init
  (set-face-attribute 'default nil
		      :family "Source Code Pro"
		      :height 100))

(provide 'init-fonts)

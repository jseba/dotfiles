;;; config-spell.el

(defvar-local +flyspell-check-immediately t
  "If non-nil, spellcheck the current buffer upon starting `flyspell-mode'.")

;;
;; `ispell'
(setq ispell-dictionary "english"
      ispell-list-command "--list"
      ispell-extra-args '("--dont-tex-check-comments"))

(after! ispell
  (when (equal (file-name-base ispell-program-name) "aspell")
    (add-to-list ispell-extra-args "--sug-mode=ultra")))

(use-package flyspell
  :ensure nil ;; built-in
  :init
  (defun +flyspell-check-immediately ()
    "Spellcheck the current buffer when `flyspell-mode' is enabled."
    (when (and flyspell-mode +flyspell-check-immediately)
      (flyspell-buffer)))
  (add-hook 'flyspell-mode-hook #'+flyspell-check-immediately))

(use-package flyspell-correct
  :after flyspell-correct-helm
  :commands (flyspell-correct-word-generic
             flyspell-correct-previous-word-generic)
  :config
  (require 'flyspell-correct-helm))

(use-package flyspell-correct-helm)

(provide 'config-spell)
;;; config-spell.el ends here

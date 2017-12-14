(use-package flyspell
  :demand t
  :commands flyspell-mode
  :config
  (setq ispell-program-name (executable-find "aspell")
        ispell-list-command "--list"
        ispell-extra-args '("--dont-tex-check-comments")))

(use-package flyspell-correct
  :ensure t
  :after (flyspell ivy)
  :commands (flyspell-correct-word-generic
             flyspell-correct-previous-word-generic)
  :config
  (require 'flyspell-correct-ivy))

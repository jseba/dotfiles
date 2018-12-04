;;; config-keybinds.el

(defun global-escape ()
  (interactive)
  (cond ((minibuffer-window-active-p (minibuffer-window))
         (abort-recursive-edit))
        ((cl-find-if #'funcall global-escape-hook))
        ((or defining-kbd-macro executing-kbd-macro) nil)
        ((keyboard-quit))))
(global-set-key [remap keyboard-quit] #'global-escape)

(use-package which-key
  :defer 1
  :after-call pre-command-hook
  :init
  (setq which-key-sort-order #'which-key-prefix-then-key-order
        which-key-sort-uppercase-first nil
        which-key-add-column-padding 1
        which-key-max-display-columns nil
        which-key-min-display-lines 6
        which-key-side-window-slot -10
        which-key-replacement-alist
        '(((nil . "Prefix Command")        . (nil . "prefix"))
          ((nil . "\\`\\?\\?\\'")          . (nil . "λ")) ;; Lambdas
          ((nil . "/body\\'")              . (nil . "|="));; Prettify hydra entry points
          ((nil . "<\\([[:alnum:]-]+\\)>") . (nil . "\\1"))
          (("up")         . ("↑"))
          (("right")      . ("→"))
          (("down")       . ("↓"))
          (("left")       . ("←"))
          (("DEL")        . ("⌫"))
          (("deletechar") . ("⌦"))
          (("RET")        . ("⏎"))
          (("TAB")        . ("⭾"))
          (("SPC")        . ("␣"))))
  :config
  (set-face-attribute 'which-key-local-map-description-face nil :weight 'bold)
  (which-key-setup-side-window-bottom)
  (setq-hook! 'which-key-init-buffer-hook line-spacing 3)

  (defun +which-key-no-fringes-in-buffer (&rest _)
    (set-window-fringes (get-buffer-window which-key--buffer) 0 0 nil))
  (advice-add 'which-key--show-buffer-side-window
              :after #'+which-key-no-fringes-in-buffer)

  (which-key-mode +1))

(use-package hydra)

(use-package general
  :defer 1)

(define-key universal-argument-map
  (kbd "SPC u") #'universal-argument-more)

(add-hook! tty-setup-hook (define-key input-decode-map
                            (kbd "TAB") [tab]))

(after! evil
  (evil-define-key* 'insert 'global
    "\C-a" #'backward-to-bol-or-indent
    "\C-e" #'forward-to-last-non-comment-or-eol)
  (define-key! evil-ex-completion-map
    "\C-s" #'helm-minibuffer-history
    "\C-a" #'move-beginning-of-line
    "\C-e" #'move-end-of-line
    "\C-b" #'backward-word
    "\C-f" #'forward-word))

(mapc (lambda (map)
        (define-key! map
          "\C-s" #'helm-minibuffer-history
          "\C-a" #'move-beginning-of-line
          "\C-w" #'backward-kill-word
          "\C-u" #'backward-kill-sentence
          "\C-b" #'backward-word
          "\C-f" #'forward-word
          "\C-z" (lambda! (ignore-errors
                            (call-interactively #'undo)))

          "\C-r" #'evil-paste-from-register
          "\C-j" #'next-line
          "\C-k" #'previous-line
          (kbd "C-S-j") #'scroll-up-command
          (kbd "C-S-k") #'scroll-down-command))
      (list minibuffer-local-map
            minibuffer-local-ns-map
            minibuffer-local-completion-map
            minibuffer-local-must-match-map
            minibuffer-local-isearch-map
            read-expression-map))

(provide 'config-keybinds)
;;; config-keybinds.el ends here

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
  :defer 1
  :after-call pre-command-hook
  :config
  (general-evil-setup)

  (general-create-definer
    general-leader-def
    :prefix "SPC")

  (general-create-definer
    general-local-leader-def
    :prefix "SPC m")

  ;;
  ;; Global keybindings

  (general-def
    :keymaps '(insert normal global visual motion emacs)
    "M-x" #'execute-extended-command
    "A-x" #'execute-extended-command)

  ;;
  ;; Evil keybindings

  (general-imap
   [remap newline] #'newline-and-indent
   "C-j" #'newline

   "C-SPC" #'+company-complete)

  (general-imap
    :prefix "C-x"
    "C-l" #'+company-whole-lines
    "C-o" #'company-capf
    "C-n" #'+company-dabbrev
    "C-p" #'+company-dabbrev-code-previous
    "s"   #'company-ispell
    "C-f" #'company-files
    "C-]" #'company-etags)

  (general-vmap
    "<" #'+evil-visual-dedent
    ">" #'+evil-visual-indent
    "S" #'evil-surround-region)

  (general-nmap
    "M-+" #'text-scale-increase
    "M-=" (lambda! (text-scale-set 0))
    "M--" #'text-scale-decrease
    "C-`" #'+popup-toggle
    "C-~" #'+popup-raise
    "M-t" #'+persp-new
    "M-TAB" #'+persp-display
    "M-w" #'delete-window
    "M-W" #'delete-frame
    "C-M-f" #'toggle-frame-fullscreen
    "M-n" #'evil-buffer-new
    "M-N" #'make-frame
    "M-1" (lambda! (+persp-switch-to 0))
    "M-2" (lambda! (+persp-switch-to 1))
    "M-3" (lambda! (+persp-switch-to 2))
    "M-4" (lambda! (+persp-switch-to 3))
    "M-5" (lambda! (+persp-switch-to 4))
    "M-6" (lambda! (+persp-switch-to 5))
    "M-7" (lambda! (+persp-switch-to 6))
    "M-8" (lambda! (+persp-switch-to 7))
    "M-9" (lambda! (+persp-switch-to 8))
    "M-0" (lambda! (+persp-switch-to-last))
    "gc"  #'evil-commentary)

  (general-nmap
    :keymaps 'evil-window-map
    "C-h" #'evil-window-left
    "C-j" #'evil-window-down
    "C-k" #'evil-window-up
    "C-l" #'evil-window-right
    "C-w" #'other-window
    "H"   #'+evil-window-move-left
    "J"   #'+evil-window-move-down
    "K"   #'+evil-window-move-up
    "L"   #'+evil-window-move-right
    "C-S-W" #'ace-swap-window
    "u"   #'winner-undo
    "C-u" #'winner-undo
    "C-r" #'winner-redo
    "c"   #'+persp-close-window-or-workspace
    "C-C" #'ace-delete-window)

  (general-nmap
    :keymaps '(help-mode-map helpful-mode-map)
    "o" #'ace-link-help
    "q" #'quit-window
    "Q" #'helm-resume
    "]l" #'forward-button
    "[l" #'backward-button)

  (general-nvmap
    "C-a" #'evil-numbers/inc-at-pt
    "C-z" #'evil-numbers/dec-at-pt)

  (general-omap
    "s" #'evil-surround-edit
    "S" #'evil-Surround-edit)

  (general-mmap
    "]e" #'next-error
    "[e" #'previous-error
    "]S" #'flyspell-correct-word-generic
    "[S" #'flyspell-correct-previous-word-generic
    "]t" #'hl-todo-next
    "[t" #'hl-todo-previous)

  ;;
  ;; Leader keybindings

  (general-nmap
    :prefix "SPC"
    "SPC" '(projectile-find-file
            :which-key "Find file in project")
    "."   '(find-file
            :which-key "Browse files")
    "~"   '(+popup-toggle
            :which-key "Toggle last popup")
    "'"   '(helm-resume
            :which-key "Resume last search")
    ";"   '(+smartparens-hydra/body
            :which-key "Sexps")
    "RET" '(bookmark-jump
            :which-key "Jump to bookmark")
    ","   '(persp-switch-to-buffer
            :which-key "Switch workspace buffer")
    "<"   '(switch-to-buffer
            :which-key "Switch buffer")
    "u"   '(universal-argument
            :which-key "Universal argument")
    "w"   '(evil-window-map
            :which-key "Window")
    [tab] '(:ignore t :which-key "Workspace"))

  (general-nvmap
    :prefix "SPC /"
    "b"   '(swiper-helm
            :which-key "Buffer")
    "p"   '(+helm-project-search
            :which-key "Project")
    "i"   '(imenu
            :which-key "Symbols")
    "I"   '(imenu-anywhere
            :which-key "Symbols across buffers"))

  (general-nmap
    :prefix "SPC TAB"
    ""    '(nil
            :which-key "Workspace")
    [tab] '(+persp-display
            :which-key "Show tab bar")
    "c"   '(+persp-new
            :which-key "New workspace")
    "l"   '(+persp-load
            :which-key "Load workspace from file")
    "L"   '(+persp-load-session
            :which-key "Load session from file")
    "s"   '(+persp-save
            :which-key "Save session to file")
    "S"   '(+persp-save-session
            :which-key "Save current session")
    "."   '(+persp-switch-to
            :which-key "Switch workspace")
    "x"   '(+persp-kill-session
            :which-key "Kill session")
    "d"   '(+persp-delete
            :which-key "Delete workspace")
    "r"   '(+persp-rename
            :which-key "Rename workspace")
    "n"   '(+persp-switch-right
            :which-key "Next workspace")
    "p"   '(+persp-switch-left
            :which-key "Previous workspace")
    "1"   `(,(lambda! (+persp-switch-to 0))
            :which-key "Switch to workspace 1")
    "2"   `(,(lambda! (+persp-switch-to 1))
            :which-key "Switch to workspace 2")
    "3"   `(,(lambda! (+persp-switch-to 2))
            :which-key "Switch to workspace 3")
    "4"   `(,(lambda! (+persp-switch-to 3))
            :which-key "Switch to workspace 5")
    "5"   `(,(lambda! (+persp-switch-to 4))
            :which-key "Switch to workspace 5")
    "6"   `(,(lambda! (+persp-switch-to 5))
            :which-key "Switch to workspace 6")
    "7"   `(,(lambda! (+persp-switch-to 6))
            :which-key "Switch to workspace 7")
    "8"   `(,(lambda! (+persp-switch-to 7))
            :which-key "Switch to workspace 8")
    "9"   `(,(lambda! (+persp-switch-to 8))
            :which-key "Switch to workspace 9")
    "0"   '(+persp-switch-to-last
            :which-key "Switch to last workspace"))
  
  (general-nmap
    :prefix "SPC b"
    ""    '(nil :which-key "Buffer")
    "n"   '(evil-buffer-new
            :which-key "New empty buffer")
    "b"   '(persp-switch-to-buffer
            :which-key "Switch workspace buffer")
    "B"   '(switch-to-buffer
            :which-key "Switch buffer")
    "k"   '(kill-this-buffer
            :which-key "Kill this buffer")
    "K"   '(kill-other-buffers
            :which-key "Kill other buffers")
    "s"   '(save-buffer
            :which-key "Save buffer")
    "z"   '(bury-buffer
            :which-key "Bury buffer")
    "]"   '(next-buffer
            :which-key "Next buffer")
    "["   '(previous-buffer
            :which-key "Previous buffer"))

  (general-nmap
    :prefix "SPC c"
    ""    '(nil :which-key "Code")
    "x"   '(flycheck-list-errors
            :which-key "List errors"))

  (general-nmap
    :prefix "SPC f"
    ""    '(nil :which-key "File")
    "."   '(find-file
            :which-key "Find file")
    "/"   '(projectile-find-file
            :which-key "Find file in project")
    "a"   '(projectile-find-other-file
            :which-key "Find other file")
    ;;"c"   '()
    "d"   '(dired
            :which-key "Find directory")
    "r"   '(recentf-open-files
            :which-key "Recent files")
    "R"   '(projectile-recentf
            :which-key "Recent project files"))

  (general-nmap
    :prefix "SPC g"
    ""    '(nil :which-key "Git"))

  (general-nmap
    :prefix "SPC h"
    ""    '(nil :which-key "Help")
    "a"   '(apropos
            :which-key "Apropos")
    "c"   '(describe-char
            :which-key "Describe character")
    "f"   '(describe-function
            :which-key "Describe function")
    "F"   '(describe-face
            :which-key "Describe face")
    "i"   '(info-lookup-symbol
            :which-key "Info")
    "k"   '(describe-key
            :which-key "Describe key")
    "l"   '(find-library
            :which-key "Find library")
    "m"   '(view-echo-area-messages
            :which-key "View *Messages*")
    "M"   '(describe-mode
            :which-key "Describe mode")
    "v"   '(describe-variable
            :which-key "Describe variable")
    "w"   '(woman
            :which-key "Man pages")
    "."   '(helpful-at-point
            :which-key "Describe at point"))

  (general-nvmap
    :prefix "SPC i"
    ""   '(nil :which-key "Insert")
    "y"  '(helm-show-kill-ring
           :which-key "From kill-ring"))

  (general-nmap
    :prefix "SPC n"
    ""   '(nil :which-key "Notes")
    "x"  '(org-capture
           :which-key "Org capture"))

  (general-nmap
    :prefix "SPC o"
    ""   '(nil :which-key "Open")
    "a"  '(org-agenda
           :which-key "Org agenda")
    "b"  '(browse-url-of-file
           :which-key "Default browser")
    "-"  '(dired-jump
           :which-key "Dired")
    "e"  '(+eshell-open
           :which-key "Eshell")
    "E"  '(+eshell-open-popup
           :which-key "Eshell popup"))

  (general-nmap
    :prefix "SPC p"
    ""   '(nil :which-key "Project")
    "/"  '(projectile-find-file
           :which-key "Find file in project")
    "!"  '(projectile-run-command-in-root
           :which-key "Run command in project root")
    "c"  '(projectile-compile-project
           :which-key "Compile project")
    "o"  '(projectile-find-other-file
           :which-key "Find other file")
    "p"  '(projectile-switch-project
           :which-key "Switch project")
    "r"  '(projectile-recentf
           :which-key "Recent project files")
    "x"  '(projectile-invalidate-cache
           :which-key "Invalidate project cache"))

  (general-nmap
    :prefix "SPC q"
    ""   '(nil :which-key "Quit")
    "q"  '(evil-quit-all
           :which-key "Quit Emacs")
    "Q"  '(evil-save-and-quit-all
           :which-key "Save and quit")
    "X"  '(+persp-kill-session-and-quit
           :which-key "Quit (forget session)"))

  (general-nmap
    :prefix "SPC r"
    ""   '(nil :which-key "System"))

  (general-nmap
    :prefix "SPC s"
    ""   '(nil :which-key "Snippets"))

  (general-nmap
    :prefix "SPC t"
    ""   '(nil :which-key "Toggle")
    "s"  '(flyspell-mode
           :which-key "Flyspell")
    "f"  '(flycheck-mode
           :which-key "Flycheck")
    "F"  '(toggle-frame-fullscreen
           :which-key "Fullscreen")
    "i"  '(highlight-indentation-mode
           :which-key "Indent guides")
    "I"  '(highlight-indentation-current-column-mode
           :which-key "Indent guides (common)")))

(define-key universal-argument-map
  (kbd "SPC u") #'universal-argument-more)

(add-hook! tty-setup-hook (define-key input-decode-map
                            (kbd "TAB") [tab]))

(after! evil
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

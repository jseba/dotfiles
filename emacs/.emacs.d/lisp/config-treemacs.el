;;; config-treemacs.el

(use-package treemacs
  :init
  (setq treemacs-follow-after-init t
        treemacs-width 35
        treemacs-position 'left
        treemacs-is-never-other-window t
        treemacs-silent-refresh t
        treemacs-indentation 2
        treemacs-sorting 'alphabetic-desc
        treemacs-show-hidden-files t
        treemacs-goto-tag-strategy 'refetch-index
        treemacs-display-in-side-window t
        treemacs-persist-file (concat %var-dir "treemacs-persist"))
  :config
  (defvar +treemacs-use-git-mode
    (pcase (cons (not (null (executable-find "git")))
                 (not (null (executable-find "python3"))))
      (`(t . t) 'extended)
      (`(t)     'simple))
    "Type of Git integration for `treemacs-git-mode.'")

  (defvar treemacs-collapse-dirs (if (executable-find "python3") 3 0))

  (treemacs-follow-mode -1)
  (treemacs-filewatch-mode t)
  (when (memq +treemacs-use-git-mode '(simple extended))
    (treemacs-git-mode +treemacs-use-git-mode))
 
  (+popup-set-rule
   "^\\*Treemacs"
   :side treemacs-position
   :size treemacs-width
   :quit nil
   :ttl 0))

(use-package treemacs-evil
  :after treemacs
  :config
  (evil-set-initial-state 'treemacs-mode 'normal)
  (evil-define-key* 'normal treemacs-mode-map
    "?" #'treemacs-helpful-hydra
    [tab] #'treemacs-TAB-action
    [?\t] #'treemacs-TAB-action
    [return] #'treemacs-RET-action
    "RET" #'treemacs-RET-action
    "r"   #'treemacs-rename
    "d"   #'treemacs-delete
    "cf"  #'treemacs-create-file
    "cd"  #'treemacs-create-dir
    "R"   #'treemacs-refresh
    "u"   #'treemacs-goto-parent-node
    "q"   #'bury-buffer
    "Q"   #'treemacs-kill-buffer
    "ov"  #'treemacs-visit-node-vertical-split
    "oh"  #'treemacs-visit-node-horizontal-split
    "oo"  #'treemacs-visit-node-no-split
    "oaa" #'treemacs-visit-node-ace
    "oah" #'treemacs-visit-node-ace-horizontal-split
    "oav" #'treemacs-visit-node-ace-vertical-split
    "ox"  #'treemacs-visit-node-in-external-application
    "P"   #'treemacs-peek
    "n"   #'treemacs-next-line
    "p"   #'treemacs-previous-line
    "M-N" #'treemacs-next-line-other-window
    "M-P" #'treemacs-previous-line-other-window
    "<prior>" #'treemacs-previous-page-other-window
    "<next>" #'treemacs-next-page-other-window
    "M-n" #'treemacs-next-neighbour
    "M-p" #'treemacs-previous-neighbour
    "th"  #'treemacs-toggle-show-dotfiles
    "tw"  #'treemacs-toggle-fixed-width
    "tv"  #'treemacs-fringe-indicator-mode
    "tg"  #'treemacs-git-mode
    "tf"  #'treemacs-follow-mode
    "ta"  #'treemacs-filewatch-mode
    "w"   #'treemacs-set-width
    "yy"  #'treemacs-copy-path-at-point
    "yr"  #'treemacs-copy-project-root
    "s"   #'treemacs-resort
    "b"   #'treemacs-add-bookmark
    "C-p r" #'treemacs-rename-project
    "C-p a" #'treemacs-add-project-to-workspace
    "C-p d" #'treemacs-remove-project-from-workspace
    "C-p c c" #'treemacs-collapse-project
    "C-p c o" #'treemacs-collapse-other-projects
    "C-p c a" #'treemacs-collapse-all-projects
    "<backtab>" #'treemacs-collapse-all-projects
    "C-j" #'treemacs-next-project
    "C-k" #'treemacs-previous-project
    "h"   #'treemacs-root-up
    "l"   #'treemacs-root-down
    [escape] #'delete-window
    "j"   #'treemacs-next-line
    "k"   #'treemacs-previous-line
    "M-j" #'treemacs-next-neighbour
    "M-k" #'treemacs-previous-neighbour
    [down-mouse-1] #'ignore))

(use-package treemacs-projectile
  :after treemacs)

(provide 'config-treemacs)

(use-package helm
  :ensure t
  :init
  (require 'helm-mode)
  (require 'helm-config)
  (require 'helm-elisp)
  (require 'helm-grep)
  (require 'helm-buffers)
  (require 'helm-imenu)
  (require 'helm-regexp)
  (require 'helm-info)
  (require 'helm-files)
  (require 'helm-ring)

  (defun helm-hide-minibuffer-maybe ()
      (when (with-helm-buffer helm-echo-input-in-header-line)
        (let ((ov (make-overlay (point-min) (point-max) nil nil t)))
          (overlay-put ov 'window (selected-window))
          (overlay-put ov 'face (let ((bg-color (face-background 'default nil)))
                                  `(:background ,bg-color :foreground ,bg-color)))
          (setq-local cursor-type nil))))
  (add-hook 'helm-minibuffer-set-up-hook 'helm-hide-minibuffer-maybe)

  (setq helm-split-window-in-side-p t
        helm-mode-fuzzy-matching t
        helm-buffers-fuzzy-matching t
        helm-imenu-fuzzy-match t
        helm-M-x-fuzzy-match t
        helm-recentf-fuzzy-match t
        helm-semantic-fuzzy-match t
        helm-apropos-fuzzy-match t
        helm-ff-file-name-history-use-recentf t
        helm-ff-search-library-in-sexp t
        helm-echo-input-in-header-line t
        helm-display-header-line nil
        ;; No insta-jumps please
        helm-imenu-execute-action-at-once-if-one nil
        )
  (global-set-key (kbd "C-c h") #'helm-command-prefix)
  (global-unset-key (kbd "C-x c"))
  (when (executable-find "rg")
    (setq helm-grep-ag-command "rg --color=always --colors 'match:fg:black' --colors 'match:bg:yellow' --smart-case --no-heading --line-number %s %s %s"
          helm-grep-ag-pipe-cmd-switches '("--colors 'match:fg:black' --colors 'match:bg:yellow'")))

  (helm-mode)

  :bind
  (([remap find-file] . helm-find-files)
   ([remap switch-to-buffer] . helm-mini)
   ([remap imenu] . helm-imenu)
   ([remap occur] . helm-occur)
   ([remap info] . helm-info-at-point)
   ([remap yank-pop] . helm-show-kill-ring)
   ([remap insert-register] . helm-register)
   ([remap apropos-command] . helm-apropos)
   ([remap find-tag] . helm-etags-select)
   ([remap list-buffers] . helm-buffers-list)

   ("C-c h i" . helm-semantic-or-imenu)
   ("C-c h l" . helm-resume)
   ("C-c h m" . helm-man-woman)
   ("C-c h o" . helm-occur)
   ("C-c h f" . helm-recentf)
   ("C-c h x" . helm-register)
   ("C-c s a" . helm-do-grep-ag)
   ("C-c s A" . helm-grep-ag)

   :map helm-command-map
   ("o" . helm-occur)
   ("g" . helm-do-grep)
   ("SPC" . helm-all-mark-rings)

   :map helm-grep-mode-map
   ("<return>" . helm-grep-mode-jump-other-window)
   ("n" . helm-grep-mode-jump-other-window-forward)
   ("p" . helm-grep-mode-jump-other-window-backward)

   :map isearch-mode-map
   ("C-o" . helm-occur-from-isearch))
  :diminish helm-mode)

(use-package helm-projectile
  :ensure t
  :after (helm projectile)
  :init
  (setq helm-projectile-fuzzy-match t
	projectile-switch-project-action #'helm-projectile)
  (helm-projectile-on)
  :bind
  (("C-c s p" . helm-projectile-ag))
  )

(use-package helm-gtags
  :ensure t
  :init
  (setq helm-gtags-fuzzy-match t
        helm-gtags-direct-helm-completing t
        helm-gtags-display-style 'detail
        helm-gtags-ignore-case t
        helm-gtags-prefix-key "\C-cu"
        helm-gtags-suggested-key-mapping t)
  (add-hook 'dired-mode-hook    #'helm-gtags-mode)
  (add-hook 'eshell-mode-hook   #'helm-gtags-mode)
  (add-hook 'c-mode-common-hook #'helm-gtags-mode)
  (add-hook 'java-mode-hook     #'helm-gtags-mode)
  (add-hook 'asm-mode-hook      #'helm-gtags-mode)
  :bind
  (:map helm-gtags-mode-map
	("C-c u a" . helm-gtags-tags-in-this-function)
	("C-c u h" . helm-gtags-display-browser)
	("C-c u C-]" . helm-gtags-find-tag-from-here)
	("C-c u C-t" . helm-gtags-pop-stack)
	("C-c u P" . helm-gtags-parse-file)
	("C-c u f" . helm-gtags-find-file)
	("C-c u p" . helm-gtags-find-pattern)
	("C-c u s" . helm-gtags-find-symbol)
	("C-c u r" . helm-gtags-find-rtag)
	("C-c u t" . helm-gtags-find-tag)
	("C-c u <" . helm-gtags-previous-history)
	("C-c u >" . helm-gtags-next-history))
  :diminish helm-gtags-mode)

(use-package helm-descbinds
  :ensure t
  :init
  (helm-descbinds-mode))

(use-package helm-swoop
  :ensure t
  :after helm
  :init
  (setq helm-swoop-speed-or-color t
	helm-swoop-split-window-function #'helm-default-display-buffer)
  :bind
  (("C-c h s" . helm-swoop)
   ("C-c h S" . helm-multi-swoop)
   ("C-c h C-s" . helm-multi-swoop-all)
   :map isearch-mode-map
   ("M-i" . helm-swoop-from-isearch)
   :map helm-swoop-map
   ("M-i" . helm-multi-swoop-all-from-helm-swoop)))

(use-package helm-company
  :ensure t
  :after (helm company)
  :bind
  (:map company-mode-map
   ("C-:" . helm-company)
   :map company-active-map
   ("C-:" . helm-company)))

(provide 'init-helm)

;;; config-helm.el

(use-package helm-mode
  :defer t
  :ensure helm
  :after-call pre-command-hook
  :preface
  (defvar +helm-global-prompt ">>> ")
  (defvar helm-generic-files-map (make-sparse-keymap))

  :init
  (map! [remap apropos] #'helm-apropos
        [remap find-library] #'helm-locate-library
        [remap bookmark-jump] #'helm-bookmarks
        [remap execute-extended-command] #'helm-M-x
        [remap find-file] #'helm-find-files
        [remap imenu-anywhere] #'helm-imenu-anywhere
        [remap imenu] #'helm-semantic-or-imenu
        [remap noop-show-kill-ring] #'helm-show-kill-ring
        [remap switch-to-buffer] #'helm-buffers-list
        [remap recentf-open-files] #'helm-recentf))

(use-package helm
  :after helm-mode
  :preface
  (setq helm-candidate-number-limit 50
        helm-display-header-line nil
        helm-mode-line-string nil
        helm-ff-auto-update-initial-value nil
        helm-find-files-doc-header nil
        helm-mode-handle-completion-in-region nil
        helm-display-buffer-default-width nil
        helm-display-buffer-default-height 0.25
        helm-imenu-execute-action-at-once-if-one nil
        helm-ff-lynx-style-map nil
        helm-split-window-inside-p t
        helm-default-prompt-display-function #'+helm--set-prompt-display)

  :init
  (let ((fuzzy t))
    (setq helm-M-x-fuzzy-match fuzzy
          helm-ag-fuzzy-match fuzzy
          helm-apropos-fuzzy-match fuzzy
          helm-bookmark-show-location fuzzy
          helm-buffers-fuzzy-matching fuzzy
          helm-completion-in-region-fuzzy-match fuzzy
          helm-ff-fuzzy-matching fuzzy
          helm-file-cache-fuzzy-match fuzzy
          helm-flx-for-helm-locate fuzzy
          helm-imenu-fuzzy-match fuzzy
          helm-lisp-fuzzy-completion fuzzy
          helm-locate-fuzzy-match fuzzy
          helm-mode-fuzzy-match fuzzy
          helm-projectile-fuzzy-match fuzzy
          helm-recentf-fuzzy-match fuzzy
          helm-semantic-fuzzy-match fuzzy))

  (defun +helm-replace-prompt (plist)
    (cond ((not +helm-global-prompt) plist)
          ((keywordp (car plist))
           (plist-put plist :prompt +helm-global-prompt))
          ((setf (nth 2 plist) +helm-global-prompt)
           plist)))
  (advice-add #'helm :filter-args #'+helm-replace-prompt)

  (defun +helm-hide-mode-line (&rest _)
    (with-current-buffer (helm-buffer-get)
      (unless helm-mode-line-string
        (hide-mode-line-mode +1))))
  (add-hook 'helm-after-initialize-hook #'+helm-hide-mode-line)
  (advice-add #'helm-display-mode-line :override #'+helm-hide-mode-line)

  (defun +helm-use-helpful (orig-fn &rest args)
    (cl-letf (((symbol-function #'describe-function)
               (symbol-function #'helpful-callable))
              ((symbol-function #'describe-variable)
               (symbol-function #'helpful-variable)))
      (apply orig-fn args)))
  (advice-add #'helm-describe-variable :around #'+helm-use-helpful)
  (advice-add #'helm-describe-function :around #'+helm-use-helpful)
  
  :config
  (helm-mode +1)

  (map! (:keymap helm-map
          [left]     #'left-char
          [right]    #'right-char
          "C-S-n"    #'helm-next-source
          "C-S-p"    #'helm-previous-source
          "C-f"      #'helm-next-page
          "C-S-f"    #'helm-previous-page
          "C-u"      #'helm-delete-minibuffer-contents
          "C-w"      #'backward-kill-word
          "C-s"      #'helm-minibuffer-history
          "C-b"      #'backward-word
          "TAB"      #'helm-execute-persistent-action
          "C-z"      #'helm-select-action)
        (:keymap (helm-find-files-map helm-read-file-map)
          [M-return] #'helm-ff-run-switch-other-window
          "C-w"      #'helm-find-files-up-one-level)
        (:keymap helm-generic-files-map
          [M-return] #'helm-ff-run-switch-other-window)
        (:keymap helm-buffer-map
          [M-return] #'helm-buffer-switch-other-window)
        (:keymap helm-moccur-map
          [M-return] #'helm-moccur-run-goto-line-ow)
        (:keymap helm-grep-map
          [M-return] #'helm-grep-run-other-window-action))

  (after! helm-bookmark
    (setq helm-bookmark-show-location t))

  (after! helm-files
    (setq helm-boring-file-regexp-list (append (list "\\.projects$"
                                                     "\\.DS_Store$")
                                               helm-boring-file-regexp-list)))

  (after! helm-locate
    (set-keymap-parent helm-generic-files-map helm-map))
  
  (add-to-list 'helm-completing-read-handlers-alist
               (cons #'find-file-at-point nil))

  (+popup-set-rule "^\\*helm" :vslot -100 :size 0.22))

(use-package helm-ag
  :init
  (defvar +helm-project-search-tools '(rg ag))

  (map! (:keymap helm-ag-map
          [backtab] #'helm-ag-edit
          [left]    nil
          [right]   nil)
        (:keymap helm-ag-edit-map
          [remap quit-window] #'helm-ag--edit-abort
          "RET" #'compile-goto-error))

  (defun +helm-ag-search-args (all-files-p recursivep)
    (list (concat "ag "
                  (if %-IS-WIN32 "--vimgrep" "--nocolor --nogroup"))
          "-S"
          (if all-files-p "-a -z")
          (unless recursivep "--depth 1")))

  (defun +helm-rg-search-args (all-files-p recursivep)
    (list "rg --no-heading --line-number --color never"
          "-S"
          (if all-files-p "-uu -z")
          (unless recursivep "--maxdepth 1")))

  (defun +helm--grep-source ()
    (helm-build-async-source (capitalize (helm-grep-command t))
      :header-name
      (lambda (_name)
        "Helm Projectile Grep (C-c ? Help)")
      :candidates-process #'helm-grep-collect-candidates
      :filter-one-by-one #'helm-grep-filter-one-by-one
      :candidate-number-limit 9999
      :nohighlight t
      :keymap helm-grep-map
      :history 'helm-grep-history
      :action (apply #'helm-make-actions
                     helm-projectile-grep-or-ack-actions)
      :persistent-action 'helm-grep-persistent-action
      :persistent-help
      "Jump to line (`C-u' Record in mark ring)"
      :requires-pattern 2))

  (defun +helm--grep-search (directory query prompt &optional all-files-p recursivep)
    (let* ((default-directory directory)
           (helm-ff-default-directory directory)
           (helm-grep-in-recurse recursivep)
           (helm-grep-ignored-files
            (unless all-files-p
              (cl-union (projectile-ignored-files-rel) grep-find-ignored-files)))
           (helm-grep-ignored-directories
            (unless all-files-p
              (cl-union (mapcar 'directory-file-name
                                (projectile-ignored-directories-rel))
                        grep-find-ignored-directories)))
           (helm-grep-default-command
            (if (and nil (eq (projectile-project-vcs) 'git))
                (format "git --no-pager grep --no-color -n%%c -e %%p %s -- %%f"
                        (if recursivep "" "--max-depth 1"))
              (format "grep -si -a %s %%e -n%%cH -e %%p %%f %s"
                      (if recursivep "-R" "")
                      (if recursivep "." "./*"))))
           (helm-grep-default-recurse-command
            helm-grep-default-command))
      (setq helm-source-grep (+helm--grep-source))
      (helm :sources 'helm-source-grep
            :input query
            :prompt prompt
            :buffer "*helm grep*"
            :default-directory default-directory
            :keymap helm-grep-map
            :history 'helm-grep-history
            :truncate-lines helm-grep-truncate-lines)))

  (cl-defun +helm-file-search (engine &key query in all-files (recursive t))
    (declare (indent defun))
    (require 'helm-ag)
    (helm-ag--init-state)
    (let* ((project-root (+projectile-project-root))
           (directory (or in project-root))
           (default-directory directory)
           (helm-ag--default-directory directory)
           (helm-ag--default-target (list directory))
           (engine (or engine
                       (cl-find-if
                        #'executable-find
                        +helm-project-search-tools
                        :key #'symbol-name)
                       (and (or (executable-find "grep")
                                (executable-find "git"))
                            'grep)
                       (user-error
                        "No search engine specified (is ag, rg, or git installed?)")))
           (query (or query
                      (when (use-region-p)
                        (let ((beg (or (bound-and-true-p evil-visual-beginning)
                                       (region-beginning)))
                              (end (or (bound-and-true-p evil-visual-end)
                                       (region-end))))
                          (when (> (abs (- end beg)) 1)
                            (rxt-quote-pcre
                             (buffer-substring-no-properties beg end)))))
                      ""))
           (prompt (format "[%s %s]"
                           (symbol-name engine)
                           (cond ((file-equal-p directory project-root)
                                  (projectile-project-name))
                                 ((file-equal-p directory default-directory)
                                  "./")
                                 ((file-relative-name directory project-root)))))
           (command
            (pcase engine
              (`ag (+helm-ag-search-args all-files recursive))
              (`rg (+helm-rg-search-args all-files recursive))
              (`grep (+helm-grep-search-directory
                      directory
                      query
                      prompt
                      all-files
                      recursive)
                     (cl-return t))))
           (helm-ag-base-command (string-join command " ")))
      (helm-attrset 'name (format "[%s %s] Searching %s"
                                  engine
                                  (string-join (delq nil (cdr command)) " ")
                                  (abbreviate-file-name directory))
                    helm-source-do-ag)
      (cl-letf ((+helm-global-prompt prompt)
                ((symbol-function 'helm-do-ag--helm)
                 (lambda ()
                   (helm :sources '(helm-source-do-ag)
                         :buffer "*helm-ag*"
                         :keymap helm-do-ag-map
                         :input query
                         :history 'helm-ag--helm-history))))
        (helm-do-ag directory))))

  (defun +helm--get-command (format)
    (cl-loop for tool in (cl-remove-duplicates +helm-project-search-tools
                                               :from-end t)
             if (executable-find (symbol-name tool))
             return (intern (format format tool))))

  (defun +helm-project-search (&optional all-files-p)
    (interactive "P")
    (funcall (or (+helm--get-command "+helm-%s")
                 #'+helm-grep)
             (or all-files-p current-prefix-arg)))
  (defun +helm-project-search-from-here (&optional all-files-p)
    (interactive "P")
    (funcall (or (+helm--get-command "+helm-%s-from-here")
                 #'+helm-grep-from-here)
             (or all-files-p current-prefix-arg)))

  (defun +helm-rg (all-files-p &optional query directory)
    (interactive "P")
    (+helm-file-search 'rg
      :query query
      :in directory
      :all-files all-files-p))
  (defun +helm-ag (all-files-p &optional query directory)
    (interactive "P")
    (+helm-file-search 'ag
      :query query
      :in directory
      :all-files all-files-p))
  (defun +helm-grep (all-files-p &optional query directory)
    (interactive "P")
    (+helm-file-search 'grep
      :query query
      :in directory
      :all-files all-files-p))
  (defun +helm-rg-from-here (all-files-p &optional query directory)
    (interactive "P")
    (+helm-file-search 'rg
      :query query
      :in default-directory
      :all-files all-files-p))
  (defun +helm-ag-from-here (all-files-p &optional query directory)
    (interactive "P")
    (+helm-file-search 'ag
      :query query
      :in default-directory
      :all-files all-files-p))
  (defun +helm-grep-from-here (all-files-p &optional query directory)
    (interactive "P")
    (+helm-file-search 'grep
      :query query
      :in default-directory
      :all-files all-files-p))

  :config
  (+popup-set-rule "^\\*helm-ag-edit" :size 0.35 :ttl 0 :quit nil)
  (advice-add #'helm-ag-show-status-default-mode-line :override #'ignore))

;; (use-package helm-c-yasnippet)

(use-package helm-company)

(use-package helm-describe-modes)

(use-package helm-projectile
  :after helm
  :commands
  (helm-projectile-find-file
   helm-projectile-switch-project
   helm-projectile-switch-to-buffer)
  :init
  (defun +helm-projectile-find-file ()
    (interactive)
    (call-interactively
     (if (or (file-equal-p default-directory "~")
             (when-let* ((proot (+projectile-project-root 'nocache)))
               (file-equal-p proot "~")))
         #'helm-find-files
       #'helm-projectile-find-file)))

  (setq projectile-completion-system 'helm)
  (defvar helm-projectile-find-file-map (make-sparse-keymap))
  (set-keymap-parent helm-projectile-find-file-map helm-map)

  (map! [remap projectile-find-file] #'helm-projectile-find-file
        [remap projectile-switch-project] #'helm-projectile-switch-project
        [remap projectile-switch-to-buffer] #'helm-projectile-switch-to-buffer))

(use-package swiper-helm
  :config
  (setq swiper-helm-display-function
        (lambda (buf &optional _resume)
          (pop-to-buffer buf))))

(provide 'config-helm)
;;; config-helm.el ends here

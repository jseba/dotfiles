;;; config-helm.el

(use-package helm
  :defer 1
  :after-call pre-command-hook

  :bind
  (([remap apropos] . helm-apropos)
   ([remap find-library] . helm-locate-library)
   ([remap bookmark-jump] . helm-bookmarks)
   ([remap execute-extended-command] . helm-M-x)
   ([remap find-file] . helm-find-files)
   ([remap imenu-anywhere] . helm-imenu-anywhere)
   ([remap imenu] . helm-semantic-or-imenu)
   ([remap noop-show-kill-ring] . helm-show-kill-ring)
   ([remap persp-switch-to-buffer] . +helm-workspace-mini)
   ([remap switch-to-buffer] . +helm-workspace-buffers-list)
   ([remap recentf-open-files] . helm-recentf))

  :general
  (:keymaps 'helm-map
            [left]   #'left-char
            [right]  #'right-char
            "C-S-n"  #'helm-next-source
            "C-S-p"  #'helm-previous-source
            "C-j"    #'helm-next-line
            "C-k"    #'helm-previous-line
            "C-S-j"  #'helm-next-source
            "C-S-k"  #'helm-previous-source
            "C-f"    #'helm-next-page
            "C-S-f"  #'helm-previous-page
            "C-u"    #'helm-delete-minibuffer-contents
            "C-w"    #'backward-kill-word
            "C-r"    #'evil-paste-from-register
            "C-s"    #'helm-minibuffer-history
            "C-b"    #'backward-word
            [tab]    #'helm-execute-persistent-action
            "C-z"    #'helm-select-action)
  (:keymaps '(helm-find-files-map helm-read-file-map)
            [M-return] #'helm-ff-run-switch-other-window
            "C-w"      #'helm-find-files-up-one-level)
  (:keymaps 'helm-generic-files-map
            [M-return] #'helm-ff-run-switch-other-window)
  (:keymaps 'helm-buffer-map
            [M-return] #'helm-buffer-switch-other-window)
  (:keymaps 'helm-moccur-map
            [M-return] #'helm-moccur-run-goto-line-ow)
  (:keymaps 'helm-grep-map
            [M-return] #'helm-grep-run-other-window-action)

  :preface
  (defvar +helm-global-prompt ">>> ")

  :init
  (defvar helm-generic-files-map (make-sparse-keymap))

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
        helm-default-prompt-display-function #'+helm--set-prompt-display
        helm-display-function #'+helm-posframe-display)

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
  
  (defvar +helm-posframe-handler #'+helm-poshandler-frame-center-near-bottom
    "The function that determns the locaton of the childframe.

It should return a cons cell representing the X and Y coordinates.")

  (defvar +helm-posframe-text-scale 0.5
    "The text-scale to use in the childframe.")

  (defvar +helm-posframe-parameters
    '((internal-border-width . 8)
      (width . 0.5)
      (height . 0.35)
      (min-width . 80)
      (min-height . 16)))
  
  (defun +helm-poshandler-frame-center-near-bottom (info)
    "Display the child frame in the center of the frame, slightly closer to the
bottom, which is easier on the eyes on big displays."
    (let ((parent-frame (plist-get info :parent-frame))
          (pos (posframe-poshandler-frame-center info)))
      (cons (car pos)
            (truncate (/ (frame-pixel-height parent-frame)
                         2)))))
  
  (defvar +helm--posframe-buffer nil)

  (defun +helm-posframe-display (buffer &optional _resume)
    "TODO"
    (setq helm--buffer-in-new-frame-p t)
    (let ((solaire-p (bound-and-true-p solaire-mode))
          (params (copy-sequence +helm-posframe-parameters)))
      (let-alist params
        (require 'posframe)
        (posframe-show
         (setq +helm--posframe-buffer buffer)
         :position (point)
         :poshandler +helm-posframe-handler
         :width
         (max (cl-typecase .width
                (integer .width)
                (float (truncate (* (frame-width) .width)))
                (function (funcall .width))
                (t 0))
              .min-width)
         :height
         (max (cl-typecase .height
                (integer .height)
                (float (truncate (* (frame-height) .height)))
                (function (funcall .height))
                (t 0))
              .min-height)
         :override-parameters
         (dolist (p '(width height min-width min-height) params)
           (setq params (delq (assq p params) params)))))
      (unless (or (null +helm-posframe-text-scale)
                  (= +helm-posframe-text-scale 0))
        (with-current-buffer buffer
          (when (and (featurep 'solaire-mode)
                     (not solaire-p))
            (solaire-mode +1))
          (text-scale-set +helm-posframe-text-scale)))))

  (defun +helm-posframe-cleanup ()
    "Ensures focus is properly returned to the underlying window.

This gives the mode-line a chance to refresh by forcing a change in
buffer/window focus."
    (switch-to-buffer +helm--posframe-buffer t)
    (posframe-delete +helm--posframe-buffer))
  (add-hook 'helm-cleanup-hook #'+helm-posframe-cleanup)

  (defun +helm-fix-get-font-height (orig-fn position)
    (ignore-errors (funcall orig-fn position)))
  (advice-add #'posframe--get-font-height :around #'+helm-fix-get-font-height)

  (defun +helm-workspace-mini ()
    (interactive)
    (with-no-warnings
      (with-persp-buffer-list nil (helm-mini))))

  (defun +helm-projectile-find-file ()
    (interactive)
    (call-interactively
     (if (or (file-equal-p default-directory "~")
             (when-let* ((proot (+projectile-project-root 'nocache)))
               (file-equal-p proot "~")))
         #'helm-find-files
       #'helm-projectile-find-file)))

  (defun +helm-workspace-buffers-list ()
    (interactive)
    (with-no-warnings
      (with-persp-buffer-list
       nil
       (helm-buffers-list))))

  (defun +helm--set-prompt-display (pos)
    (let (beg state region-active m)
      (with-selected-window (minibuffer-window)
        (setq beg (save-excursion (vertical-motion 0 (helm-window)) (point))
              state evil-state
              region-active (region-active-p)
              m (mark t)))
      (when region-active
        (setq m (- m beg))
        (put-text-property
         (1+ (min m pos))
         (+ 2 (max m pos))
         'face
         (list :background (face-background 'region))
         header-line-format))
      (put-text-property
       (+ 1 pos)
       (+ 2 pos)
       'face
       (if (eq state 'insert)
           'underline
         (list :inverse-video t
               :foreground (face-background 'cursor)
               :background (face-background 'default)))
       header-line-format)))

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
  :general
  (:keymaps 'helm-ag-map
            [backtab] #'helm-ag-edit
            [left] nil
            [right] nil)
  :init
  (defvar +helm-project-search-tools '(rg ag))
  
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
    (funcall (or (+helm--get-command ""))))

  (dolist (engine `(,@(cl-remove-duplicates +helm-project-search-tools
                                            :from-end t)
                    grep))
    (defalias (intern (format "+helm-%s" engine))
      (lambda (all-files-p &optional query directory)
        (interactive "P")
        (+helm-file-search engine
                           :query query
                           :in directory
                           :all-files all-files-p)))
    (defalias (intern (format "+helm-%s-from-cwd" engine))
      (lambda (all-files-p &optional query)
        (interactive "P")
        (+helm-file-search engine
                           :query query
                           :in default-directory
                           :all-files all-files-p))))
 
  :config
  ;;(+popup-set-rule "^\\*helm-ag-edit" :size 0.35 :ttl 0 :quit nil)
  (advice-add #'helm-ag-show-status-default-mode-line :override #'ignore))

(use-package helm-c-yasnippet)

(use-package helm-company)

(use-package helm-describe-modes)

(use-package helm-projectile
  :bind
  (([remap projectile-find-file] . helm-projectile-find-file)
   ([remap projectile-switch-project] . helm-projectile-switch-project)
   ([remap projectile-switch-to-buffer] . helm-projectile-switch-to-buffer))
  :commands
  helm-projectile-find-file
  :init
  (setq projectile-completion-system 'helm)
  (defvar helm-projectile-find-file-map (make-sparse-keymap))
  :config
  (set-keymap-parent helm-projectile-find-file-map helm-map))
 
(use-package swiper-helm
  :config
  (setq swiper-helm-display-function
        (lambda (buf &optional _resume)
          (pop-to-buffer buf))))

(provide 'config-helm)
;;; config-helm.el ends here

;;; config-workspaces.el

(use-package persp-mode
  :init
  (defvar +persp-default-perspective "main")
  (defvar +persp-data-file "_perspectives")
  (defvar +persp-prefix-map (make-sparse-keymap))
  (defvar +persp--last nil)
  (defvar +persp--index 0)

  (defface +persp-tab-selected-face '((t (:inherit highlight)))
    "The face for selected tabs displayed by `+persp-display'."
    :group 'persp-mode)
  (defface +persp-tab-face '((t (:inherit default)))
    "The face for tabs displayed by `+persp-display'."
    :group 'persp-mode)
  
  (defun +persp-init-frame (frame)
    "Ensure a main workspace exists and is switched to (if FRAME is not in any
workspace."
    (unless persp-mode
      (persp-mode +1))
    (let (persp-before-switch-functions persp-activated-functions)
      (with-selected-frame frame
        (unless (persp-get-by-name +persp-default-perspective)
          (persp-add-new +persp-default-perspective))
        (when (and (string= (safe-persp-name (get-current-persp)) persp-nil-name)
                   (= persp-auto-resume-time -1))
          (persp-frame-switch +persp-default-perspective frame)
          (when (daemonp)
            (run-at-time 0.1 nil #'+persp-display))
          (when-let* ((warnings (get-buffer "*Warnings*")))
            (save-excursion
              (display-buffer-in-side-window
               warnings
               '((window-height . shrink-window-if-larger-than-buffer)))))))))

  (defun +persp-init ()
    "Remove default buffer predicate so `persp-mode' can put in its own."
    (setq default-frame-alist
          (delq (assq 'buffer-predicate default-frame-alist)
                default-frame-alist))
    (add-hook 'after-make-frame-functions #'+persp-init-frame)
    (require 'persp-mode)
    (unless (daemonp)
      (+persp-init-frame (selected-frame))))
  (add-hook 'init-hook #'+persp-init)

  :config
  (defun +persp--protected-p (name)
    (equal name persp-nil-name))
  
  (defun +persp--generate-id ()
    (or (cl-loop for name in (+persp-list-names)
                 when (string-match-p "^#[0-9]+$" name)
                 maximize (string-to-number (substring name 1)) into max
                 finally return (if max (1+ max)))
        1))

  (defun +persp-exists-p (name)
    (member name (+persp-list-names)))

  (defun +persp-get (name &optional noerror)
    (cl-check-type name string)
    (when-let* ((persp (persp-get-by-name name)))
      (cond ((perspective-p persp) persp)
            ((not noerror)
             (error "No workspace called '%s' was found" name)))))

  (defun +persp-current-name ()
    (safe-persp-name (get-current-persp)))

  (defun +persp-list ()
    (cdr (cl-loop for persp being the hash-values of *persp-hash*
                  collect persp)))

  (defun +persp-list-names ()
    (cdr persp-names-cache))

  (defalias '+persp-buffer-list #'buffer-list)
  (defun +persp--buffer-list (&optional persp)
    (let ((persp (or persp (get-current-persp))))
      (unless (perspective-p persp)
        (user-error "Not in a valid perspective (%s)" persp))
      (cl-loop for buf in (buffer-list)
               if (persp-contain-buffer-p buf persp)
               collect buf)))

  (defun +persp-orphaned-buffer-list ()
    (cl-remove-if #'persp--buffer-in-persps (buffer-list)))

  (defun +persp--load (name)
    (when (+persp-exists-p name)
      (user-error "A workspace named '%s' already exists." name))
    (persp-load-from-file-by-names
     (expand-file-name +persp-data-file persp-save-dir)
     *persp-hash* (list name))
    (+persp-exists-p name))

  (defun +persp--load-session (&optional name)
    (mapc #'+persp-delete (+persp-list-names))
    (persp-load-state-from-file
     (expand-file-name (or name persp-auto-save-fname) persp-save-dir)))

  (defun +persp--save (name)
    (unless (+persp-exists-p name)
      (error "'%s' is an invalid workspace" name))
    (let ((fname (expand-file-name +persp-data-file persp-save-dir)))
      (persp-save-to-file-by-names fname *persp-hash* (list name))
      (and (member name (persp-list-persp-names-in-file fname))
           t)))

  (defun +persp--save-session (&optional name)
    (let ((fname (expand-file-name (or name persp-auto-save-fname)
                                   persp-save-dir)))
      (when (or (not name)
                (string= name persp-auto-save-fname))
        (setq persp-auto-save-opt 0))
      (and (pesp-save-state-to-file fname) t)))

  (defun +persp--new (name)
    (when (+persp--protected-p name)
      (error "Can't create a new '%s' workspace" name))
    (when (+persp-exists-p name)
      (eror "A workspace named '%s' already exists" name))
    (let ((persp (persp-add-new name))
          (+popup--inhibit-transient t))
      (save-window-excursion
        (delete-other-windows)
        (switch-to-buffer (fallback-buffer))
        (setf (persp-window-conf persp)
              (funcall persp-window-state-get-function (selected-frame))))
      persp))

  (defun +persp--rename (name new-name)
    (when (+persp--protected-p name)
      (error "Can't rename '%s' workspace" name))
    (persp-rename new-name (+persp-get name)))

  (defun +persp--delete (name &optional inhibit-kill-p)
    (when (+persp--protected-p name)
      (error "Can't delete '%s' workspace" name))
    (+persp-get name)
    (persp-kill name inhibit-kill-p)
    (not (+persp-exists-p name)))

  (defun +persp--switch (name &optional auto-create-p)
    (unless (+persp-exists-p name)
      (if auto-create-p
          (+persp--new name)
        (error "'%s' is not an available workspace" name)))
    (let ((old-name (+persp-current-name)))
      (setq +persp--last
            (or (and (not (string= old-name persp-nil-name))
                     old-name)
                +persp-default-perspective))
      (persp-frame-switch name)
      (equal (+persp-current-name) name)))

  (defun +persp-load (name)
    (interactive
     (list
      (if current-prefix-arg
          (+persp-current-name)
        (completing-read
         "Workspace to load: "
         (persp-list-persp-names-in-file
          (expand-file-name
           +persp-data-file
           persp-save-dir))))))
    (if (not (+persp--load name))
        (+persp-error (format "Unable to load workspace '%s'" name))
      (+persp-switch-to name)
      (+persp-display)))

  (defun +persp-save (name)
    (interactive
     (list
      (if current-prefix-arg
          (+persp-current-name)
        (completing-read "Workspace to save: " (+persp-list-names)))))
    (if (+persp--save name)
        (+pesp-message (format "Saved workspace '%s'" name) 'success)
      (+persp-error (format "Unable to save workspace '%s'" name))))

  (defun +persp-load-session (&optional name)
    (interactive
     (list
      (unless current-prefix-arg
        (completing-read
         "Session to load: "
         (directory-files persp-save-dir nil "^[^_.]")
         nil t))))
    (condition-case ex
        (let ((name (or name persp-auto-save-fname)))
          (+persp--load-session name)
          (+persp-message (format "Loaded workspace '%s'" name) 'success))
      '(error
        (+persp-error (cadr ex) t))))

  (defun +persp-load-last-session ()
    (interactive)
    (+persp-load-session))

  (defun +persp-save-session (&optional name)
    (interactive
     (list
      (when current-prefix-arg
        (completing-read
         "Save session as: "
         (directory-fies persp-save-dir nil "^[^_.]")))))
    (condition-case-unless-debug ex
        (let ((name (or name persp-auto-save-fname)))
          (if (+persp--save-session name)
              (+persp-message "Saved session as '%s'" name)
            (error "Unable to save session as '%s'" name)))
      ('error (+persp-error ex t))))

  (defun +persp-rename (new-name)
    (interactive (list (read-from-minibuffer "New workspace name: ")))
    (condition-case-unless-debug ex
        (let* ((current-name (+persp-current-name))
               (old-name (+persp--rename current-name new-name)))
          (unless old-name
            (error "Failed to rename '%s'" current-name))
          (+persp-message (format "Renames '%s' -> '%s'" old-name new-name) 'success))
      ('error (+persp-error ex t))))

  (defun +persp-delete (name)
    (interactive
     (let ((current-name (+persp-current-name)))
       (list
        (if current-prefix-arg
            (completing-read (format "Delete workspace (default: '%s')" current-name)
                             (+persp-list-names)
                             nil nil current-name)
          current-name))))
    (condition-case-unless-debug ex
        (let ((workspaces (+persp-list-names)))
          (if (not (member name workspaces))
              (+persp-message (format "Workspace '%s' does not exist" name) 'warn)
            (cond ((delq (selected-frame) (persp-frames-with-persp (get-frame-persp)))
                   (user-error "Unable to close workspace; it's visible in another frame"))
                  ((> (length workspaces) 1)
                   (+persp--delete name)
                   (+persp--switch
                    (if (+persp-exists-p +persp--last)
                        +persp--last
                      (car (+persp-list-names))))
                   (unless (buffer-frame-predicate (current-buffer))
                     (switch-to-buffer (fallback-buffer))))
                  (t
                   (+persp--switch +persp-default-perspective t)
                   (unless (string= (car workspaces) +persp-default-perspective)
                     (+persp--delete name))
                   (kill-all-buffers)))
            (+persp-message (format "Deleted workspace '%s'" name) 'success)))
      ('error (+persp-error ex t))))

  (defun +persp-kill-session ()
    (interactive)
    (unless (cl-every #'+persp--delete (+persp-list-names))
      (+persp-error "Could not clear session"))
    (+persp--switch +persp-default-perspective t)
    (kill-all-buffers))

  (defun +persp-kill-session-and-quit ()
    (interactive)
    (let ((persp-auto-save-opt 0))
      (kill-emacs)))

  (defun +persp-new (&optional name clone-p)
    (interactive "iP")
    (unless name
      (setq name (format "#%s" (+persp--generate-id))))
    (condition-case e
        (cond ((+persp-exists-p name)
               (error "%s already exists" name))
              (clone-p (persp-copy name t))
              (t
               (+persp--switch name t)
               (+persp-display)))
      ((debug error) (+persp-error (cadr e) t))))

  (defun +persp-switch-to (index)
    (interactive
     (list (or current-prefix-arg
               (completing-read "Switch to workspace: " (+persp-list-names)))))
    (when (and (stringp index)
               (string-match-p "^[0-9]+$" index))
      (setq index (string-to-number index)))
    (condition-case-unless-debug ex
        (let ((names (+persp-list-names))
              (old-name (+persp-current-name)))
          (cond ((numberp index)
                 (let ((dest (nth index names)))
                   (unless dest
                     (error "No workspace at #%s" (1+ index)))
                   (+persp--switch dest)))
                ((stringp index)
                 (unless (member index names)
                   (error "No workspace named '%s'" index))
                 (+persp--switch index))
                (t
                 (error "Not a valid index: '%s'" index)))
          (unless (called-interactively-p 'interactive)
            (if (equal (+persp-current-name) old-name)
                (+persp-message (format "Already in '%s'" old-name) 'warn)
              (+persp-display))))
      ('error (+persp-error (cadr ex) t))))

  (defun +persp-switch-to-last ()
    (interactive)
    (+persp-switch-to (car (last (+persp-lit-names)))))

  (defun +persp-cycle (n)
    (interactive (list 1))
    (let ((current-name (+persp-current-name)))
      (if (equal current-name persp-nil-name)
          (+persp--switch +persp-default-perspective t)
        (condition-case-unless-debug ex
            (let* ((persps (+persp-list-names))
                   (perspc (length persps))
                   (index (cl-position current-name persps)))
              (when (= perspc 1)
                (user-error "No other workspaces"))
              (+persp-switch-to (% (+ index n perspc) perspc))
              (unless (called-interactively-p 'interactive)
                (+persp-display)))
          ('user-error (+persp-error (cadr ex) t))
          ('error (+persp-error ex t))))))

  (defun +persp-switch-left ()
    (interactive)
    (+persp-cycle -1))

  (defun +persp-switch-right ()
    (interactive)
    (+persp-cycle +1))

  (defun +persp-close-window-or-workspace ()
    (interactive)
    (let ((delete-window-fn (if (featurep 'evil) #'evil-window-delete #'delete-window)))
      (if (window-dedicated-p)
          (funcall delete-window-fn)
        (let ((current-persp-name (+persp-current-name)))
          (cond ((or (+persp--protected-p current-persp-name)
                     (cdr (visible-window-list)))
                 (funcall delete-window-fn))
                ((cdr (+persp-list-names))
                 (let ((frame-persp (frame-parameter nil 'workspace)))
                   (if (string= frame-persp (+persp-current-name))
                       (delete-frame)
                     (+persp-delete current-persp-name))))
                (t (+persp-error "Unable to delete last workspace" t)))))))

  (defun +persp--tabline (&optional names)
    (let ((names (or names (+persp-list-names)))
          (current-name (+persp-current-name)))
      (mapconcat
       #'identity
       (cl-loop for name in names
                for i to (length names)
                collect
                (propertize (format " [%d] %s " (1+ i) name)
                            'face (if (equal current-name name)
                                      '+persp-tab-selected-face
                                    '+persp-tab-face)))
       " ")))

  (defun +persp-display ()
    (interactive)
    (let (message-log-max)
      (minibuffer-message "%s" (+persp--tabline))))

  (defun +persp--message-body (message &optional type)
    (concat (+persp--tabline)
            (propertize " | " 'face 'font-lock-comment-face)
            (propertize (format "%s" message)
                        'face (pcase type
                                ('error 'error)
                                ('warn 'warning)
                                ('success 'success)
                                ('info 'font-lock-comment-face)))))

  (defun +persp-message (message &optional type)
    (message "%s" (+persp--message-body message type)))

  (defun +persp-error (message &optional noerror)
    (funcall (if noerror #'message #'error)
             "%s" (+persp--message-body message 'error)))

  (setq persp-auto-kill-buffer-on-remove 'kill-weak
        persp-nil-hidden t
        persp-keymap-prefix nil
        persp-auto-save-fname "autosave"
        persp-save-dir (concat %etc-dir "workspaces/")
        persp-set-last-persp-for-new-frames t
        persp-switch-to-added-buffer nil
        persp-remove-buffers-from-nil-persp-behaviour nil
        persp-auto-resume-time -1
        persp-auto-save-opt 1
        persp-add-buffer-on-find-file t
        persp-add-buffer-on-after-change-major-mode t
        persp-init-frame-behaviour t
        persp-init-new-frame-behaviour-override nil
        persp-interactive-init-frame-behaviour-override #'+persp-associate-frame
        persp-emacsclient-init-frame-behaviour-override #'+persp-associate-frame)

  (defun +persp-autosave-real-buffers (orig-fn &rest args)
    "Don't autosave if no real buffers are open."
    (when (real-buffer-list)
      (apply orig-fn args))
    t)
  (advice-add #'persp-asave-on-exit :around #'+persp-autosave-real-buffers)

  (add-hook 'persp-add-buffer-on-after-change-major-mode-filter-functions #'unreal-buffer-p)

  (defun +persp-config ()
    (cond (persp-mode
           (remove-hook 'kill-buffer-query-functions 'persp-kill-buffer-query-function)
           (add-hook 'kill-buffer-query-functions 'persp-kill-buffer-query-function t)
           (advice-add #'+persp-buffer-list :override #'+persp--buffer-list))
          ((advice-remove #'+persp-buffer-list #'+persp--buffer-list))))
  (add-hook 'persp-mode-hook #'+persp-config)

  (defun +persp-leave-nil-perspective (&rest _)
    (when (string= (+persp-current-name) persp-nil-name)
      (+persp--switch (or (perspective-p +persp--last) +persp--last)
                         (car (+persp-list-names))
                         +persp-default-perspective)
                     'auto-create))
  (add-hook 'persp-after-load-state-functions #'+persp-leave-nil-perspective)
  
  (defun +persp-delete-associated-workspace (&optional frame)
    (when persp-mode
      (unless frame
        (setq frame (selected-frame)))
      (let ((frame-persp (frame-parameter frame 'workspace)))
        (when (string= frame-persp (+persp-current-name))
          (+persp-delete frame-perp)))))
  (add-hook 'delete-frame-functions #'+persp-delete-associated-perspective)

  (defun +persp-cleanup-associated-buffers ()
    (when persp-mode
      (cl-loop for buf in (+persp--buffer-list)
               unless (or (persp--buffer-in-persps buf)
                          (get-buffer-window buf))
               if (kill-buffer buf)
               sum 1)))
  (add-hook 'cleanup-hook #'+persp-cleanup-associated-buffers)

  (defun +persp-associate-frame (frame &optional _new-frame-p)
    (when persp-mode
      (if (not (persp-frame-list-without-daemon))
          (+persp--switch +persp-default-perspective t)
        (with-selected-frame frame
          (+persp--switch (format "#%s" (+persp--generate-id)) t)
          (unless (real-buffer-p (current-buffer))
            (switch-to-buffer (fallback-buffer)))
          (set-frame-parameter frame 'workspace (+persp-current-name))
          (persp-set-frame-buffer-predicate frame))
        (run-at-time 0.1 nil #'+persp-display))))

  (defun +persp-switch-project-by-name (orig-fn &rest args)
    (when persp-mode
      (+persp--switch (car args t))
      (with-current-buffer (switch-to-buffer (fallback-buffer))
        (setq default-directory (car args))))
    (apply orig-fn args))

  (defvar +persp--project-dir nil)
  (defun +persp-set-project-action  ()
    (setq +persp--project-dir default-directory))

  (defun +persp-switch-to-project (&optional dir)
    (when dir
      (setq +persp--project-dir dir))
    (when (and persp-mode +persp--project-dir)
      (unwind-protect
          (if (+persp--buffer-list)
              (let* (persp-p
                     (persp
                      (let* ((default-directory +persp--project-dir)
                             (project-name (+projectile-project-name 'nocache)))
                        (or (setq persp-p (+persp-get project-name t))
                            (+persp--new project-name))))
                     (new-name (persp-name persp)))
                (+persp--switch new-name)
                (unless persp-p
                  (switch-to-buffer (fallback-buffer)))
                (with-current-buffer (fallback-buffer)
                  (setq default-directory +persp--project-dir))
                (unless current-prefix-arg
                  (funcall +persp-switch-project-function +persp--project-dir))
                (+persp-message
                 (format "Switched to '%s' in new workspace" new-name)
                 'success))
            (with-current-buffer (switch-to-buffer (fallback-buffer))
              (setq default-directory +persp--project-dir)
              (message "Switched to '%s'" (+projectile-project-name 'nocache)))
            (unless current-prefix-arg
              (funcall +persp-switch-project-function +persp--project-dir)))
        (setq +persp--project-dir nil))))
  
  (after! projectile
    (setq projectile-switch-project-action #'+persp-set-project-action)
    (add-hook 'projectile-after-switch-project-hook #'+persp-switch-to-project))

  (defun +persp-ignore-errors-on-kill-emacs (orig-fn)
    (ignore-errors (funcall orig-fn)))
  (advice-add #'persp-kill-emacs-h :around #'+persp-ignore-errors-on-kill-emacs)

  (persp-def-buffer-save/load
   :mode 'eshell-mode
   :tag-symbol 'def-eshell-buffer
   :save-vars '(major-mode default-directory))
  (persp-def-buffer-save/load
   :mode 'compilation-mode
   :tag-symbol 'def-compilation-buffer
   :save-vars '(major-mode
                default-directory
                compilation-directory
                compilation-environment
                compilation-arguments))

  (defvar +persp--indirect-buffers-to-restore nil)
  (persp-def-buffer-save/load
   :tag-symbol 'def-indirect-buffer
   :predicate #'buffer-base-buffer
   :save-function (lambda (buf tag vars)
                    (list tag (buffer-name buf) vars
                          (buffer-name (buffer-base-buffer))))
   :load-function (lambda (savelist &rest _rest)
                    (cl-destructuring-bind (buf-name _vars base-buf-name &rest _)
                        (cdr savelist)
                      (push (cons buf-name base-buf-name)
                            +persp--indirect-buffers-to-restore)
                      nil)))

  (defun +persp-reload-indirect-buffers (&rest _)
    (dolist (ibc +persp--indirect-buffers-to-restore)
      (let* ((nbn (car ibc))
             (bbn (cdr ibc))
             (bb (get-buffer bbn)))
        (when bb
          (when (get-buffer nbn)
            (setq nbn (generate-new-buffer-name nbn)))
          (make-indirect-buffer bb nbn t))))
    (setq +persp--indirect-buffers-to-restore nil))
  (add-hook 'persp-after-load-state-functions #'+persp-reload-indirect-buffers)

  (define-key! persp-mode-map
    [remap delete-window] #'+persp-close-window-or-workspace
    [remap evil-delete-window] #'+persp-close-window-or-workspace)

  ;; (general-def
  ;;   :states 'normal
  ;;   :prefix "SPC"
  ;;   :prefix-map 'leader-map
  ;;   "TAB" '(:keymap +persp-prefix-map :wk "workspaces"))

  ;; (general-def
  ;;   :keymaps '+persp-prefix-map
  ;;   "TAB" '(+persp-display      :wk "Display tab bar")
  ;;   "n"   '(+persp-switch-right :wk "Next workspace")
  ;;   "p"   '(+persp-switch-left  :wk "Previous workspace")
  ;;   "N"   '(+persp-new          :wk "New workspace")
  ;;   "r"   '(+persp-rename       :wk "Rename workspace")
  ;;   "s"   '(+persp-save         :wk "Save workspace")
  ;;   "S"   '(+persp-save-session :wk "Save session")
  ;;   "l"   '(+persp-load         :wk "Load workspace")
  ;;   "L"   '(+persp-load-session :wk "Load session")
  ;;   "x"   '(+persp-delete       :wk "Delete workspace")
  ;;   "X"   '(+persp-kill-session :wk "Kill session")
  ;;   "C-x" '(+persp-kill-session-and-quit :wk "Kill session and quit")
  ;;   "."   '(+persp-switch-to             :wk "Switch workspace")
  ;;   "1"   '((lambda! (+persp-switch-to 0))
  ;;           :wk "Switch to workspace 1")
  ;;   "2"   '((lambda! (+persp-switch-to 1))
  ;;           :wk "Switch to workspace 2")
  ;;   "3"   '((lambda! (+persp-switch-to 2))
  ;;           :wk "Switch to workspace 3")
  ;;   "4"   '((lambda! (+persp-switch-to 3))
  ;;           :wk "Switch to workspace 4")
  ;;   "5"   '((lambda! (+persp-switch-to 4))
  ;;           :wk "Switch to workspace 5")
  ;;   "6"   '((lambda! (+persp-switch-to 5))
  ;;           :wk "Switch to workspace 6")
  ;;   "7"   '((lambda! (+persp-switch-to 6))
  ;;           :wk "Switch to workspace 7")
  ;;   "8"   '((lambda! (+persp-switch-to 7))
  ;;           :wk "Switch to workspace 8")
  ;;   "9"   '((lambda! (+persp-switch-to 8))
  ;;           :wk "Switch to workspace 9")
  ;;   "0"   '(+persp-switch-to-last :wk "Switch to last workspace"))
  )

(provide 'config-workspaces)
;;; config-workspaces.el ends here

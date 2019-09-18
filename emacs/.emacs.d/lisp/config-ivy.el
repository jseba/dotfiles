;;; config-ivy.el --- -*- lexical-binding: t -*-

(use-package ivy
  :defer 1
  :after-call pre-command-hook
  :bind ((:map ivy-mode-map
	       ([remap switch-to-buffer] . +ivy-switch-buffer)
	       ([remap switch-to-buffer-other-window] . +ivy-switch-buffer-other-window)))
  :init
  (setq ivy-re-builders-alist
	'((counsel-ag . ivy--regex-plus)
	  (counsel-rg . ivy--regex-plus)
	  (counsel-grep . ivy--regex-plus)
	  (swiper . ivy--regex-plus)
	  (swiper-isearch . ivy--regex-plus)
	  (t . ivy--regex-ignore-order)))
  (ivy-mode +1)
  :config
  (setq ivy-sort-max-size 50000
	ivy-use-virtual-buffers nil
	ivy-use-selectable-prompt t
	ivy-on-del-error-function nil
	ivy-virtual-abbreviate 'full
	ivy-count-format ""
	ivy-height 15
	ivy-wrap t
	ivy-magic-slash-non-match-action nil
	ivy-do-completion-in-region t
	ivy-fixed-height-minibuffer t
	ivy-initial-inputs-alist nil
	ivy-format-functions-alist '((t . ivy-format-function-default)))

  (defadvice! +ivy--counsel-file-jump-use-fd-rg (args)
    "Change `counsel-file-jump' to use fd or rg."
    :override #'counsel--find-return-list
    (cl-destructuring-bind (find-program . args)
	(cond ((executable-find "fd")
	       (cons "fd" (list "-t" "f" "-E" ".git")))
	      ((executable-find "rg")
	       (cons "rg" (list "--files" "--hidden" "--no-messages")))
	      ((cons find-program args)))
      (unless (listp args)
	(user-error "`counsel-file-jump-args' should be a list"))
      (counsel--call
       (cons find-program args)
       (lambda ()
	 (goto-char (point-min))
	 (let ((offset (if (member find-program '("fd" "rg")) 0 2))
	       files)
	   (while (< (point) (point-max))
	     (push (buffer-substring
		    (+ offset (line-beginning-position)) (line-end-position))
		   files)
	     (forward-line 1))
	   (nreverse files))))))

  (defvar +ivy-buffer-preview nil
    "If non-nil, preview buffers while switching.")
  (defun +ivy--switch-buffer-preview ()
    (let (ivy-use-virtual-buffers ivy--virtual-buffers)
      (counsel--switch-buffer-update-fn)))

  (defalias '+ivy--switch-buffer-preview-all #'counsel--switch-buffer-update-fn)
  (defalias '+ivy--switch-buffer-unwind #'counsel--switch-buffer-unwind)

  (defun +ivy--switch-buffer (other)
    (let ((current (not other))
	  prompt action update unwind)
      (cond (current
	     (setq prompt "Switch to buffer: "
		   action #'ivy--switch-buffer-action))
	    ((setq prompt "Switch to buffer in other window: "
		   action #'ivy--switch-buffer-other-window-action)))
      (when +ivy-buffer-preview
	(cond ((not (and ivy-use-virtual-buffers
			 (eq +ivy-buffer-preview 'everything)))
	       (setq update #'+ivy--switch-buffer-preview
		     unwind #'+ivy--switch-buffer-unwind))
	      ((setq update #'+ivy--switch-buffer-preview-all
		     unwind #'+ivy--switch-buffer-unwind))))
      (ivy-read prompt 'internal-complete-buffer
		:action action
		:predicate nil
		:update-fn update
		:unwind unwind
		:preselect (buffer-name (other-buffer (current-buffer)))
		:keymap ivy-switch-buffer-map
		:caller #'ivy-switch-buffer)))

  (defun +ivy-switch-buffer ()
    "Switch to another buffer."
    (interactive)
    (+ivy--switch-buffer nil))
  (defun +ivy-switch-buffer-other-window ()
    "Switch to another buffer in another window."
    (interactive)
    (+ivy--switch-buffer t))

  (defvar +ivy-task-tags '(("TODO" . warning)
			   ("FIXME" . error))
    "An alist of tags for `+ivy-tasks' to include in its search, whose
CDR is the face to render it with.")

  (defun +ivy--tasks-candidates (tasks)
    "Generate a list of task tags (specified by `+ivy-task-tags') from TASKS
for `+ivy-tasks'."
    (let* ((max-type-width
	    (cl-loop for task in +ivy-task-tags maximize (length (car task))))
	   (max-desc-width
	    (cl-loop for task in tasks maximize (length (cl-cdadr task))))
	   (max-width (max (+ max-desc-width 3)
			   25)))
      (cl-loop
       with fmt = (format "%%-%ds %%-%ds%%s:%%s" max-type-width max-width)
       for alist in tasks
       collect
       (let-alist alist
	 (list (format fmt
		       (propertize .type 'face (cdr (assoc .type +ivy-task-tags)))
		       (substring .desc 0 (min max-desc-width (length .desc)))
		       (propertize (abbreviate-file-name .file) 'face 'font-lock-keyword-face)
		       (propertize .line 'face 'font-lock-constant-face))
	       .type .file .line)))))

  (defun +ivy--tasks (target)
    (let* ((task-tags (mapcar #'car 'ivy-task-tags))
	   (cmd (format "%s -H -S --no-heading -- %s %s"
			(or (when-let (bin (executable-find "rg"))
			      (concat bin " --line-number"))
			    (when-let (bin (executable-find "ag"))
			      (concat bin " --numbers"))
			    (error "unable to find rg or ag"))
			(shell-quote-argument
			 (concat "\\s("
				 (string-join task-tags "|")
				 ")([\\s:]:\\([^)]+\\):?)"))
			target))
	   case-fold-search)
      (save-match-data
	(cl-loop with out = (shell-command-to-string cmd)
		 for x in (and out (split-string out "\n" t))
		 when (condition-case-unless-debug ex
			  (string-match
			   (concat "^\\([^:]+\\):\\([0-9]+\\):.+\\("
				   (string-join task-tags "\\|")
				   "\\):?\\s-*\\(.+\\)")
			   x)
			(error
			 (user-error "Error matching task in file: (%s) %s"
				     (error-message-string ex)
				     (car (split-string x ":")))
			 nil))
		 collect `((type . ,(match-string 3 x))
			   (desc . ,(match-string 4 x))
			   (file . ,(match-string 1 x))
			   (line . ,(match-string 2 x)))))))

  (defun +ivy--tasks-open-action (x)
    "Jump to the file and line of the current task."
    (cl-destructuring-bind (label type file line) x
      (with-ivy-window
	(find-file (expand-file-name file (+projectile-project-root)))
	(goto-char (point-min))
	(forward-line (1- (string-to-number line)))
	(when (search-forward type (line-end-position) t)
	  (backward-char (length type)))
	(recenter))))

  (defun +ivy-tasks (&optional arg)
    "Search through all TODO/FIXME tags in the current project.

If ARG, only search the current file."
    (interactive "P")
    (ivy-read (format "Tasks (%s): "
		      (if arg
			  (concat "in " (file-relative-name buffer-file-name))
			"project"))
	      (let ((tasks (+ivy--tasks
			    (if arg
				buffer-file-name
			      (+projectile-project-root)))))
		(unless tasks
		  (user-error "No tasks found"))
		(+ivy--tasks-candidates tasks))
	      :action #'ivy--tasks-open-action
	      :caller '+ivy-tasks)))      

(use-package ivy-rich
  :after ivy
  :config
  (cl-pushnew '(+ivy-rich-buffer-icon)
	      (cadr (plist-get ivy-rich-display-transformers-list
			       'ivy-switch-buffer)))

  (defvar +ivy-buffer-unreal-face 'font-lock-comment-face
    "Face to use for unreal buffers in `ivy-switch-to-buffer'.")
  
  (defun +ivy-rich-buffer-name (candidate)
    "Display the buffer name.

Buffers that are considered unreal are dimmed with `+ivy-buffer-unreal-face'."
    (let ((b (get-buffer candidate)))
      (when (null uniquify-buffer-name-style)
	(when-let* ((file-path (buffer-file-name b))
		    (uniquify-buffer-name-style 'forward))
	  (setq candidate
		(uniquify-get-proposed-name
		 (replace-regexp-in-string "<[0-9]+>$" "" (buffer-name b))
		 (directory-file-name
		  (if file-path
		      (file-name-directory file-path)
		    default-directory))
		 1))))
      (cond ((ignore-errors
	       (file-remote-p
		(buffer-local-value 'default-directory b)))
	     (ivy-append-face candidate 'ivy-remote))
	    ((unreal-buffer-p b)
	     (ivy-append-face candidate '+ivy-buffer-unreal-face))
	    ((not (buffer-file-name b))
	     (ivy-append-face candidate 'ivy-subdir))
	    ((buffer-modified-p b)
	     (ivy-append-face candidate 'ivy-modified-buffer))
	    (candidate))))

  (defun +ivy-rich-buffer-icon (candidate)
    "Display the icon for CANDIDATE buffer."
    (propertize "\t" 'display
		(if-let* ((buffer (get-buffer candidate))
			  (mode (buffer-local-value 'major-mode buffer)))
		    (or
		     (all-the-icons-ivy--icon-for-mode mode)
		     (all-the-icons-ivy--icon-for-mode (get mode 'derived-mode parent))
		     (funcall
		      all-the-icons-ivy-family-fallback-for-buffer
		      all-the-icons-ivy-name-fallback-for-buffer))
		  (all-the-icons-icon-for-file candidate))))

  (defun +ivy-rich-describe-variable-transformer (candidate)
    "Previews the value of the CANDIDATE in the minibuffer."
    (let* ((sym (intern candidate))
	   (val (and (boundp sym) (symbol-value sym)))
	   (print-level 3))
      (replace-regexp-in-string
       "[\n\t\^[\^M\^@\^G]" " "
       (cond ((booleanp val)
	      (propertize (format "%s" val)
			  'face (if (null val)
				    'font-lock-comment-face
				  'success)))
	     ((symbolp val)
	      (propertize (format "'%s" val)
			  'face 'highlight-quoted-symbol))
	     ((keymapp val)
	      (propertize "<keymap>"
			  'face 'font-lock-constant-face))
	     ((listp val)
	      (prin1-to-string val))
	     ((stringp val)
	      (propertize (format "%S" val)
			  'face 'font-lock-string))
	     ((numberp val)
	      (propertize (format "%s" val)
			  'face 'highlight-numbers-number))
	     ((format "%s" val)))
       t)))
  
  (setq ivy-rich-display-transformers-list
	(plist-put ivy-rich-display-transformers-list
		   'counsel-describe-variable
		   '(:columns
		     ((counsel-describe-variable-transformer (:width 40))
		      (+ivy-rich-describe-variable-transformer (:width 50)
		      (ivy-rich-counsel-variable-docstring (:face font-lock-doc-face))))))
	ivy-switch-buffer-faces-alist nil)
  (ivy-set-display-transformer 'internal-complete-buffer nil)

  (let* ((plist (plist-get ivy-rich-display-transformers-list 'ivy-switch-buffer))
	 (switch-buffer-alist (assq 'ivy-rich-candidate (plist-get plist :columns))))
    (when switch-buffer-alist
      (setcar switch-buffer-alist '+ivy-rich-buffer-name)))

  (ivy-rich-mode +1))

(use-package all-the-icons-ivy
  :after ivy
  :config
  (setq all-the-icons-buffer-commands nil)

  (all-the-icons-ivy-setup)
  
  (after! counsel-projectile
    (let ((all-the-icons-ivy-file-commands '(counsel-projectile
					     counsel-projectile-find-file
					     counsel-projectile-find-dir)))
      (all-the-icons-setup))))
    
(use-package counsel
  :commands counsel-describe-face
  :bind (([remap apropos] . counsel-apropos)
	 ([remap bookmark-jump] . counsel-bookmark)
	 ([remap describe-face] . counsel-faces)
	 ([remap describe-function] . counsel-describe-function)
	 ([remap describe-variable] . counsel-describe-variable)
	 ([remap describe-bindings] . counsel-descbinds)
	 ([remap set-variable] . counsel-set-variable)
	 ([remap execute-extended-command] . counsel-M-x)
	 ([remap find-file] . counsel-find-file)
	 ([remap find-library] . counsel-find-library)
	 ([remap info-lookup-symbol] . counsel-info-lookup-symbol)
	 ([remap imenu] . counsel-imenu)
	 ([remap recentf-open-files] . counsel-recentf)
	 ([remap org-capture] . counsel-org-capture)
	 ([remap swiper] . counsel-grep-or-swiper)
	 ([remap yank-pop] . counsel-yank-pop))
  :config
  (+popup-set-rule "^\\*ivy-occur" :size 0.35 :ttl 0 :quit nil)

  (setq counsel-find-file-ignore-regexp "\\(?:^[#.]\\)\\|\\(?:[#~]$\\)\\|\\(?:^Icon?\\)"
	counsel-describe-function-function #'helpful-callable
	counsel-describe-variable-function #'helpful-variable
	counsel-rg-base-command "rg -S --no-heading --line-number --color never %s ."))

(use-package counsel-projectile
  :bind (([remap projectile-find-file] . +ivy-projectile-find-file)
	 ([remap projectile-find-dir] . counsel-projectile-find-dir)
	 ([remap projectile-switch-to-buffer] . counsel-projectile-switch-to-buffer)
	 ([remap projectile-grep] . counsel-projectile-grep)
	 ([remap projectile-ag] . counsel-projectile-ag)
	 ([remap projectile-switch-project] . counsel-projectile-switch-project))
  :config
  (defun +ivy-projectile-find-file ()
    "A more sensible `counsel-projectile-find-file' that will revert to
`counsel-find-file' if invoked from $HOME, `counsel-file-jump' if from a
non-project, `projectile-find-file' if in a project with more than
`ivy-sort-max-size' files or `counsel-projectile-find-file' otherwise."
    (interactive)
    (call-interactively
     (cond ((or (file-equal-p default-directory "~")
		(when-let (proj-root (+projectile-project-root))
		  (file-equal-p proj-root "~")))
	    #'counsel-find-file)
	   ((+projectile-project-p)
	    (let ((files (projectile-current-project-files)))
	      (if (<= (length files) ivy-sort-max-size)
		  #'counsel-projectile-find-file
		#'projectile-find-file)))
	   (#'counsel-file-jump))))
  
  (ivy-set-display-transformer #'counsel-projectile-find-file nil))

(use-package ivy-prescient
  :after ivy
  :hook (ivy-mode . ivy-prescient-mode)
  :commands ivy-prescient-re-builder
  :init
  (setq prescient-filter-method '(literal regexp initialism)
	prescient-save-file (no-littering-expand-var-file-name "prescient-save.el")
	ivy-prescient-enable-filtering nil
	ivy-prescient-retain-classic-highlighting t
	ivy-initial-inputs-alist nil
	ivy-re-builders-alist '((counsel-ag . +ivy-prescient-non-fuzzy)
				(counsel-rg . +ivy-prescient-non-fuzzy)
				(counsel-grep . +ivy-prescient-non-fuzzy)
				(swiper . +ivy-prescient-non-fuzzy)
				(swiper-isearch . +ivy-prescient-non-fuzzy)
				(t . ivy-prescient-re-builder)))
  :config
  (defun +ivy-prescient-non-fuzzy (str)
    (let ((prescient-filter-method '(literal regexp)))
      (ivy-prescient-re-builder str)))
  (prescient-persist-mode +1))

(provide 'config-ivy)
;;; config-ivy.el ends here

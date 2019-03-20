;;; config-evil.el

(use-package evil
  :preface
  (defvar evil-want-C-u-scroll t)
  (defvar evil-want-C-w-scroll t)
  (defvar evil-want-Y-yank-to-eol t)

  :init
  (defvar +evil--default-cursor-color
    (or (ignore-errors (frame-parameter nil 'cursor-color))
        "#ffffff"))

  (setq evil-want-visual-char-semi-exclusive t
        evil-magic t
        evil-echo-state t
        evil-indent-convert-tabs t
        evil-ex-search-vim-style-regexp t
        evil-ex-substitute-global t
        evil-ex-visual-char-range t
        evil-insert-skip-empty-lines t
        evil-mode-line-format 'nil
        evil-respect-visual-line-mode t
        evil-symbol-word-search t
        shift-select-mode nil
        evil-default-cursor '+evil-default-cursor
        evil-normal-state-cursor 'box
        evil-emacs-state-cursor '(box '+evil-emacs-cursor)
        evil-insert-state-cursor 'bar
        evil-visual-state-cursor 'hollow
        save-silently t
        evil-move-cursor-back nil
        evil-cross-lines t
        evil-want-keybinding nil)

  (add-hook 'init-hook #'evil-mode)
  (defun +evil-set-popup-rules ()
    (+popup-set-rules '(("^\\*evil-registers*" :size 0.3)
                        ("^\\*Command line"    :size 8))))
  (add-hook 'init-hook #'+evil-set-popup-rules)

  :config
  (evil-select-search-module 'evil-search-module 'evil-search)

  (put 'evil-define-key* 'lisp-indent-function 'defun)

  (defun +evil-default-cursor () (set-cursor-color +evil--default-cursor-color))
  (defun +evil-emacs-cursor ()   (set-cursor-color (face-foreground 'warning)))

  (defun +evil-update-cursor-color ()
    (setq +evil--default-cursor-color (face-background 'cursor)))
  (add-hook 'load-theme-hook #'+evil-update-cursor-color)

  (defun +evil-update-shift-width ()
    (setq evil-shift-width tab-width))
  (add-hook 'after-change-major-mode-hook #'+evil-update-shift-width t)

  (define-key! 'global
    [remap evil-toggle-fold]   #'+evil-fold-toggle
    [remap evil-close-fold]    #'+evil-fold-close
    [remap evil-open-fold]     #'+evil-fold-open
    [remap evil-open-fold-rec] #'+evil-fold-open
    [remap evil-close-folds]   #'+evil-fold-close-all
    [remap evil-open-folds]    #'+evil-fold-open-all)

  (defun +evil-disable-highlights ()
    (when (evil-ex-hl-active-p 'evil-ex-search)
      (evil-ex-nohighlight)
      t))
  (add-hook 'global-escape-hook #'+evil-disable-highlights)

  (defun +evil-display-vimish-save-message ()
    (message "\"%s\" %dL, %dC written"
             (if buffer-file-name
                 (file-relative-name
                  (file-truename buffer-file-name)
                  (+projectile-project-root))
               (buffer-name))
             (count-lines (point-min) (point-max))
             (buffer-size)))
  (add-hook 'after-save-hook #'+evil-display-vimish-save-message)

  (defun +evil-escape (&rest _)
    "Call `global-escape' if `evil-force-normal-state' is called interactively."
    (when (called-interactively-p 'any)
      (call-interactively #'global-escape)))
  (advice-add #'evil-force-normal-state :after #'+evil-escape)

  (defun +evil-static-reindent (orig-fn &rest args)
    "Don't move cursor on reindent."
    (save-excursion (apply orig-fn args)))
  (advice-add #'evil-indent :around #'+evil-static-reindent)

  (defun +evil-resolve-vim-path (file-name)
    (let* (case-fold-search
           (regexp (concat "\\(?:^\\|[^\\\\]\\)"
                           "\\([#%]\\)"
                           "\\(\\(?::\\(?:[phtreS~.]\\|g?s[^:\t\n ]+\\)\\)*\\)"))
           (matches
            (cl-loop with i = 0
                     while (and (< i (length file-name))
                                (string-match regexp file-name i))
                     do (setq i (1+ (match-beginning 0)))
                     and collect
                     (cl-loop for j to (/ (length (match-data)) 2)
                              collect (match-string j file-name)))))
      (dolist (match matches)
        (let ((flags (split-string (car (cdr (cdr match))) ":" t))
              (path (and buffer-file-name
                         (pcase (car (cdr match))
                           ("%" (file-relative-name buffer-file-name))
                           ("#" (save-excursion (other-window 1) (file-relative-name buffer-file-name))))))
              flag global)
          (if (not path)
              (setq path "")
            (while flags
              (setq flag (pop flags))
              (when (string-suffix-p "\\" flag)
                (setq flag (concat flag (pop flags))))
              (when (string-prefix-p "gs" flag)
                (setq global t
                      flag (substring flag 1)))
              (setq path
                    (or (pcase (substring flag 0 1)
                          ("p" (expand-file-name path))
                          ("~" (concat "~/" (file-relative-name path "~")))
                          ("." (file-relative-name path default-directory))
                          ("t" (file-name-nondirectory (directory-file-name path)))
                          ("r" (file-name-sans-extension path))
                          ("e" (file-name-extension path))
                          ("S" (shell-quote-argument path))
                          ("h"
                           (let ((parent (file-name-directory (expand-file-name path))))
                             (unless (equal (file-truename path)
                                            (file-truename parent))
                               (if (file-name-absolute-p path)
                                   (directory-file-name parent)
                                 (file-relative-name parent)))))
                          ("s"
                           (when-let* ((args (evil-delimited-arguments (substring flag 1) 2)))
                             (let ((pattern (evil-transform-vim-style-regexp (car args)))
                                   (replace (cadr args)))
                               (replace-regexp-in-string
                                (if global pattern (concat "\\(" pattern "\\).*\\'"))
                                (evil-transform-vim-style-regexp replace)
                                path t t
                                (unless global 1)))))
                          (_ path))
                        "")))
            (when (and (not (string= path "")) (equal (substring path -1) "/"))
              (setq path (substring path 0 -1))))
          (setq file-name
                (replace-regexp-in-string (format "\\(?:^\\|[^\\\\]\\)\\(%s\\)"
                                                  (regexp-quote (string-trim-left (car match))))
                                          path
                                          file-name
                                          t t 1))))
      (replace-regexp-in-string regexp "\\1" file-name t)))
  (advice-add #'evil-ex-replace-special-filenames :override #'+evil-resolve-vim-path)

  (defun +evil-fix-dabbrev-in-minibuffer ()
    (set-syntax-table (let* ((table (make-syntax-table)))
                        (modify-syntax-entry ?/ "." table)
                        table)))
  (add-hook 'minibuffer-inactive-mode-hook #'+evil-fix-dabbrev-in-minibuffer)

  (evil-define-command +evil-window-split (&optional count file)
    "Same as `evil-window-split', but focuses and recenters the new split."
    :repeat nil
    (interactive "P<f>")
    (split-window (selected-window) count
                  (if evil-split-window-below 'above 'below))
    (call-interactively
     (if evil-split-window-below
         #'evil-window-up
       #'evil-window-down))
    (recenter)
    (when (and (not count) evil-auto-balance-windows)
      (balance-windows (window-parent)))
    (if file (evil-edit file)))
  (advice-add #'evil-window-split  :override #'+evil-window-split)

  (evil-define-command +evil-window-vsplit (&optional count file)
    "Same as `evil-window-vsplit', but focuses and recenters the new split."
    :repeat nil
    (interactive "P<f>")
    (split-window (selected-window) count
                  (if evil-vsplit-window-right 'left 'right))
    (call-interactively
     (if evil-vsplit-window-right
         #'evil-window-left
       #'evil-window-right))
    (recenter)
    (when (and (not count) evil-auto-balance-windows)
      (balance-windows (window-parent)))
    (if file (evil-edit file)))
  (advice-add #'evil-window-vsplit :override #'+evil-window-vsplit)

  (defun +evil-set-jump (orig-fn &rest args)
    (evil-set-jump)
    (let ((evil--jumps-jumping t))
      (apply orig-fn args)))

  (defun +evil-make-numbered-markers-global (orig-fn char)
    (or (and (>= char ?2) (<= char ?9))
        (funcall orig-fn char)))
  (advice-add #'evil-global-marker-p :around #'+evil-make-numbered-markers-global)

  (defun +evil-insert-newline-above-and-respect-comments (orig-fn count)
    (cl-letf* ((old-insert-newline-above (symbol-function 'evil-insert-newline-above))
               ((symbol-function 'evil-insert-newline-above)
                (lambda ()
                  (if (evil-insert-state-p)
                      (funcall old-insert-newline-above)
                    (let ((pos (save-excursion (beginning-of-line-text) (point))))
                      (evil-narrow-to-field
                       (if (save-excursion (nth 4 (syntax-ppss pos)))
                           (evil-save-goal-column
                            (setq evil-auto-indent nil)
                            (goto-char pos)
                            (let ((ws (abs (skip-chars-backward " \t"))))
                              (save-excursion
                                (if comment-line-break-function
                                    (funcall comment-line-break-function)
                                  (comment-indent-new-line))
                                (when (and (derived-mode-p 'c-mode
                                                           'c++-mode
                                                           'objc-mode
                                                           'java-mode
                                                           'js2-mode)
                                           (eq (char-after) ?/))
                                  (insert "*"))
                                (insert
                                 (make-string (max 0 (+ ws (skip-chars-backward " \t")))
                                              32)))
                              (insert (make-string (max 1 ws) 32))))
                         (evil-move-beginning-of-line)
                         (insert (if use-hard-newlines hard-newline "\n"))
                         (forward-line -1)
                         (back-to-indentation))))))))
      (let ((evil-auto-indent evil-auto-indent))
        (funcall orig-fn count))))
  (advice-add #'evil-open-above :around #'+evil-insert-newline-above-and-respect-comments)

  (defun +evil-insert-newline-below-and-respect-comments (orig-fn count)
    (cl-letf* ((old-insert-newline-below (symbol-function 'evil-insert-newline-below))
               ((symbol-function 'evil-insert-newline-below)
                (lambda ()
                  (if (evil-insert-state-p)
                      (funcall old-insert-newline-below)
                    (let ((pos (save-excursion (beginning-of-line-text) (point))))
                      (evil-narrow-to-field
                       (evil-move-end-of-line)
                       (cond ((sp-point-in-comment pos)
                              (setq evil-auto-indent nil)
                              (if comment-line-break-function
                                  (funcall comment-line-break-function)
                                (comment-indent-new-line)))
                             (t
                              (insert (if use-hard-newlines hard-newline "\n"))
                              (back-to-indentation)))))))))
      (let ((evil-auto-indent evil-auto-indent))
        (funcall orig-fn count))))
  (advice-add #'evil-open-below :around #'+evil-insert-newline-below-and-respect-comments)

  (defvar +evil--flag nil)
  (defun +evil--ex-match-init (name &optional face update-hook)
    (with-current-buffer evil-ex-current-buffer
      (cond ((eq +evil--flag 'start)
             (evil-ex-make-hl name
                              :face (or face 'evil-ex-substitute-matches)
                              :update-hook (or update-hook #'evil-ex-pattern-update-ex-info))
             (setq +evil--flag 'update))
            ((eq +evil--flag 'stop)
             (evil-ex-delete-hl name)))))

  (defun +evil--ex-buffer-match (arg &optional hl-name flags beg end)
    (when (and (eq +evil--flag 'update)
               evil-ex-substitute-highlight-all
               (not (zerop (length arg))))
      (condition-case lossage
          (let ((pattern (evil-ex-make-substitute-pattern
                          arg
                          (or flags (list))))
                (range (or (evil-copy-range evil-ex-range)
                           (evil-range (or beg (line-beginning-position))
                                       (or end (line-end-position))
                                       'line
                                       :expanded t))))
            (evil-expand-range range)
            (evil-ex-hl-set-region hl-name
                                   (max (evil-range-beginning range) (window-start))
                                   (min (evil-range-end range) (window-end)))
            (evil-ex-hl-change hl-name pattern))
        (end-of-file
         (evil-ex-pattern-update-ex-info nil "Incomplete replacement"))
        (user-error
         (evil-ex-pattern-update-ex-info nil (format "?%s" lossage))))))

  (defun +evil-ex-buffer-match (flag &optional arg)
    (let ((hl-name 'evil-ex-buffer-match)
          (+evil--flag flag))
      (with-selected-window (minibuffer-selected-window)
        (+evil--ex-match-init hl-name)
        (+evil--ex-buffer-match arg hl-name (list (if evil-ex-substitute-global ?g))))))

  (defun +evil-ex-global-match (flag &optional arg)
    (let ((hl-name 'evil-ex-global-match)
          (+evil--flag flag))
      (with-selected-window (minibuffer-selected-window)
        (+evil--ex-match-init hl-name)
        (+evil-ex-buffer-match arg hl-name nil (point-min) (point-max)))))
  
  (evil-ex-define-argument-type buffer-match :runner +evil-ex-buffer-match)
  (evil-ex-define-argument-type global-match :runner +evil-ex-global-match)
  (evil-define-interactive-code "<//>"
                                :ex-arg buffer-match (list (if (evil-ex-p) evil-ex-argument)))
  (evil-define-interactive-code "<//g>"
                                :ex-arg global-match (list (if (evil-ex-p) evil-ex-argument)))

  (defun +evil-ex-global-delim-match (flag &optional arg)
    (let ((hl-name 'evil-ex-global-delim-match)
          (+evil--flag flag))
      (with-selected-window (minibuffer-selected-window)
        (+evil--ex-match-init hl-name)
        (let ((result (car-safe (evil-delimited-arguments arg 2))))
          (+evil--ex-buffer-match result hl-name nil (point-min) (point-max))))))
  (evil-ex-define-argument-type global-delim-match :runner +evil-ex-global-delim-match)
  (dolist (sym '(evil-ex-global evil-ex-global-inverted))
    (evil-set-command-property sym :ex-arg 'global-delim-match))

  (evil-define-operator +evil-align (beg end pattern &optional bang)
    (interactive "<r><//g><!>")
    (align-regexp
     beg end
     (concat "\\(\\s-*\\)" (evil-transform-vim-style-regexp pattern))
     1 1 bang))

  (evil-define-operator +evil-align-right (beg end pattern &optional bang)
    (interactive "<r><//g><!>")
    (align-regexp
     beg end
     (concat "\\(" (evil-transform-vim-style-regexp pattern) "\\)")
     -1 1 bang))

  (evil-set-command-properties
   '+evil-align
   :move-point t
   :ex-arg 'buffer-match
   :ex-bang t
   :keep-visual t
   :suppress-operator t)

  (evil-define-operator +evil-delete (beg end type register yank-handler)
    (interactive "<R><x><y>")
    (condition-case _ex
        (evil-delete beg end type register yank-handler)
      ('text-read-only
       (evil-apply-on-block
        (lambda (beg _)
          (goto-char beg)
          (call-interactively #'wgrep-mark-deletion))
        beg (1- end) nil))))

  (defun +evil-visual-indent ()
    "vnoremap > >gv"
    (interactive)
    (evil-shift-right (region-beginning) (region-end))
    (evil-normal-state)
    (evil-visual-restore))

  (defun +evil-visual-dedent ()
    "vnoremap < <gv"
    (interactive)
    (evil-shift-left (region-beginning) (region-end))
    (evil-normal-state)
    (evil-visual-restore))

  (defun +evil-reselect-paste ()
    (interactive)
    (cl-destructuring-bind (_ _ _ beg end &optional _)
        evil-last-paste
      (evil-visual-make-selection
       (save-excursion (goto-char beg) (point-marker))
       end)))

  (defun +evil-paste-preserve-register ()
    (interactive)
    (let ((evil-this-register ?0))
      (call-interactively #'evil-paste-after)))

  (defun +evil--window-swap (direction)
    (when (window-dedicated-p)
      (user-error "Cannot swap a dedicated window"))
    (let* ((this-window (selected-window))
           (this-buffer (current-buffer))
           (that-window (windmove-find-other-window direction nil this-window))
           (that-buffer (window-buffer that-window)))
      (when (or (minibufferp that-buffer)
                (window-dedicated-p this-window))
        (setq that-buffer nil
              that-window nil))
      (if (not (or that-window (one-window-p t)))
          (funcall (pcase direction
                     ('left #'evil-window-move-far-left)
                     ('right #'evil-window-move-far-right)
                     ('up #'evil-window-move-very-top)
                     ('down #'evil-window-move-very-bottom)))
        (unless that-window
          (setq that-window
                (split-window this-window nil
                              (pcase direction
                                ('up 'above)
                                ('down 'below)
                                (_ direction))))
          (with-selected-window that-window
            (switch-to-buffer (fallback-buffer)))
          (setq that-buffer (window-buffer that-window)))
        (with-selected-window this-window
          (switch-to-buffer that-buffer))
        (with-selected-window that-window
          (switch-to-buffer this-buffer))
        (select-window that-window))))

  (defun +evil-window-move-left ()
    (interactive)
    (+evil--window-swap 'left))
  (defun +evil-window-move-right ()
    (interactive)
    (+evil--window-swap 'right))
  (defun +evil-window-move-up ()
    (interactive)
    (+evil--window-swap 'up))
  (defun +evil-window-move-down ()
    (interactive)
    (+evil--window-swap 'down))

  (evil-define-operator +evil-apply-macro (beg end)
    :motion nil
    :move-point nil
    (interactive "<r>")
    (let ((register (or evil-this-register (read-char)))
          macro)
      (cond ((or (and (eq register ?@) (eq evil-last-register ?:))
                 (eq register ?:))
             (setq macro (lambda () (evil-ex-repeat nil))
                   evil-last-register ?:))
            ((eq register ?@)
             (unless evil-last-register
               (user-error "No previously executed keyboard macro"))
             (setq macro (evil-get-register evil-last-register t)
                   evil-last-register register)))
      (evil-change-state 'normal)
      (evil-with-single-undo
        (apply-macro-to-region-lines beg end macro)))))

(use-package evil-args
  :after evil
  :commands (evil-inner-arg
             evil-outer-arg
             evil-forward-arg
             evil-backward-arg
             evil-jump-out-args)
  :config
  (unless (member "<" evil-args-openers)
    (push "<" evil-args-openers)
    (push ">" evil-args-closers)))

(use-package evil-collection
  :disabled t
  :commands evil-collection-init
  :init
  (defvar +evil-collection-modules
    '(ag
      avy
      bookmark
      calc
      calendar
      cmake-mode
      compile
      daemons
      deadgrep
      debug
      diff-mode
      doc-view
      edebug
      ediff
      elfeed
      elisp-refs
      emms
      eshell
      etags-select
      eww
      flycheck
      ggtags
      git-timemachine
      go-mode
      ibuffer
      image+
      imenu-list
      info
      log-view
      lsp-ui-imenu
      macrostep
      man
      magit
      mu4e
      mu4e-conversation
      notmuch
      outline
      paren
      (pdf pdf-view)
      popup
      proced
      profiler
      python
      realgud
      (term term ansi-term multi-term)
      vc-annotate
      vdiff
      view
      which-key
      wgrep
      woman
      xref))

  (defun +evil-collection-init (module)
    (when %-debug-mode
      (message "Loaded evil-collection-%s" (or (car-safe module)
                                               module)))
    (with-demoted-errors "evil-collection error: %s"
      (evil-collection-init (list module))))
  
  (dolist (mode +evil-collection-modules)
    (dolist (req (or (cdr-safe mode) (list mode)))
      (with-eval-after-load req
        (+evil-collection-init mode))))

  (evil-define-key* 'insert 'global
    "\C-a" #'backward-to-bol-or-indent
    "\C-e" #'forward-to-last-non-comment-or-eol
    "\C-b" #'backward-word
    "\C-f" #'forward-word)

  (after! eldoc
    (eldoc-add-command-completions "evil-window-"))

  (after! comint
    (evil-define-key* 'normal comint-mode-map
      (kbd "C-d") #'evil-scroll-down
      (kbd "C-n") #'comint-next-input
      (kbd "C-p") #'comint-previous-input
      (kbd "gj")  #'comint-next-input
      (kbd "gk")  #'comint-previous-input
      (kbd "]")   #'comint-next-input
      (kbd "[")   #'comint-previous-input)
    (evil-define-key* 'insert comint-mode-map
      (kbd "<up>") #'comint-previous-input
      (kbd "<down>") #'comint-next-input))

  (after! cus-edit
    (evil-set-initial-state 'Custom-mode 'normal)
    (evil-define-key* 'motion custom-mode-map
      (kbd "<tab>") #'widget-forward
      (kbd "S-<tab>") #'widget-backward
      (kbd "<backtab>") #'widget-backward
      (kbd "]") #'widget-forward
      (kbd "[") #'widget-backward
      (kbd "C-n") #'widget-forward
      (kbd "C-p") #'widget-backward
      "gj" #'widget-forward
      "gk" #'widget-backward)
    (evil-define-key* 'normal custom-mode-map
      (kbd "<return>") #'Custom-newline
      (kbd "C-o") #'Custom-goto-parent
      "^" #'Custom-goto-parent
      "<" #'Custom-goto-parent
      "q" #'Custom-buffer-done
      "ZQ" #'evil-quit
      "ZZ" #'Custom-buffer-done))

  (after! help-mode
    (evil-set-initial-state 'help-mode 'normal)
    (evil-define-key* 'normal help-mode-map
      (kbd "SPC") #'scroll-up-command
      (kbd "S-SPC") #'scroll-down-command
      (kbd "C-f") #'scroll-up-command
      (kbd "C-b") #'scroll-down-command
      (kbd "<tab>") #'forward-button
      (kbd "S-<tab>") #'backward-button
      (kbd "<backtab>") #'backward-button
      (kbd "C-o") #'help-go-back
      (kbd "C-i") #'help-go-forward
      "go" #'push-button
      "gO" #'push-button
      "g?" #'describe-mode
      "gr" #'revert-buffer
      "<" #'help-go-back
      ">" #'help-go-forward
      "r" #'help-follow
      "q" #'quit-window
      "ZQ" #'evil-quit
      "ZZ" #'quit-window))

  (after! man
    (evil-define-key* 'normal Man-mode-map
      "q" #'kill-this-buffer))

  (after! view
    (define-key view-mode-map
      [escape] #'View-quit-all))

  (add-transient-hook! 'Buffer-menu-mode
    (+evil-collection-init '(buff-menu "buff-menu")))
  (add-transient-hook! 'image-mode
    (+evil-collection-init 'image))
  (add-transient-hook! 'emacs-lisp-mode
    (+evil-collection-init 'elisp-mode))
  (add-transient-hook! 'occur-mode
    (+evil-collection-init 'replace)))  

(use-package evil-commentary
  :commands
  (evil-commentary
   evil-commentary-yank
   evil-commentary-line)
  :config
  (evil-commentary-mode +1))

(use-package evil-embrace
  :after evil-surround
  :commands
  (embrace-add-pair
   embrace-add-pair-regexp)
  :hook
  ((LaTeX-mode . embrace-LaTeX-mode-hook)
   (org-mode . embrace-org-mode-hook))
  :init
  (add-hook! emacs-lisp-mode (embrace-add-pair ?\` "`" "'"))
  (add-hook! (emacs-lisp-mode lsp-mode)
    (embrace-add-pair-regexp ?f "([^ ]+ " ")" #'+evil--embrace-elisp-fn))
  (add-hook! (org-mode LaTeX-mode)
    (embrace-add-pair-regexp ?l "\\[a-z]+{" "}" #'+evil--embrace-latex))
  :config
  (setq evil-embrace-show-help-p nil)
  (evil-embrace-enable-evil-surround-integration)

  (defun +evil--embrace-get-pair (char)
    (if-let* ((pair (cdr-safe (assoc (string-to-char char) evil-surround-pairs-alist))))
        pair
      (if-let* ((pair (assoc-default char embrace--pairs-list)))
          (if-let* ((real-pair (and (functionp (embrace-pair-struct-read-function pair))
                                    (funcall (embrace-pair-struct-read-function pair)))))
              real-pair
            (cons (embrace-pair-struct-left pair) (embrace-pair-struct-right pair)))
        (cons char char))))

  (defun +evil--embrace-escaped ()
    (let ((char (read-char "\\")))
      (if (eq char 27)
          (cons "" "")
        (let ((pair (+evil--embrace-get-pair (string char)))
              (text (if (sp-point-in-string) "\\\\%s" "\\%s")))
          (cons (format text (car pair))
                (format text (cdr pair)))))))

  (defun +evil--embrace-latex ()
    (cons (format "\\%s{" (read-string "\\")) "}"))

  (defun +evil--embrace-elisp-fn ()
    (cons (format "(%s " (or (read-string "(") "")) ")"))

  (setf (alist-get ?\\ (default-value 'embrace--pairs-list))
        (make-embrace-pair-struct
         :key ?\\
         :read-function #'+evil--embrace-escaped
         :left-regexp "\\[[{(]"
         :right-regexp "\\[]})]")))
      
(use-package evil-escape
  :after evil
  :commands
  (evil-escape evil-escape-mode evil-escape-pre-command-hook)
  :init
  (setq evil-escape-excluded-states '(normal visual multiedit emacs motion)
        evil-escape-key-sequence "kj"
        evil-escape-delay 0.1)
  (add-hook 'pre-command-hook #'evil-escape-pre-command-hook)
  (evil-define-key* '(insert replace visual operator)
    'global
    "\C-g" #'evil-escape)
  :config
  (add-hook 'evil-escape-inhibit-functions #'minibufferp))

(use-package evil-indent-plus)

(use-package evil-matchit
  :after evil
  :commands
  (evilmi-jump-items
   global-evil-matchit-mode
   evilmi-outer-text-object)
  :init
  (global-set-key [remap evil-jump-item] #'evilmi-jump-items)
  (define-key evil-inner-text-objects-map "%" #'evilmi-inner-text-object)
  (define-key evil-outer-text-objects-map "%" #'evilmi-outer-text-objec)
  :config
  (global-evil-matchit-mode +1)
  (evil-set-command-properties 'evilmi-jump-items :type 'inclusive :jump t))

(use-package evil-numbers
  :commands (evil-numbers/inc-at-pt evil-numbers/dec-at-pt))

(use-package evil-surround
  :commands
  (global-evil-surround-mode
   evil-surround-edit
   evil-Surround-edit
   evil-surround-region)
  :config
  (global-evil-surround-mode +1))

(use-package evil-vimish-fold
  :commands
  (evil-vimish-fold/next-fold
   evil-vimish-fold/previous-fold
   evil-vimish-fold/delete
   evil-vimish-fold/delete-all
   evil-vimish-fold/create
   evil-vimish-fold/create-line)
  :init
  (setq vimish-fold-dir (concat %var-dir "folds/")
        vimish-fold-indication-mode 'right-fringe)
  :config
  (vimish-fold-global-mode +1))

(use-package evil-visualstar
  :after evil
  :commands
  (evil-visualstar/begin-search
   evil-visualstar/begin-search-forward
   evil-visualstar/begin-search-backward)
  :init
  (evil-define-key* 'visual 'global
    "*" #'evil-visualstar/begin-search-forward
    "#" #'evil-visualstar/begin-search-backward))

(provide 'config-evil)
;;; config-evil.el ends here

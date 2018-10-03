;;; config-modeline.el

(defvar +modeline-width 3
  "How wide the mode-line bar should be (only respected in graphical Emacs).")
(defvar +modeline-height 25
  "How tall the mode-line bar should be (only respected in graphical Emacs).")
(defvar +modeline-bar-at-end nil
  "If non-nil, the bar is placed at the end, instead of at the beginning of the
mode-line.")
(defvar +modeline-bar-invisible nil
  "If non-nil, the bar is transparent and only used to govern the height of the
mode-line.")
(defvar +modeline-buffer-path-function #'+modeline-file-path-with-project
  "A function that returns, in list form, the components of the buffer file name
to display in the mode-line.

Each item should either be a string or a cons cell whose CAR is the path
component and whose CDR is the name of a face.")

(defvar-local +modeline-format-left ())
(defvar-local +modeline-format-right ())

(defvaralias 'mode-line-format-left '+modeline-format-left)
(defvaralias 'mode-line-format-right '+modeline-format-right)

(put '+modeline-format-left 'risky-local-variable t)
(put '+modeline-format-right 'risky-local-variable t)

(setq global-mode-string '("")
      projectile-dynamic-mode-line nil)

(defvar +modeline--vspc (propertize " " 'face 'variable-pitch))

(defvar anzu--state nil)
(defvar evil-mode nil)
(defvar evil-state nil)
(defvar evil-visual-selection nil)
(defvar iedit-mode nil)
(defvar all-the-icons-scale-factor)
(defvar all-the-icons-default-adjust)

(defgroup +modeline nil
  "TODO"
  :group 'faces)

(defface +modeline-buffer-path
  '((t (:inherit (mode-line-emphasis bold))))
  "Face used for the dirname part of the buffer path."
  :group '+modeline)
(defface +modeline-buffer-file
  '((t (:inherit (mode-line-buffer-id bold))))
  "Face used for the filename part of the buffer path."
  :group '+modeline)
(defface +modeline-buffer-project-root
  '((t (:inherit +modeline-buffer-path)))
  "Face used for the project root at the beginning of the path."
  :group '+modeline)
(defface +modeline-buffer-modified
  '((t (:inherit (error bold) :background nil)))
  "Face used for the 'unsaved' symbol."
  :group '+modeline)
(defface +modeline-buffer-major-mode
  '((t (:inherit (mode-line-emphasis bold))))
  "Face used for the major-mode segment."
  :group '+modeline)
(defface +modeline-highlight
  '((t (:inherit mode-line-emphasis)))
  "Face used for bright segments."
  :group '+modeline)
(defface +modeline-panel
  '((t (:inherit mode-line-highlight)))
  "Face for 'X out of Y' segments (such as `+modeline--anzu')."
  :group '+modeline)
(defface +modeline-info
  '((t (:inherit (success bold))))
  "Face for info-level messages (used by `*vc')."
  :group '+modeline)
(defface +modeline-warning
  '((t (:inherit (warning bold))))
  "Face for warning-level messages (used by `*flycheck')."
  :group '+modeline)
(defface +modeline-urgent
  '((t (:inherit (error bold))))
  "Face for error-level messages (used by `*flycheck')."
  :group '+modeline)
(defface +modeline-bar
  '((t (:inherit highlight)))
  "Face used for the left-most bar in an active window."
  :group '+modeline)

(use-package all-the-icons
  :commands (all-the-icons-octicon
             all-the-icons-faicon
             all-the-icons-fileicon
             all-the-icons-wicon
             all-the-icons-material
             all-the-icons-alltheicon
             all-the-icons-install-fonts)
  :init
  (defun all-the-icons-maybe (orig-fn &rest args)
    (when (display-graphic-p)
      (apply orig-fn args)))
  (dolist (fn #'(all-the-icons-octicon
                 all-the-icons-faicon
                 all-the-icons-fileicon
                 all-the-icons-wicon
                 all-the-icons-material
                 all-the-icons-alltheicon))
    (advice-add fn :around #'all-the-icons-maybe)))

(use-package anzu
  :after-call isearch-mode
  :config
  (setq anzu-cons-mode-line-p nil
        anzu-minimum-input-length 1
        anzu-search-threshold 250)
  (global-anzu-mode +1)

  (defun +modeline-fix-anzu-count (positions here)
    (cl-loop for (start . end) in positions
             collect t into before
             when (and (>= here start) (<= here end))
             return (length before)
             finally return 0))
  (advice-add #'anzu--where-is-here :override #'+modeline-fix-anzu-count)

  (mapc #'make-variable-buffer-local
        '(anzu--total-matched
          anzu--current-position
          anzu--state
          anzu--cached-count
          anzu--cached-positions
          anzu--last-command
          anzu--last-isearch-string
          anzu--overflow-p))

  (add-hook 'isearch-mode-end-hook #'anzu--reset-status t)
  (add-hook 'global-escape-hook #'anzu--reset-status t)
  (add-hook 'iedit-mode-end-hook #'anzu--reset-status))

(use-package evil-anzu
  :after-call (evil-ex-start-search evil-ex-start-word-search))

(use-package shrink-path)

(use-package hide-mode-line
  :init
  (add-hook 'completion-list-mode-hook #'hide-mode-line-mode)
  (add-hook 'Man-mode-hook #'hide-mode-line-mode))

(defvar +modeline--alist nil)

(defun +modeline--segment-active-p (segment xs)
  (cond ((null xs) nil)
        ((listp xs)
         (or (+modeline--segment-active-p segment (car xs))
             (+modeline--segment-active-p segment (cdr xs))))
        ((eq xs segment))))

(defun +modeline-segment-active-p (segment)
  (or (+modeline--segment-active-p segment +modeline-format-left)
      (+modeline--segment-active-p segment +modeline-format-right)))

(defun +modeline-define-format (name left &optional right)
  "Define a preset mode-line format by name.

NAME is a symbol. The convention is to use keywords for global formats, such as
:main or :minimal, but to use regular symbols for buffer-local formats, such as
'twitter or 'pdf.

LEFT and RIGHT are lists that assume the same structure as `mode-line-format'
and make up the mode-line in two parts, separated by variable-width space, to
keep them left and right aligned, respectively."
  (setf (alist-get name +modeline--alist) (list left right)))

(defmacro +modeline-define-segment (name &rest rest)
  (declare (doc-string 2))
  (let ((docstring (if (and (stringp (car rest)) (cdr rest)) (pop rest)))
        body)
    (macroexp-progn
     (if (not (keywordp (car rest)))
         (append `((defvar-local ,name nil ,docstring)
                   (put ',name 'risky-local-variable t))
                 (if (or (stringp (car rest))
                         (memq (car (car-safe rest)) '(:eval :propertize)))
                     `((setq-default ,name ,(car rest)))
                   (let ((fn (intern (format "+modeline--%s" name))))
                     `((fset ',fn (lambda () ,@rest))
                       (byte-compile ',fn)
                       (setq-default ,name (quote (:eval (,fn))))))))
       (setq body rest)
       (while (keywordp (car body))
         (setq body (cddr body)))
       (cl-destructuring-bind (&key init faces on-hooks on-set
                                    &allow-other-keys)
           rest
         (let ((realvar (if (and body faces)
                            (intern (format "+modeline--var-%s" name))
                          name)))
           (append (when body
                     (if (or on-hooks on-set)
                         (let ((setter-fn
                                (intern (format "+modeline--set-%s" name)))
                               (varsetter-fn
                                (intern (format "+modeline--setvar-%s" name))))
                           (append `((fset ',setter-fn
                                           (lambda (&rest _)
                                             (when (+modeline-segment-active-p
                                                    ',name)
                                               (setq-local ,realvar
                                                           ,(macroexp-progn
                                                             body)))))
                                     (byte-compile ',setter-fn))
                                   (mapcar (lambda (hook)
                                             `(add-hook ',hook #',setter-fn))
                                           on-hooks)
                                   (when on-set
                                     `((fset ',varsetter-fn
                                             (lambda (sym val op where)
                                               (and (eq op 'set) where
                                                    (with-current-buffer where
                                                      (set sym val)
                                                      (,setter-fn)))))
                                       ,@(mapcan (lambda (var)
                                                   `((add-variable-watcher ',var
                                                                           #',varsetter-fn)))
                                                 on-set)))))
                       (setq init `(quote (:eval ,(macroexp-progn body))))
                       nil))
                   (if (eq realvar name)
                       `((defvar-local ,name nil ,docstring)
                         (setq-default ,name ,init))
                     `((defvar-local ,realvar ,init)
                       (defvar-local ,name nil ,docstring)
                       (setq-default
                        ,name
                        '(:eval (cond ((active) ,realvar)
                                      (,realvar
                                       (substring-no-properties ,realvar)))))))
                   `((put ',name 'risky-local-variable t)))))))))

(defun +modeline-set-format (name &optional default)
  "Replace the current buffer's mode-line with a prest mode-line format defined
with `+modeline-define-format'.

If DEFAULT is non-nil, make it the default mode-line for all buffers."
  (cl-check-type name symbol)
  (let ((modeline (cdr (assq name +modeline--alist))))
    (unless modeline
      (error "The %s mode-line format does not exist" name))
    (if default
        (setq-default +modeline-format-left  `("" ,@(car  modeline))
                      +modeline-format-right `("" ,@(cadr modeline)))
      (setq +modeline-format-left  `("" ,@(car  modeline))
            +modeline-format-right `("" ,@(cadr modeline))))
    (force-mode-line-update)))

(defvar +modeline--current-window (frame-selected-window))

(defun +modeline-set-selected-window (&rest _)
  (when-let* ((win (frame-selected-window)))
    (unless (minibuffer-window-active-p win)
      (setq +modeline--current-window win)
      (force-mode-line-update))))

(defun +modeline-unset-selected-window ()
  (setq +modeline--current-window nil)
  (force-mode-line-update))

(add-hook 'window-configuration-change-hook #'+modeline-set-selected-window)
(add-hook 'enter-window-hook #'+modeline-set-selected-window)

(if (not (boundp 'after-focus-change-function))
    (progn
      (add-hook 'focus-in-hook #'+modeline-set-selected-window)
      (add-hook 'focus-out-hook #'+modeline-unset-selected-window))
  (defun +modeline-refresh-frame ()
    (setq +modeline--current-window nil)
    (cl-loop for frame in (frame-list)
             if (eq (frame-focus-state frame) t)
             return (setq +modeline--current-window (frame-selected-window
                                                     frame)))
    (force-mode-line-update t))
  (add-function :after after-focus-change-function #'+modeline-refresh-frame))

(defsubst active ()
  (eq (selected-window) +modeline--current-window))

(defvar +modeline--remap-face-cookies nil)

(defun +modeline-focus-all-windows (&rest _)
  (cl-loop for (buffer . cookie) in +modeline--remap-face-cookies
           if (buffer-live-p buffer)
           do (with-current-buffer buffer
                (face-remap-remove-relative cookie))))

(defun +modeline-unfocus-all-windows (&rest _)
  (setq +modeline--remap-face-cookies
        (cl-loop for window in (window-list)
                 for buffer in (window-buffer window)
                 if (buffer-live-p buffer)
                 collect
                 (with-current-buffer
                     (cons buffer
                           (face-remap-add-relative 'mode-line
                                                    'mode-line-inactive))))))

(add-hook 'focus-in-hook #'+modeline-focus-all-windows)
(add-hook 'focus-out-hook #'+modeline-unfocus-all-windows)
(advice-add #'posframe-hide :after #'+modeline-focus-all-windows)
(advice-add #'posframe-delete :after #'+modeline-focus-all-windows)

(after! helm
  (add-hook 'helm-before-initialize-hook #'+modeline-unfocus-all-windows)
  (add-hook 'helm-cleanup-hook #'+modeline-focus-all-windows))

(defun +modeline--make-xpm (width height &optional color)
  "Create a XPM bitmap."
  (propertize
   " " 'display
   (let ((data (make-list height (make-list width 1)))
         (color (or color "None")))
     (ignore-errors
       (create-image
        (concat
         (format "/* XPM */\nstatic char * percent[] = {\n\"%i %i 2 1\",\n\". c %s\",\n\"  c %s\","
                 (length (car data)) (length data) color color)
         (cl-loop with idx = 0
                  with len = (length data)
                  for dl in data
                  do (cl-incf idx)
                  concat "\""
                  concat (cl-loop for d in dl
                                  if (= d 0) collect (string-to-char " ")
                                  else collect (string-to-char "."))
                  concat (if (eq idx len) "\"};" "\",\n")))
        'xpm t :ascent 'center)))))

(defun +modeline--build-path (path)
  "Construct the file path for the `+modeline-buffer-id' segment using
  `+modeline-buffer-path-function'.

If the buffer has no `buffer-file-name' just use `buffer-name'."
  (let ((buffer-file-name (or path buffer-file-name)))
    (if (or (eq major-mode 'dired-mode)
            (null buffer-file-name))
        (propertize "%s" 'face '+modeline-buffer-path)
      (cl-loop for spec in (funcall +modeline-buffer-path-function)
               if (stringp spec) concat spec
               else if (not (null spec))
               concat (propertize (car spec) 'face (cdr spec))))))

(defun +modeline-file-path-with-project ()
  "Returns the unaltered buffer file path relative to the project root's
parent."
  (let ((filename (or buffer-file-truename
                      (file-truename buffer-file-name))))
    (append (if (+projectile-project-p)
                (let* ((project-root (+projectile-project-root))
                       (relative-dirs (file-relative-name
                                       (file-name-directory filename)
                                       (file-truename project-root))))
                  (list (cons (concat (+projectile-project-name) "/")
                              '+modeline-buffer-project-root)
                        (unless (equal "./" relative-dirs)
                          (cons relative-dirs '+modeline-buffer-path))))
              (list nil (cons (file-name-directory filename)
                              '+modeline-buffer-path)))
            (list (cons (file-name-nondirectory filename)
                        '+modeline-buffer-file)))))

(defun +modeline-file-path-from-project ()
  "Returns the file path relative to (and including) the project root.

Descendant folders are truncated."
  (let* ((parts (+modeline-file-path-with-project))
         (dirs (car (nth 1 parts))))
    (setcar (nth 1 parts)
            (shrink-path--dirs-internal dirs t))
    parts))

(defun +modeline-file-path-truncated-upto-project ()
  "Returns the file path, truncating segments prior to the project root."
  (pcase-let
      ((`(,root-parent ,root ,dir ,file)
        (shrink-path-file-mixed (+projectile-project-root)
                                (file-name-directory buffer-file-name)
                                buffer-file-name)))
    (list (cons root-parent 'font-lock-comment-face)
          (cons root '+modeline-buffer-project-root)
          (cons (concat "/" dir) '+modeline-buffer-path)
          (cons file +modeline-buffer-file))))

(defun +modeline-file-path-truncated-upto-project-root ()
  "Returns the file path, truncating segments prior to and including the project
root."
  (let* ((parts (+modeline-file-path-truncated-upto-project))
         (root (car (nth 1 parts))))
    (setcar (nth 1 parts)
            (let ((first (substring root 0 1)))
              (if (equal first ".")
                  (substring root 0 2)
                first)))
    parts))

(defun +modeline-file-path-truncated ()
  "Returns the absolute file path with all directories truncated."
  (pcase-let ((`(,dir . ,file) (shrink-path-prompt buffer-file-name)))
    (list (cons dir '+modeline-buffer-path)
          (cons file '+modeline-buffer-file))))

(defun +modeline-file-name ()
  "Returns the file name."
  (list (cons "%b" '+modeline-buffer-path)))

(defvar +modeline-bar-start nil)
(defvar +modeline-bar-end nil)

(put '+modeline-bar-start 'risky-local-variable t)
(put '+modeline-bar-end   'risky-local-variable t)

(defvar +modeline-bar-active nil)
(defvar +modeline-bar-inactive nil)

(defun +modeline-setup-bars ()
  (setq +modeline-bar-active
        (+modeline--make-xpm +modeline-width +modeline-height
                             (unless +modeline-bar-invisible
                               (face-background '+modeline-bar nil t)))
        +modeline-bar-inactive
        (+modeline--make-xpm +modeline-width +modeline-height))
  (setq +modeline-bar-start nil
        +modeline-bar-end nil)
  (if +modeline-bar-at-end
      (setq +modeline-bar-end '+modeline-bar)
    (setq +modeline-bar-start '+modeline-bar)))
(add-hook 'load-theme-hook #'+modeline-setup-bars)

(defun +modeline-setup-bars-after-change (sym val op _where)
  (when (eq op 'set)
    (set sym val)
    (+modeline-setup-bars)))
(dolist (var '(+modeline-width
               +modeline-height
               +modeline-bar-at-end
               +modeline-bar-invisible))
  (add-variable-watcher var #'+modeline-setup-bars-after-change))

(+modeline-define-segment
 +modeline-bar
 (if (active)
     '+modeline-bar-active
   '+modeline-bar-inactive))

(defun +modeline-update-on-change ()
  (+modeline--set-+modeline-buffer-state)
  (remove-hook 'post-command-hook #'+modeline-update-on-change t))

(defun +modeline-start-update-on-change ()
  (add-hook 'post-command-hook #'+modeline-update-on-change nil t))
(add-hook 'first-change-hook #'+modeline-start-update-on-change)

(advice-add #'undo :after #'+modeline--set-+modeline-buffer-state)
(advice-add #'undo-tree-undo :after #'+modeline--set-+modeline-buffer-state)

(+modeline-define-segment
 +modeline-buffer-state
 :on-hooks (find-file-hook
            read-only-mode-hook
            after-change-functions
            after-save-hook
            after-revert-hook)
 (let ((icon (cond (buffer-read-only
                    (all-the-icons-octicon
                     "lock"
                     :face '+modeline-warning
                     :v-adjust -0.05))
                   ((buffer-modified-p)
                    (all-the-icons-faicon
                     "floppy-o"
                     :face '+modeline-buffer-modified
                     :v-adjust -0.05))
                   ((and buffer-file-name
                         (not (file-exists-p buffer-file-name)))
                    (all-the-icons-octicon
                     "circle-slash"
                     :face '+modeline-urgent
                     :v-adjust -0.05)))))
   (if icon (concat icon " "))))

(+modeline-define-segment
 +modeline-buffer-id
 :on-hooks (find-file-hook
            after-save-hook
            after-revert-hook)
 :init (propertize "%b" 'face '+modeline-buffer-file)
 :faces t
 (+modeline--build-path (buffer-file-name (buffer-base-buffer))))

(+modeline-define-segment
 +modeline-buffer-directory
 (let ((face (if (active) '+modeline-buffer-path)))
   (concat (if (display-graphic-p) " ")
           (all-the-icons-octicon
            "file-directory"
            :face face
            :v-adjust -0.1
            :height 1.25)
           " "
           (propertize (abbreviate-file-name default-directory)
                       'face face))))

(+modeline-define-segment
 +modeline-vcs
 :on-set (vc-mode)
 (when (and vc-mode buffer-file-name)
   (let* ((backend (vc-backend buffer-file-name))
          (state   (vc-state buffer-file-name backend)))
     (let ((face    'mode-line-inactive)
           (active  (active))
           (all-the-icons-default-adjust -0.1))
       (concat (cond ((memq state '(edited added))
                      (if active (setq face '+modeline-info))
                      (all-the-icons-octicon
                       "git-compare"
                       :face face
                       :v-adjust -0.05))
                     ((eq state 'needs-merge)
                      (if active (setq face '+modeline-info))
                      (all-the-icons-octicon
                       "git-merge"
                       :face face))
                     ((eq state 'needs-update)
                      (if active (setq face '+modeline-warning))
                      (all-the-icons-octicon
                       "arrow-down"
                       :face face))
                     ((memq state '(removed conflict unregistered))
                      (if active (setq face '+modeline-urgent))
                      (all-the-icons-octicon
                       "alert"
                       :face face))
                     (t
                      (if active (setq face 'font-lock-doc-face))
                      (all-the-icons-octicon
                       "git-compare"
                       :face face
                       :v-adjust -0.05)))
               +modeline--vspc
               (propertize
                (substring vc-mode (+ (if (eq backend 'Hg) 2 3) 2))
                'face (if active face)))))))

(+modeline-define-segment
 +modeline-encoding
 :on-hooks (after-revert-hook
            after-save-hook
            find-file-hook)
 :on-set (buffer-file-coding-system
          indent-tabs-mode
          tab-width)
 (format "%s%d  %s  %s"
         (if indent-tabs-mode "⭾" "␣")
         tab-width
         (pcase (coding-system-eol-type buffer-file-coding-system)
           (0 "LF")
           (1 "CRLF")
           (2 "CR"))
         (let* ((sys (coding-system-plist buffer-file-coding-system))
                (category (plist-get sys :category)))
           (cond ((eq category 'coding-category-undecided)
                  "")
                 ((or (eq category 'coding-category-utf-8)
                      (eq (plist-get sys :name) 'prefer-utf-8))
                  "UTF-8  ")
                 ((concat (upcase (symbol-name (plist-get sys :name)))
                          "  "))))))

(+modeline-define-segment
 +modeline-major-mode
 (propertize (format-mode-line mode-name)
             'face (if (active) '+modeline-buffer-major-mode)))

(defun +modeline--macro-recording ()
  "Display current Emacs or Evil macro being recorded."
  (when (and (active) (or defining-kbd-macro executing-kbd-macro))
    (let ((sep (propertize " " 'face '+modeline-panel)))
      (concat sep
              (propertize (if (bound-and-true-p evil-this-macro)
                              (char-to-string evil-this-macro)
                            "Macro")
                          'face '+modeline-panel)
              sep
              (all-the-icons-octicon
               "triangle-right"
               :face '+modeline-panel
               :v-adjust -0.05)
              sep))))

(defsubst +modeline--anzu ()
  "Show the match index and total number of matches.

Requires `anzu' and `evil-anzu'."
  (when (and anzu--state (not iedit-mode))
    (propertize
     (let ((here anzu--current-position)
           (total anzu--total-matched))
       (cond ((eq anzu--state 'replace-query)
              (format " %d replace " total))
             ((eq anzu--state 'replace)
              (format " %d/%d " here total))
             (anzu--overflow-p
              (format " %s+ " total))
             ((format " %s/%d " here total))))
     'face (if (active) '+modeline-panel))))

(defsubst +modeline--evil-substitute ()
  "Show number of matches for evil-ex substitutions and highlights in
real-time."
  (when (and evil-mode
             (or (assq 'evil-ex-substitute evil-ex-active-highlights-alist)
                 (assq 'evil-ex-global-match evil-ex-active-highlights-alist)
                 (assq 'evil-ex-buffer-match evil-ex-active-highlights-alist)))
    (propertize
     (let ((range (if evil-ex-range
                      (cons (car evil-ex-range) (cadr evil-ex-range))
                    (cons (line-beginning-position) (line-end-position))))
           (pattern (car-safe (evil-delimited-arguments evil-ex-argument 2))))
       (if pattern
           (format " %s matches " (how-many pattern (car range) (cdr range)))
         " - "))
     'face (if (active) '+modeline-panel))))

(defun +modeline--overlay-sort (a b)
  (< (overlay-start a) (overlay-start b)))

(defsubst +modeline--iedit ()
  "Show the number of `iedit' region matches, as well as what match you're on."
  (when (and iedit-mode iedit-occurrences-overlays)
    (propertize
     (let ((this-oc (or (let ((inhibit-message t))
                          (iedit-find-current-occurrence-overlay))
                        (progn
                          (iedit-prev-occurrence)
                          (iedit-find-current-occurrence-overlay))))
           (length (length iedit-occurrences-overlays)))
       (format " %s/%d "
               (if this-oc
                   (- length
                      (length (memq this-oc
                                    (sort (append iedit-occurrences-overlays
                                                  nil)
                                          #'+modeline--overlay-sort)))
                      -1)
                 "-")
               length))
     'face (if (active) '+modeline-panel))))

(+modeline-define-segment
 +modeline-matches
 "Displays:

  1. the currently recording macro,
  2. a current/total for the current search term,
  3. the number of ex substitutions being conducted,
  4. the number of active `iedit' regions."
 (let ((meta (concat (+modeline--macro-recording)
                     (+modeline--anzu)
                     (+modeline--evil-substitute)
                     (+modeline--iedit)
                     " ")))
   (or (and (not (equal meta " ")) meta)
       (if buffer-file-name " %I "))))

(defsubst +modeline-column (pos)
  (save-excursion (goto-char pos)
                  (current-column)))

(defvar-local +modeline-enable-word-count nil
  "If non-nil, add a word count to the selection-info mode-line segment.")
(setq-hook! 'text-mode-hook +modeline-enable-word-count t)
                     
(+modeline-define-segment
 +modeline-selection-info
 (let ((beg (or evil-visual-beginning (region-beginning)))
       (end (or evil-visual-end (region-end))))
   (propertize
    (let ((lines (count-lines beg (min end (point-max)))))
      (concat (cond ((or (bound-and-true-p rectangle-mark-mode)
                         (eq 'block evil-visual-selection))
                     (let ((cols (abs (- (+modeline-column end)
                                         (+modeline-column beg)))))
                       (format "%dx%dB" lines cols)))
                    ((eq evil-visual-selection 'line)
                     (format "%dL" lines))
                    ((> lines 1)
                     (format "%dC %dL" (- end beg) lines))
                    ((format "%dC" (- end beg))))
              (when +modeline-enable-word-count
                (format " %dW" (count-words beg end)))))
    'face '+modeline-highlight)))

(defun +modeline-enable-selection-info ()
  (add-to-list '+modeline-format-left '+modeline-selection-info t #'eq))
(defun +modeline-disable-selection-info ()
  (setq +modeline-format-left (delq '+modeline-selection-info
                                    +modeline-format-left)))

(cond ((featurep 'evil)
       (add-hook 'evil-visual-state-entry-hook
                 #'+modeline-enable-selection-info)
       (add-hook 'evil-visual-state-exit-hook
                 #'+modeline-disable-selection-info))
      ((add-hook 'activate-mark-hook #'+modeline-enable-selection-info)
       (add-hook 'deactivate-mark-hook #'+modeline-disable-selection-info)))

(defun +modeline-ml-icon (icon &optional text face voffset)
  (concat (when icon
            (concat
             (all-the-icons-material
              icon
              :face face
              :height 1.1
              :v-adjust (or voffset -0.2))
             (if text +modeline--vspc)))
          (if text (propertize text 'face face))))

(defun +modeline--flycheck-status (status)
  (pcase status
    (`finished (if flycheck-current-errors
                   (let-alist (flycheck-count-errors flycheck-current-errors)
                     (let ((sum (+ (or .error 0) (or .warning 0))))
                       (+modeline-ml-icon
                        "do_not_disturb_alt"
                        (number-to-string sum)
                        (if .error '+modeline-urgent '+modeline-warning)
                        '0.25)))
                 (+modeline-ml-icon "check" nil '+modeline-info)))
    (`running (+modeline-ml-icon "access_time" nil 'font-lock-doc-face -0.25))
    (`errored (+modeline-ml-icon "sim_card_alert" "Error" '+modeline-urgent))
    (`interrupted (+modeline-ml-icon "pause" "Interrupted"
                                     'font-lock-doc-face))))

(defun +modeline-update-flycheck-segment (&optional status)
  (setq +modeline-flycheck
        (when-let* ((status-str (+modeline--flycheck-status status)))
          (concat +modeline--vspc status-str " "))))
(add-hook 'flycheck-mode-hook #'+modeline-update-flycheck-segment)
(add-hook 'flycheck-status-changed-functions
          #'+modeline-update-flycheck-segment)

(+modeline-define-segment
 +modeline-flycheck
 "Displays color-coded flycheck error status in the current buffer (with pretty
 icons)."
 :init nil)

(+modeline-define-format
 :main
 '(+modeline-matches
   " "
   +modeline-buffer-state
   +modeline-buffer-id
   "  %2l:%c %p  ")
 `(mode-line-misc-info
   +modeline-encoding
   +modeline-major-mode
   " "
   (vc-mode (" " +modeline-vcs " "))
   mode-line-process
   +modeline-flycheck))

(+modeline-define-segment
 +modeline--rest
 (let ((rhs-str (format-mode-line +modeline-format-right)))
   (list (propertize
          " " 'display
          `((space :align-to (- (+ right right-fringe right-margin)
                                ,(1+ (string-width rhs-str))))))
         rhs-str)))

(setq-default mode-line-format '(""
                                 +modeline-bar-start
                                 +modeline-format-left
                                 +modeline--rest
                                 +modeline-bar-end))

(+modeline-set-format :main t)
 
(provide 'config-modeline)
;;; config-modeline.el ends here

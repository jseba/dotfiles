;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;;  ~~ JSeba's .emacs setup --
;;;
;;;   Currently setup primarily for a C++ workflow
;;;
;;;   Keybinding policy:
;;;     * C-x: primary, system level commands (mostly just overrides with better subsystems)
;;;     * C-c: secondary, user level commands (most packages have bindings here)
;;;     * C-.: tertiary commands (mostly temporary/testing)
;;;
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Begin

(add-to-list 'load-path "~/.emacs.d/site")
(add-to-list 'load-path "~/.emacs.d/personal")

;; Set core interface settings as early as possible
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'horizontal-scroll-bar-mode) (horizontal-scroll-bar-mode -1))
(setq inhibit-startup-screen t
    inhibit-default-init t
    initial-scratch-message ""
    visible-bell nil)
(size-indication-mode t)

;; Fonts
(set-face-attribute 'default nil
          :family "Source Code Pro"
          :height 100)
(set-face-attribute 'variable-pitch nil
          :family "Fira Sans"
          :height 110
          :weight 'regular)

;; Sane defaults
(fset 'yes-or-no-p #'y-or-n-p)
(fset 'display-startup-echo-area-message #'ignore)
(setq load-prefer-newer t)

;; Setup package manager and use-package
(require 'package)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
;; TODO: make it easy to prefer stable vs latest melpa
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))
(setq package-enable-at-startup nil)
(or (file-exists-p package-user-dir)
  (package-refresh-contents))
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))
(require 'bind-key)
(require 'diminish)
(require 'time-date)

(use-package bug-hunter
  :ensure t)

(use-package hydra
  :ensure t)

;; Relocate the customization file
(defconst jseba-custom-file (locate-user-emacs-file "custom.el")
      "File used to store settings from Customization UI.")
(use-package cus-edit
  :defer t
  :init
  (load jseba-custom-file 'no-error 'no-message)
  :config
  (setq custom-file jseba-custom-file
      custom-buffer-done-kill nil
      custom-buffer-verbose-help nil
      custom-unlispify-tag-names nil
      custom-unlispify-menu-entries nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Functions
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Smarter move to beginning of line
(defun smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between first non-whitespace character and
the beginning of the line."

  (interactive "^p")
  (setq arg (or arg 1))

  (when (/= arg 1)
  (let ((line-move-visual nil))
    (forward-line (1- arg))))
  (let ((orig-point (point)))
  (back-to-indentation)
  (when (= orig-point (point))
    (move-beginning-of-line 1))))

(defun create-new-buffer (&optional mode)
  "Create a new empty buffer named \"untitled\""
  (interactive)
  (let ((-buf (generate-new-buffer "untitled")))
    (switch-to-buffer -buf)
    (when arg
      (funcall mode))
    (setq buffer-offer-save t)))

;;;; The following functions are from lunaryorn
(defun close-and-kill-this-pane ()
  "If there are multiple windows, then close this pane and kill the buffer in it also."
  (interactive)
  (kill-this-buffer)
  (if (not (one-window-p))
  (delete-window)))

(defun display-buffer-fullframe (buffer alist)
  "Display BUFFER in fullscreen.

ALIST is a `display-buffer' ALIST.

Return the new window for BUFFER."
  (let ((window (display-buffer-pop-up-window buffer alist)))
    (when window
      (delete-other-windows window))
    window))

(defun smart-kill-whole-line (&optional arg)
    "Kill whole line and move back to indentation.
  Kill the whole line with function `kill-whole-line' and then move
  `back-to-indentation'."
    (interactive "p")
      (kill-whole-line arg)
        (back-to-indentation))

(defun smart-backward-kill-line ()
    "Kill line backwards and re-indent."
      (interactive)
        (kill-line 0)
          (indent-according-to-mode))

(defun smart-open-line ()
    "Insert empty line after the current line."
      (interactive)
        (move-end-of-line nil)
          (newline-and-indent))

(defun back-to-indentation-or-beginning-of-line (arg)
    "Move point back to indentation of beginning of line.
  Move point to the first non-whitespace character on this line.
  If point is already there, move to the beginning of the line.
  Effectively toggle between the first non-whitespace character and
  the beginning of the line.
  If ARG is not nil or 1, move forward ARG - 1 lines first.  If
  point reaches the beginning or end of the buffer, stop there."
    (interactive "^p")
      (setq arg (or arg 1))

        ;; Move lines first
          (when (/= arg 1)
                (let ((line-move-visual nil))
                        (forward-line (1- arg))))

            (let ((orig-point (point)))
                  (back-to-indentation)
                      (when (= orig-point (point))
                              (move-beginning-of-line 1))))

(define-minor-mode auto-fill-comments-mode
  "Minor mode to auto-fill comments only."
  :lighter nil
  :keymap nil
  (cond
    (auto-fill-comments-mode
      (setq-local comment-auto-fill-only-comments t)
      (auto-fill-mode 1))
    (:else
      (kill-local-variable 'comment-auto-fill-only-comments)
      (auto-fill-mode -1))))

(defun insert-current-date (iso)
    "Insert the current date at point.
  When ISO is non-nil, insert the date in ISO 8601 format.
  Otherwise insert the date as Mar 04, 2014."
    (interactive "P")
      (insert (format-time-string (if iso "%F" "%b %d, %Y"))))

(defun current-file ()
    "Gets the \"file\" of the current buffer.
  The file is the buffer's file name, or the `default-directory' in
  `dired-mode'."
    (if (derived-mode-p 'dired-mode)
            default-directory
                (buffer-file-name)))

(defun copy-filename-as-kill (&optional arg)
  "Copy the name of the currently visited file to kill ring.
With a zero prefix arg, copy the absolute file name.  With
\\[universal-argument], copy the file name relative to the
current Projectile project, or to the current buffer's
`default-directory', if the file is not part of any project.
Otherwise copy the non-directory part only."
  (interactive "P")
  (if-let ((file-name (current-file))
           (name-to-copy
             (cond
               ((zerop (prefix-numeric-value arg)) file-name)
               ((consp arg)
                 (let* ((projectile-require-project-root nil)
                        (directory (and (fboundp 'projectile-project-root)
                                        (projectile-project-root))))
                   (file-relative-name file-name directory)))
               (t (file-name-nondirectory file-name)))))
       (progn
         (kill-new name-to-copy)
         (message "%s" name-to-copy))
     (user-error "This buffer is not visiting a file")))

(defun rename-file-and-buffer ()
    "Rename the current file and buffer."
  (interactive)
  (let* ((filename (buffer-file-name))
         (old-name (if filename
                       (file-name-nondirectory filename)
                       (buffer-name)))
         (new-name (read-file-name "New name: " nil nil nil old-name)))
    (cond
      ((not (and filename (file-exists-p filename))) (rename-buffer new-name))
      ((vc-backend filename) (vc-rename-file filename new-name))
      (t
        (rename-file filename new-name 'force-overwrite)
        (set-visited-file-name new-name 'no-query 'along-with-file)))))

(defun delete-file-and-buffer ()
  "Delete the current file and kill the buffer."
  (interactive)
  (let ((filename (buffer-file-name)))
    (cond
      ((not filename) (kill-buffer))
      ((vc-backend filename) (vc-delete-file filename))
      (t
        (delete-file filename)
        (kill-buffer)))))

(defun launch-dwim ()
  "Open the current file externally."
  (interactive)
  (if (derived-mode-p 'dired-mode)
    (let ((marked-files (dired-get-marked-files)))
         (if marked-files
           (launch-files marked-files 'confirm)
           (launch-directory (dired-current-directory))))
    (if (buffer-file-name)
      (launch-file (buffer-file-name))
      (user-error "The current buffer is not visiting a file"))))

(defun find-user-init-file-other-window ()
  "Edit the `user-init-file', in another window."
  (interactive)
  (find-file-other-window user-init-file))

(defun recompile-packages ()
  "Recompile all packages."
  (interactive)
  (byte-recompile-directory package-user-dir nil 'force))

(defun all-init-files (&optional with-packages)
  "Return a list of all Emacs Lisp files in my configuration.
If WITH-PACKAGES is given and non-nil include 3rd party
packages."
  (append (list user-init-file)
          (directory-files-recursively (locate-user-emacs-file "lisp/")
                                       (rx ".el" eos))
          (if with-packages
            (directory-files-recursively package-user-dir
                                         (rx ".el" eos))
            nil)))

(defun count-config-lines (&optional with-packages)
  "Show a buffer with LoC statistics for my Emacs config.
If WITH-PACKAGES is given and non-nil include 3rd party packages
into the count."
  (interactive "P")
  (let ((cloc (executable-find "cloc")))
    (unless cloc
      (user-error "Please install cloc"))
    (with-current-buffer (get-buffer-create " *LoC Emacs configuration*")
      (text-mode)
      (read-only-mode)
      (view-mode)
      (let ((inhibit-read-only t)
            (files (all-init-files with-packages)))
        (erase-buffer)
        (goto-char (point-min))
        (apply #'call-process cloc nil t t "--quiet" files))
      (pop-to-buffer (current-buffer)))))


(defun browse-feature-url (feature)
  "Browse the URL of the given FEATURE.
Interactively, use the symbol at point, or prompt, if there is
none."
  (interactive
         (let ((symbol (or (symbol-at-point)
                           (completing-read "Feature: " features nil 'require-match))))
           (list symbol)))
  (let* ((library (if (symbolp feature) (symbol-name feature) feature))
         (library-file (find-library-name library)))
    (when library-file
      (with-temp-buffer
        (insert-file-contents library-file)
        (let ((url (lm-header "URL")))
          (if url
            (browse-url url)
            (user-error "Library %s has no URL header" library)))))))

;;;; Highlights and fontification
;(defun lunaryorn-whitespace-style-no-long-lines ()
  ;"Configure `whitespace-mode' for Org.
;Disable the highlighting of overlong lines."
  ;(setq-local whitespace-style (-difference whitespace-style '(lines lines-tail))))

;(defun lunaryorn-whitespace-mode-local ()
  ;"Enable `whitespace-mode' after local variables where set up."
  ;(add-hook 'hack-local-variables-hook #'whitespace-mode nil 'local))

(defun reftex-find-ams-environment-caption (environment)
  "Find the caption of an AMS ENVIRONMENT."
  (let ((re (rx-to-string `(and "\\begin{" ,environment "}"))))
    ;; Go to the beginning of the label first
    (re-search-backward re)
    (goto-char (match-end 0)))
  (if (not (looking-at (rx (zero-or-more space) "[")))
    (error "Environment %s has no title" environment)
    (let ((beg (match-end 0)))
      ;; Move point onto the title start bracket and move over to the end,
      ;; skipping any other brackets in between, and eventually extract the text
      ;; between the brackets
      (goto-char (1- beg))
      (forward-list)
      (buffer-substring-no-properties beg (1- (point))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Appearance
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Turn off blinking cursor
(blink-cursor-mode -1)

;; Turn off bell ring
(setq ring-bell-function 'ignore)

;; Improve scrolling
(setq scroll-margin 0
      scroll-conservatively 1000
      scroll-error-top-bottom t
      scroll-preserve-screen-position 1)

;; Mouse
(setq mouse-wheel-scroll-amount '(1)
      mouse-wheel-progressive-speed nil)

;; Mode line settings
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

;; Face remapping
(use-package face-remap
  :bind (("C-c w z" . text-scale-adjust)))

;; Flash the cursor after a large movement
;(use-package beacon
  ;:ensure t
  ;:init
  ;(beacon-mode +1)
  ;:diminish
  ;beacon-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Evil
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package evil
  :ensure t
  :bind
  (("C-c C-v" . evil-mode)
   :map evil-normal-state-map
   (":" . evil-ex)
   (";" . evil-ex)
   ("!" . string-inflection-cycle)
   ("+" . evil-numbers/inc-at-pt)
   ("-" . evil-numbers/dec-at-pt)
   ("j" . evil-next-visual-line)
   ("k" . evil-previous-visual-line)
   ("C-h" . evil-window-left)
   ("C-j" . evil-window-down)
   ("C-k" . evil-window-up)
   ("C-l" . evil-window-right)
   ("C-]" . rtags-find-symbol-at-point)
   :map evil-visual-state-map
   (":" . evil-ex)
   (";" . evil-ex)
   :map evil-motion-state-map
   (":" . evil-ex)
   ([C-i] . evil-jump-forward)
   :map evil-emacs-state-map
   )
  :init
  (evil-mode 1)
  (evil-define-command evil-maybe-exit ()
    :repeat change
    (interactive)
    (let ((modified (buffer-modified-p))
          (entry-key ?k)
          (exit-key ?j))
      (insert entry-key)
      (let ((evt (read-event (format "Insert %c to exit insert state" exit-key) nil 0.5)))
        (cond
          ((null evt) (message ""))
          ((and (integerp evt) (char-equal evt exit-key))
           (delete-char -1)
           (set-buffer-modified-p modified)
           (push 'escape unread-command-events))
          (t (push evt unread-command-events))))))

  (defvar evil-cursors '(("normal" "DarkGoldenrod2" box)
                         ("insert" "chartreuse3" (bar . 2))
                         ("emacs" "SkyBlue2" box)
                         ("hybrid" "SkyBlue2" (bar . 2))
                         ("replace" "chocolate" (hbar . 2))
                         ("evilified" "LightGoldenrod3" box)
                         ("visual" "gray" (hbar . 2))
                         ("motion" "plum3" box)
                         ("lisp" "HotPink1" box)
                         ("iedit" "firebrick1" box)
                         ("iedit-insert" "firebrick1" (bar . 2))))
  (setq evil-find-skip-newlines t
        evil-cross-lines t
        evil-move-cursor-back nil
        evil-mode-line-format nil
        evil-search-module 'evil-search
        evil-want-C-w-in-emacs-state t

        evil-normal-state-tag   (propertize "N" 'face '((:background "blue" :foreground "black")))
        evil-emacs-state-tag    (propertize "E" 'face '((:background "orange" :foreground "black")))
        evil-insert-state-tag   (propertize "I" 'face '((:background "green" :foreground "black")))
        evil-visual-state-tag   (propertize "V" 'face '((:background "yellow" :foreground "black")))
        evil-motion-state-tag   (propertize "M" 'face '((:background "grey80" :foreground "black")))
        evil-operator-state-tag (propertize "O" 'face '((:background "purple"))))
  :config
  (define-key evil-insert-state-map "k" #'evil-maybe-exit)
  (add-hook 'evil-insert-state-entry-hook (lambda () (hl-line-mode t)))
  (add-hook 'evil-insert-state-exit-hook (lambda () (let ((current-prefix-arg '(0))) (call-interactively 'hl-line-mode)))))

(use-package evil-leader
  :ensure t
  :init
  (setq evil-leader/leader ","
        evil-leader/in-all-states t)
  :config
  (evil-leader/set-key
    "vs" 'split-window-horizontally
    "cc" 'comment-dwim
    "cu" 'comment-kill
    ">"  'switch-to-next-buffer
    "<"  'switch-to-prev-buffer
    "b"  'helm-mini
    "f"  'helm-ag))

(use-package evil-surround
  :ensure t
  :init
  (setq-default evil-surround-pairs-alist '((?\( . ("(" . ")"))
                                            (?\[ . ("[" . "]"))
                                            (?\{ . ("{" . "}"))

                                            (?\) . ("( " . " )"))
                                            (?\] . ("[ " . " ]"))
                                            (?\} . ("{ " . " }"))
                                            (?>  . ("< " . " >"))

                                            (?# . ("#{" . "}"))
                                            (?p . ("(" . ")"))
                                            (?b . ("[" . "]"))
                                            (?B . ("{" . "}"))
                                            (?< . ("<" . ">"))
                                            (?t . evil-surround-read-tag)))
  :config
  (global-evil-surround-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Editing
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Keybindings!!
;; TODO: this is a base from lunaryorn's config
(use-package which-key
  :ensure t
  :init
  (which-key-mode)
  :config
  (setq which-key-idle-delay 0.4
        which-key-sort-order 'which-key-prefix-then-key-order
        which-key-key-replacement-alist
          '(("<\\([[:alnum:]-]+\\)>" . "\\1")
            ("up"                    . "↑")
            ("right"                 . "→")
            ("down"                  . "↓")
            ("left"                  . "←")
            ("DEL"                   . "⌫")
            ("deletechar"            . "⌦")
            ("RET"                   . "⏎"))
        which-key-description-replacement-alist
          '(("Prefix Command" . "prefix")
            ;; Lambdas
            ("\\`\\?\\?\\'"   . "λ")
            ;; Prettify hydra entry points
            ("/body\\'"       . "|=")
            ;; Drop/shorten package prefixes
            ("\\`lunaryorn-"  . "")
            ("projectile-"    . "proj-")
            ("helm-"          . "h-")
            ("magit-"         . "ma-")))
  (which-key-declare-prefixes
    ;; Prefixes for global prefixes and minor modes
    "C-c @" "outline"
    "C-c !" "flycheck"
    "C-c 8" "typo"
    "C-c 8 -" "typo/dashes"
    "C-c 8 <" "typo/left-brackets"
    "C-c 8 >" "typo/right-brackets"
    ;; Prefixes for my personal bindings
    "C-c a" "applications"
    "C-c b" "buffers"
    "C-c c" "compile-and-comments"
    "C-c e" "errors"
    "C-c f" "files"
    "C-c f v" "variables"
    "C-c g" "git"
    "C-c g g" "github/gist"
    "C-c h" "helm/help"
    "C-c i" "insert"
    "C-c i l" "licenses"
    "C-c j" "jump"
    "C-c l" "language/spelling"
    "C-c m" "major mode"
    "C-c o" "cursors"
    "C-c p" "projects"
    "C-c p s" "projects/search"
    "C-c p x" "projects/execute"
    "C-c p 4" "projects/other-window"
    "C-c r" "rtags"
    "C-c s" "search"
    "C-c t" "toggle"
    "C-c w" "windows/frames"
    "C-c x" "text")
  ;(which-key-declare-prefixes-for-mode 'org-mode
    ;;; TODO:
    ;)
  ;(which-key-declare-prefixes-for-mode 'cc-mode
    ;;; TODO: rtags mostly
    ;)
  ;(which-key-declare-prefixes-for-mode 'python-mode
    ;;; TODO:
    ;)
  :diminish which-key-mode)

(bind-key "C-c x i" #'indent-region)
(bind-key [remap just-one-space] #'cycle-spacing)

;; Use major modes to configure indentation
(setq-default indent-tabs-mode nil
        tab-width 4)

;; Indent before completing
(setq-default tab-always-indent 'complete)

;; Always display the ibuffer in other window
(setq-default ibuffer-use-other-window t)

;; Newline at end of file
(setq require-final-newline t)

;; Set up rings and registers
(setq kill-ring-max 200
      kill-do-not-save-duplicates t
      save-interprogram-paste-befor-kill t)

;; Disable tabs, but use the correct width
(setq-default indent-tabs-mode nil
              tab-width 4)

;; Indicate empty lines at the end of a buffer
(setq indicate-empty-lines t)

;; Setup a reasonable fill column
(setq-default fill-column 160)
(add-hook 'text-mode-hook #'auto-fill-mode)
(diminish 'auto-fill-function)

;; When typing over a selected region, delete then insert
(delete-selection-mode)

;; Use UTF-8 by default
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)

;; Bring back narrowing
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-defun 'disabled nil)

;; Bring back region casing
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; Extra highlighting
(use-package volatile-highlights
  :ensure t
  :diminish volatile-highlights-mode
  :config
  (volatile-highlights-mode))

;; AFTER volatile-highlights!
;; add cutting the current line without marking it
(use-package rect
  :init
  (defun slick-cut (beg end &optional mode)
  "When called interactively with no active region, kill a single line instead."
  (interactive
   (if mark-active
     (list (region-beginning) (region-end) 'rectangle-mark-mode)
     (list (line-beginning-position) (line-beginning-position 2)))))
  (defun slick-copy (beg end &optional mode)
  "When called interactively with no active region, copy a single line instead."
  (interactive
    (if mark-active
      (list (region-beginning) (region-end))
    (message "Copied line")
    (list (line-beginning-position) (line-beginning-position 2)))))
  (advice-add 'kill-region :before #'slick-cut)
  (advice-add 'kill-ring-save :before #'slick-copy))

;; tramp (for sudo)
;; (use-package tramp
;;   :init
;;   (setq tramp-default-method "ssh"))

;;; Spelling and syntax checking
(use-package ispell                     ; Spell checking
  :defer t
  :config
  (setq ispell-program-name (if (eq system-type 'darwin)
                              (executable-find "aspell")
                              (executable-find "hunspell"))
        ispell-dictionary "en_GB"     ; Default dictionnary
        ispell-silently-savep t       ; Don't ask when saving the private dict
        ;; Increase the height of the choices window to take our header line
        ;; into account.
        ispell-choices-win-default-height 5)

  (unless ispell-program-name
    (warn "No spell checker available.  Install Hunspell or ASpell for OS X.")))

;; On-the-fly spell checking
(use-package flyspell
  :bind
  (("C-c t s" . flyspell-mode)
   ("C-c l b" . flyspell-buffer))
  :init
  (progn (dolist (hook '(text-mode-hook message-mode-hook))
    (add-hook hook 'turn-on-flyspell))
    (add-hook 'prog-mode-hook 'flyspell-prog-mode))
  :config
    (progn
      (setq flyspell-use-meta-tab nil
            ;; Make Flyspell less chatty
            flyspell-issue-welcome-flag nil
            flyspell-issue-message-flag nil)
      ;; Free C-M-i for completion
      (define-key flyspell-mode-map "\M-\t" nil)
      ;; Undefine mouse buttons which get in the way
      (define-key flyspell-mouse-map [down-mouse-2] nil)
      (define-key flyspell-mouse-map [mouse-2] nil))
  :diminish flyspell-mode)

;; Automatically infer dictionary
(use-package auto-dictionary
  :ensure t
  ;; Always change dictionary through adict, because it triggers hooks that let
  ;; us automatically update the "language" for other modes (e.g. Typo Mode) as
  ;; well
  :bind
  (("C-c l l" . adict-change-dictionary)
   ("C-c l g" . adict-guess-dictionary))
  :init
  (add-hook 'flyspell-mode-hook #'auto-dictionary-mode))

;; Turn page breaks into lines
;(use-package page-break-lines
  ;:ensure t
  ;:init
  ;(global-page-break-lines-mode)
  ;:diminish
  ;page-break-lines-mode)

;; keep an eye on whitespace misuse
(use-package whitespace
  ;; built-in
  :init
  (defun enable-whitespace ()
    (whitespace-mode +1))
  :bind
  (("C-c t w" . whitespace-mode))
  :config
  (setq whitespace-line-column -1
        whitespace-style '(face tabs empty trailing lines-tail
                                indentation space-after-tab tab-mark
                                space-after-tab))
  (dolist (hook '(prog-mode-hook text-mode-hook conf-mode-hook))
    (add-hook hook #'enable-whitespace))
  (add-hook 'before-save-hook #'whitespace-cleanup)
  :diminish whitespace-mode)

;; enable change region case commands
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; expand-region
(use-package expand-region
  :ensure t
  :bind
  ("C-c v" . er/expand-region))

;; Buffers, Windows, & Frames
(setq frame-resize-pixelwise t
      ;; Better frame titles (show file name if buffer is file, or buffer name otherwise)
      frame-title-format
      '("" invocation-name " - " (:eval (if (buffer-file-name)
                                          (abbreviate-file-name (buffer-file-name "%b")))))
      window-combination-resize t)

(setq-default line-spacing 0.2) ;; smidge more space between lines

;; Configure 'display-buffer' behaviour for special buffers
(setq display-buffer-alist
      ;; magit status window in a fullscreen window
      `((,(rx "*magit: ")
          (display-buffer-fullframe)
          (reusable-frames . nil))
        ;; Give helm-help a non-side window because it has very peculiar ideas about how to display its help
        (,(rx bos "*Helm Help" (* nonl) "*" eos)
          (display-buffer-use-some-window
           display-buffer-pop-up-window))
        ;; Pin helm to side window
        (,(rx bos "*" (* nonl) "helm" (* nonl) "*" eos)
          (display-buffer-in-side-window)
          (side . bottom)
          (window-height . 0.4)
          (window-width  . 0.6))
        ;; Put REPLs and error lists into the bottom side window
        (,(rx bos
              (or "*Help"
                  "*Warnings*"
                  "*Compile-Log*"
                  "*compilation*"
                  "*Flycheck errors*"
                  "*shell"
                  "*sbt"
                  "*ensime-update*"
                  "*SQL"
                  "*Cargo"
                  (and (1+ nonl) " output*")
                  ))
          (display-buffer-reuse-window
           display-buffer-in-side-window)
          (side . bottom)
          (reusable-frames . visible)
          (window-height . 0.33))
        ("." nil (reusable-frames . visible))))

;; Frame
(use-package frame
  :bind
  (("C-c w F" . toggle-frame-fullscreen))
  :init
  (progn
    ;; kill 'suspend-frame'
    (global-set-key (kbd "C-z") nil)
    (global-set-key (kbd "C-x C-z") nil))
  :config
  (add-to-list 'initial-frame-alist '(fullscreen . fullboth)))

;; Buffer tools (from lunaryorn)
(use-package buffers
  :defer t
  :bind
  (("C-c b k" . lunaryorn/kill-this-buffer))
  :config
  (add-hook 'kill-buffer-query-functions
            'lunaryorn/do-not-kill-important-buffers))

;; Unique buffer names
(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'forward))

;; Better buffer list
(use-package ibuffer
  :bind
  (([remap list-buffers] . ibuffer))
  :config
  ;; show version control status in ibuffer
  (setq ibuffer-formats
        '((mark modified read-only vc-status-mini " "
                (name 18 18 :left :elide)
                " "
                (size 9 -1 :right)
                " "
                (mode 16 16 :left :elide)
                " "
                (vc-status 16 16 :left)
                " "
                filename-and-process)
          (mark modified read-only " "
                (name 18 18 :left :elide)
                " "
                (size 9 -1 :right)
                " "
                (mode 16 16 :left :elide)
                " "
                filename-and-process)
          (mark " " (name 16 -1) " " filename))))

;; Group buffers by project and status
(use-package ibuffer-vc
  :ensure t
  :defer t
  :init
  (add-hook  'ibuffer-hook (lambda ()
                             (ibuffer-vc-set-filter-groups-by-vc-root)
                             (unless (eq ibuffer-sorting-mode 'alphabetic)
                               (ibuffer-do-sort-by-alphabetic)))))

;; Group buffers by Projectile project
(use-package ibuffer-projectile
  :ensure t
  :disabled t
  :defer t
  :init
  (add-hook 'ibuffer-hook #'ibuffer-projectile-set-filter-groups))

;; Projectile
(use-package projectile
  :ensure t
  :init
  (projectile-global-mode)
  (setq projectile-completion-system 'helm
        projectile-enable-caching t
        projectile-find-dir-includes-top-level t)
  :config
  (run-with-idle-timer 10 nil #'projectile-cleanup-known-projects)
  :diminish projectile-mode)

;; Use avy for quick navigation to things
(use-package avy
  :ensure t
  :bind
  (("C-c j j" . avy-goto-word-or-subword-1)
   ("C-c j w" . avy-goto-word-1)
   ("C-c j l" . avy-goto-line)
   ("C-c j m" . avy-pop-mark)
   ("C-c j c" . avy-goto-char-2))
  :init
  (setq avy-background t
        avy-style 'at-full))

;; Use anzu for enhanced isearch & query-replace
(use-package anzu
  :ensure t
  :diminish anzu-mode
  :bind
  (([remap query-replace] . anzu-query-replace)
   ([remap query-replace-regexp] . anzu-query-replace-regexp)
  :map isearch-mode-map
  ([remap isearch-query-replace] . anzu-isearch-query-replace)
  ([remap isearch-query-replace-regexp] . anzu-isearch-query-replace-regexp))
  :init
  (global-anzu-mode)
  :config
  (setq anzu-cons-mode-line-p nil))

;; Ediff enhancements
(use-package ediff
  ;; built-in
  :init
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)) ;; don't start another window

;; Clean up old buffers automatically
(use-package midnight
  ;; built-in
  )

;; Easier killing
(use-package easy-kill
  :ensure t
  :bind
  (([remap kill-ring-save] . easy-kill)
   ([remap mark-sexp] . easy-mark)))

;; Saner regex syntax
(use-package re-builder
  ;; built-in
  :init
  (setq reb-re-syntax 'string))

;;; Terminal emulation and shells
(use-package shell                      ; Dumb shell in Emacs
  :bind
  ("C-c a t" . shell))

(use-package term                       ; Terminal emulator in Emacs
  :bind
  ("C-c a T" . ansi-term))

;; eshell
(use-package eshell
  ;; built-in
  :init
  ;; create a new shell even if there is one already
  (defun new-eshell ()
    (interactive)
    (eshell t))
  (setq eshell-directory-name (expand-file-name "eshell" user-emacs-directory)))

;; Semanticdb
(use-package semantic
  ;; built-in
  :init
  (setq semanticdb-default-save-directory (expand-file-name "semanticdb" user-emacs-directory)))

;; Compile mode
(use-package compile
  ;; built-in
  :bind
  (("C-c c C" . recompile))
  :init
  (setq compilation-ask-about-save nil  ;; just save before compiling, don't ask
        compilation-always-kill t       ;; automatically kill old compile processes before starting a new one
        compilation-scroll-output 'first-error ;; automatically scroll to first error
        compilation-skip-threshold 2    ;; Skip over warnings and info messages
        compilation-disable-input t     ;; Don't freeze when process reads from stdin
        compilation-context-lines 3     ;; Show 3 lines of context around the message
        ))

;; Colorize compile mode
(use-package ansi-color
  ;; built-in
  :init
  (defun colorize-compilation-buffer ()
    "Colorize a compilation mode buffer."
    (interactive)
    ;; don't mess with child modes
    (let ((inhibit-read-only t))
      (ansi-color-apply-on-region compilation-filter-start (point-max))))
  (add-hook 'compilation-filter-hook #'colorize-compilation-buffer))

;; Better undo
(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :config
  (global-undo-tree-mode))

;; Enable winner mode
(use-package winner
  ;; built-in
  :config
  (winner-mode +1))

;; Enable intuitive repeating commands
(use-package smartrep
  :ensure t)

;; Operate on numbers
(use-package operate-on-number
  :ensure t
  :init
  (smartrep-define-key global-map "C-c ."
             '(("+" . apply-operation-to-number-at-point)
             ("-" . apply-operation-to-number-at-point)
             ("*" . apply-operation-to-number-at-point)
             ("/" . apply-operation-to-number-at-point)
             ("\\" . apply-operation-to-number-at-point)
             ("^" . apply-operation-to-number-at-point)
             ("<" . apply-operation-to-number-at-point)
             (">" . apply-operation-to-number-at-point)
             ("#" . apply-operation-to-number-at-point)
             ("%" . apply-operation-to-number-at-point)
             ("'" . apply-operation-to-number-at-point))))

(use-package simple
  :defer t
  :bind
  (("M-g n" . hydras/errors/next-error)
   ("M-g p" . hydras/errors/previous-error))
  :init
  (defhydra hydras/errors ()
    "Errors."
    ("n" next-error "next")
    ("p" previous-error "previous")
    ("f" first-error "first")))

(use-package lunaryorn-editing
  :defer t
  :bind
  (([remap kill-whole-line]        . smart-kill-whole-line)
   ([remap move-beginning-of-line] . back-to-indentation-or-beginning-of-line)
   ("C-<backspace>"                . smart-backward-kill-line)
   ("C-S-j"                        . smart-open-line)
   ("C-<return>"                   . smart-open-line)
   ;; Additional utilities
   ("C-c i d"                      . insert-current-date))
  :commands
  (auto-fill-comments-mode)
  ;; Auto-fill comments in programming modes
  :init
  (add-hook 'prog-mode-hook #'auto-fill-comments-mode))

(use-package delsel                     ; Delete the selection instead of insert
  :defer t
  :init
  (delete-selection-mode))

;; Built-in comment features
(use-package newcomment
  :bind
  (("C-c c d" . comment-dwim)
   ("C-c c l" . comment-line)
   ("C-c c r" . comment-region)))

;; Cleanup whitespace in buffers
(use-package whitespace-cleanup-mode
  :ensure t
  :bind
  (("C-c t c" . whitespace-cleanup-mode)
   ("C-c x w" . whitespace-cleanup))
  :init
  (dolist (hook '(prog-mode-hook text-mode-hook conf-mode-hook))
  (add-hook hook #'whitespace-cleanup-mode))
  :diminish whitespace-cleanup-mode)

(use-package subword                    ; Subword/superword editing
  :defer t
  :init
  (subword-mode 1)
  :diminish subword-mode)

(use-package adaptive-wrap              ; Choose wrap prefix automatically
  :ensure t
  :defer t
  :init
  (add-hook 'visual-line-mode-hook #'adaptive-wrap-prefix-mode))

(use-package visual-fill-column         ; Fill column wrapping for Visual Line Mode
  :ensure t
  :defer t
  :init
  (add-hook 'visual-line-mode-hook #'visual-fill-column-mode))

(use-package zop-to-char                ; Better zapping
  :ensure t
  :bind
  (("M-z" . zop-to-char)
   ("M-Z" . zop-up-to-char)))

(use-package align                      ; Align text in buffers
  :bind
  (("C-c x a a" . align)
   ("C-c x a c" . align-current)))

(use-package lunaryorn-align
  :bind
  (("C-c x a r" . lunaryorn/align-repeat)
   ("C-c x a m" . lunaryorn/align-repeat-math-oper)
   ("C-c x a ." . lunaryorn/align-repeat-decimal)
   ("C-c x a ," . lunaryorn/align-repeat-comma)
   ("C-c x a ;" . lunaryorn/align-repeat-semicolon)
   ("C-c x a :" . lunaryorn/align-repeat-colon)
   ("C-c x a =" . lunaryorn/align-repeat-equal)
   ("C-c x a &" . lunaryorn/align-repeat-ampersand)
   ("C-c x a |" . lunaryorn/align-repeat-bar)
   ("C-c x a (" . lunaryorn/align-repeat-left-paren)
   ("C-c x a )" . lunaryorn/align-repeat-right-paren)))

;(use-package multiple-cursors           ; Edit text with multiple cursors
  ;:ensure t
  ;:bind
  ;(("C-c o <SPC>" . mc/vertical-align-with-space)
   ;("C-c o a"     . mc/vertical-align)
   ;("C-c o e"     . mc/mark-more-like-this-extended)
   ;("C-c o h"     . mc/mark-all-like-this-dwim)
   ;("C-c o l"     . mc/edit-lines)
   ;("C-c o n"     . mc/mark-next-like-this)
   ;("C-c o p"     . mc/mark-previous-like-this)
   ;("C-c o r"     . vr/mc-mark)
   ;("C-c o C-a"   . mc/edit-beginnings-of-lines)
   ;("C-c o C-e"   . mc/edit-ends-of-lines)
   ;("C-c o C-s"   . mc/mark-all-in-region))
  ;:config
  ;(setq mc/mode-line
        ;;; Simplify the MC mode line indicator
        ;'(:propertize (:eval (concat " " (number-to-string (mc/num-cursors))))
                      ;face font-lock-warning-face)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Autocompletion
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package company
  :ensure t
  :bind
  ("C-;" . company-complete-common)
  :init
  (add-hook 'after-init-hook 'global-company-mode)
  :config
  (setq company-idle-delay 0.5
        company-minimum-prefix-length 2
        company-show-numbers t
        company-tooltip-limit 10
        company-abbrev-downcase nil
        company-tooltip-align-annotations t
        ;; invert the navigation direction if pop-up is above point
        company-tooltip-flip-when-above t))

;;; Sort company candidates by statistics
;(use-package company-statistics
  ;:ensure t
  ;:after company
  ;:if window-system
  ;:config
  ;(company-statistics-mode))

;;; Completion for Math symbols
;(use-package company-math
  ;:ensure t
  ;:after company
  ;:config
  ;;; Add backends for math characters
  ;(add-to-list 'company-backends 'company-math-symbols-unicode)
  ;(add-to-list 'company-backends 'company-math-symbols-latex))

;;; Emojis completion like Github/Slack
;(use-package company-emoji
  ;:ensure t
  ;:after company
  ;:config
  ;(add-to-list 'company-backends 'company-emoji))

(use-package company-anaconda           ; Python backend for Company
  :ensure t
   :after company
  :config (add-to-list 'company-backends 'company-anaconda))

;;; If less than 5 completion candidates, cycle instead of pop-up
;(setq completion-cycle-threshold 5)

;; Replace dabbrev with hippie-expand
(use-package hippie-exp
  ;; built-in
  :bind
  (([remap dabbrev-expand] . hippie-expand))
  :init
  (setq hippie-expand-try-functions-list
            '(try-expand-dabbrev                      ; expand from current
              try-expand-dabbrev-all-buffers          ; expand from all other buffers
              try-expand-dabbrev-from-kill            ; expand from the kill ring
              try-complete-file-name-partially        ; complete text as file name
              try-complete-file-name
              try-expand-all-abbrevs                  ; expand word before point from all abbrev tables
              try-expand-list                         ; expand current line to entire line in buffer
              try-expand-line
              try-complete-lisp-symbol-partially      ; complete LISP symbol
              try-complete-lisp-symbol)))

;; Automatic insertion into new files
(use-package auto-insert
  :defer t
  :bind
  (("C-c i a" . auto-insert)))

;; Deal with copyright notices
;(use-package copyright
  ;:defer t
  ;:bind
  ;(("C-c i c" . copyright-update))
  ;:init
  ;;; Update copyright when visiting files
  ;(defun lunaryorn-copyright-update ()
    ;(interactive)
    ;(unless buffer-read-only
      ;(copyright-update nil 'interactive)
      ;(unless copyright-update
        ;;; Fix years when the copyright information was updated
        ;(copyright-fix-years))))
  ;(add-hook 'find-file-hook #'lunaryorn-copyright-update)
  ;:config
  ;;; Use ranges to denote consecutive years
  ;(setq copyright-year-ranges t
        ;;; Limit copyright changes to my own copyright
        ;copyright-names-regexp (regexp-quote user-full-name)))

;; Smarter M-x - smex
(use-package smex
  :ensure t
  :bind
  (("M-x" . smex)
   ("M-X" . smex-major-mode-commands))
  :init
  ;; remember most frequently used items
  (setq smex-save-file (expand-file-name ".smex-mru" user-emacs-directory))
  (smex-initialize))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Helm
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package helm
  :ensure t
  :bind
  (("C-c h l" . helm-resume))
  :init
  (require 'helm-mode)
  (helm-mode)
  (with-eval-after-load 'helm-config
    (warn "`helm-config' loaded!"))
  :config
  (setq helm-split-window-in-side-p t)
  :diminish helm-mode)

(use-package helm-buffers
  :ensure helm
  :defer t
  :bind
  (([remap switch-to-buffer] . helm-mini))
  :config
  (setq helm-buffers-fuzzy-matching t))

(use-package helm-descbinds
  :ensure t
  :init
  (helm-descbinds-mode))

(use-package helm-imenu
  :ensure helm
  :defer t
  :bind
  (("C-c j t" . helm-imenu))
  :config
  (setq helm-imenu-fuzzy-match t
        ;; No insta-jumps please
        helm-imenu-execute-action-at-once-if-one nil))

(use-package helm-projectile            ; Helm frontend for Projectile
  :ensure t
  :after projectile
  :init
  (setq helm-projectile-fuzzy-match nil)
  :bind
  (("C-c s p" . helm-projectile-ag)
   :map helm-projectile-projects-map
        ("C-t" . lunaryorn-neotree-project-root))
  :config
  (helm-projectile-on)
  (setq projectile-switch-project-action #'helm-projectile)
  (helm-add-action-to-source "Open NeoTree `C-t'"
                             #'lunaryorn-neotree-project-root
                             helm-source-projectile-projects 1))

;(use-package helm-gtags
  ;:ensure t
  ;:init
  ;(setq helm-gtags-fuzzy-match nil
        ;helm-gtags-direct-helm-completing t
        ;helm-gtags-display-style 'detail
        ;helm-gtags-ignore-case t
        ;helm-gtags-prefix-key "\C-t"
        ;helm-gtags-suggested-key-mapping t)
  ;:diminish helm-gtags-mode)

;; Regex search using helm
(use-package helm-regexp
  :ensure helm
  :defer t
  :bind
  (([remap occur] . helm-occur)))

(use-package helm-man                   ; Man pages with Helm
  :ensure helm
  :defer t
  :bind (("C-c h m" . helm-man-woman)))

(use-package helm-info                  ; Info pages with Helm
  :ensure helm
  :bind
  (([remap info] . helm-info-at-point)
   ("C-c h e"    . helm-info-emacs)))

;; Manage files with Helm
(use-package helm-files
  :ensure helm
  :defer t
  :bind
  (([remap find-file] . helm-find-files)
   ("C-c f f" . helm-for-files)
   ("C-c f r" . helm-recentf))
  :config
  (setq helm-recentf-fuzzy-match t
        helm-ff-file-name-history-use-recentf t
        helm-ff-search-library-in-sexp t))

;; Search buffers with helm
(use-package helm-swoop
  :ensure t
  :bind
  (("C-c s s" . helm-swoop)
   ("C-c s S" . helm-multi-swoop)
   ("C-c s C-s" . helm-multi-swoop-all))
  :config
  (setq helm-swoop-speed-or-color t
        helm-swoop-split-window-function #'helm-default-display-buffer))

;; Use ag from helm
(use-package helm-ag
  :ensure t
  :bind
  (("C-c s a" . helm-ag)
   ("C-c s A" . helm-do-ag))
  :config
  (setq helm-ag-fuzzy-match t
        helm-ag-insert-at-point 'symbol
        helm-ag-edit-save t))

;; Browse rings and registers with Helm
(use-package helm-ring
  :ensure helm
  :defer t
  :bind
  (([remap yank-pop] . helm-show-kill-ring)
   ([remap insert-register] . helm-register)))

;; Run makefile targets through Helm
;(use-package helm-make
  ;:ensure t
  ;:bind
  ;(("C-c c c" . helm-make-projectile)
  ;;; FIXME: Write a more sophisticated command that checks whether a
  ;;; Makefile exists and falls back to an alternative if not.
  ;("<f5>" . helm-make-projectile)))

;; Input unicode with helm
;(use-package helm-unicode
  ;:ensure t
  ;:bind
  ;("C-c i 8" . helm-unicode))

;; Helm frontend for company
(use-package helm-company
  :ensure t
  :defer t
  :bind
  (:map company-mode-map
     ([remap complete-symbol] . helm-company)
     ([remap completion-at-point] . helm-company)
   :map company-active-map
     ("C-:" . helm-company)))

(use-package helm-elisp                 ; Helm commands for elisp
  :ensure helm
  :defer t
  :bind (([remap apropos-command] . helm-apropos)
         ("C-c f l" . helm-locate-library)))

;(use-package helm-gitignore             ; Generate gitignore files
  ;:ensure t
  ;:defer t
  ;:bind
  ;("C-c g I" . helm-gitignore))

;(use-package helm-open-github ; Open Github pages for current repo
  ;;; FIXME: Triggers a password prompt during load?!
  ;:disabled t
  ;:ensure t
  ;:bind
  ;(("C-c g g i" . helm-open-github-from-issues)
   ;("C-c g g p" . helm-open-github-from-pull-requests)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Programming (General)
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Base config for all programming modes
(use-package prog-mode
  ;; built-in
  :init
  ;; Hook functions
  (defun font-lock-comment-annotations ()
  "Highlight a bunch of common comment annotations."
  (font-lock-add-keywords
    nil '(("\\<\\(\\(FIX\\(ME\\)?\\|TODO\\|OPTIMIZE\\|HACK\\|REFACTOR\\):\\)"
           1 font-lock-warning-face t))))
  (set (make-local-variable 'comment-auto-fill-only-comments) t)
  (setq font-lock-maximum-decoration 2)
  :config
  (add-hook 'prog-mode-hook #'font-lock-comment-annotations)
  :bind
  (("C-c t p" . prettify-symbols-mode)))

;; Show the name of the current function in the modeline
(use-package which-func
  ;; built-in
  :init
  (add-hook 'prog-mode-hook #'which-func-mode)
  :config
  (setq which-func-unknown "|"
        which-func-format
            `((:propertize (" > " which-func-current
                            local-map ,which-func-keymap
                            face which-func
                            mouse-face mode-line-highlight
                            help-echo "mouse-1: go to beginning\n\
mouse-2: toggle rest visibility\n\
mouse-3: go to end")))))

;; Use flycheck for as-you-type syntax checking
(use-package flycheck                   ; On-the-fly syntax checking
  :ensure t
  :bind
  (("C-c e" . hydras/flycheck-errors/body)
   ("C-c t f" . flycheck-mode))
  :init
  (defhydra hydras/flycheck-errors ()
    "Flycheck errors."
    ("n" flycheck-next-error "next")
    ("p" flycheck-previous-error "previous")
    ("f" flycheck-first-error "first")
    ("l" flycheck-list-errors "list")
    ("w" flycheck-copy-errors-as-kill "copy message")
    ;; See `helm-flycheck' package below
    ("h" helm-flycheck "list with helm"))

  :config
  (if (fboundp 'global-flycheck-mode)
    (global-flycheck-mode +1))
  (add-hook 'prog-mode-hook 'flycheck-mode)
  (setq flycheck-standard-error-navigation nil
        flycheck-display-errors-function
        #'flycheck-display-error-messages-unless-error-list
        flycheck-scalastylerc "scalastyle_config.xml")
  :diminish flycheck-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Programming - C/C++
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package cc-mode
  :ensure t
  :bind (:map c-mode-base-map
        ("RET" . newline-and-indent))
  :init
  (setq c-default-style "stroustrup"
        c-basic-offset 4
        c-toggle-hungry-state 1)
  (add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
  (defun my-c++-mode-hook ()
    (electric-indent-mode)
    (diminish 'electric-mode)
    (c-set-style "stroustrup")
    (c-set-offset 'innamespace '0) ;; don't indent when in namespace
  )
  (add-hook 'c-mode-hook 'helm-gtags-mode)
  (add-hook 'c++-mode-hook 'helm-gtags-mode)
  (add-hook 'c++-mode-hook 'my-c++-mode-hook))

(use-package ggtags
  :ensure t
  :bind
  :init
  (add-hook 'c-mode-common-hook
      (lambda ()
        (when (derived-mode-p 'c-mode 'c++-mode)
        (ggtags-mode +1)))))

(use-package rtags
  :ensure t
  :defer t
  :init
  (require 'company)
  (require 'flycheck)

  (use-package flycheck-rtags
    :config
    (defun my-flycheck-rtags-hook ()
      (flycheck-select-checker 'rtags)
      (setq-local flycheck-highlighting-mode nil)
      (setq-local flycheck-check-syntax-automatically nil))
    (setq rtags-autostart-diagnostics t
          flycheck-disabled-checkers '(c/c++-clang c/c++-gcc))
    (add-hook 'c-mode-common-hook #'my-flycheck-rtags-hook))
  (use-package company-rtags
    :bind
    (:map c-mode-base-map
          ("<C-tab>" . company-complete))
    :config
    (defun my-company-rtags-hook ()
    (setq company-rtags-begin-after-member-access t
          rtags-autostart-diagnostics t
          rtags-completions-enabled t
          company-backends (delete 'company-clang company-backends)))
    (push 'company-rtags company-backends)
  (add-hook 'c-mode-common-hook #'my-company-rtags-hook))

  (defun my-rtags-cc-mode-hook ()
    (bind-key "C-c r q" #'rtags-restart-process c-mode-base-map)
    (bind-key "C-c r f" #'rtags-find-symbol-at-point c-mode-base-map)
    (bind-key "C-c r F" #'rtags-find-symbol c-mode-base-map)
    (bind-key "C-c r r" #'rtags-find-references c-mode-base-map)
    (bind-key "C-c r d" #'rtags-diagnostics c-mode-base-map)
    (bind-key "C-c r v" #'rtags-find-virtuals-at-point c-mode-base-map)
    (bind-key "C-c r m" #'rtags-fix-fixit-at-point c-mode-base-map)
    (bind-key "C-c r ]" #'rtags-location-stack-back c-mode-base-map)
    (bind-key "C-c r [" #'rtags-location-stack-forward c-mode-base-map)
    (bind-key "C-c r n" #'rtags-next-match c-mode-base-map)
    (bind-key "C-c r p" #'rtags-previous-match c-mode-base-map)
    (bind-key "C-c r P" #'rtags-preprocess-file c-mode-base-map)
    (bind-key "C-c r s" #'rtags-print-symbol-info c-mode-base-map)
    (bind-key "C-c r t" #'rtags-symbol-type c-mode-base-map)
    (bind-key "C-c r D" #'rtags-print-dependencies c-mode-base-map)
    (bind-key "C-c r e" #'rtags-print-enum-value-at-point c-mode-base-map)
    (bind-key "C-c r Q" #'rtags-quit-rdm c-mode-base-map)
    (bind-key "C-c r R" #'rtags-rename-symbol c-mode-base-map)
    (bind-key "C-c r b" #'rtags-show-rtags-buffer c-mode-base-map)
    (bind-key "C-c r I" #'rtags-include-file c-mode-base-map)
    (bind-key "C-c r i" #'rtags-get-include-file-for-symbol c-mode-base-map))
  (add-hook 'c-mode-common-hook #'rtags-start-process-unless-running)
  (add-hook 'c-mode-common-hook #'my-rtags-cc-mode-hook)
  (setq rtags-tramp-enabled t))

;(use-package irony
;  :ensure t
;  :init
;  (defun my-irony-mode-hook ()
;    (define-key irony-mode-map [remap completion-at-point] #'irony-completion-at-point-async)
;    (define-key irony-mode-map [remap complete-symbol] #'irony-completion-at-point-async))
;  (add-hook 'irony-mode-hook #'my-irony-mode-hook)
; (add-hook 'irony-mode-hook #'irony-cdb-autosetup-compile-options)
; (add-hook 'c-mode-hook (lambda () (setq irony-additional-clang-options '("-std=c11"))))
; (add-hook 'c++-mode-hook (lambda () (setq irony-addition-clang-options '("-std=c++11")))))

;(use-package cmake-ide
;  :ensure t
;  :init
;)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Programming - LaTeX
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(use-package table                      ; Edit tables in text files
  ;:defer t
  ;:init
  ;(add-hook 'text-mode-hook #'table-recognize))

;(use-package tildify                    ; Insert non-breaking spaces on the fly
  ;:defer t
  ;:bind (("C-c x t" . tildify-region)
  ;("C-c t ~" . tildify-mode))
  ;:config
  ;;; Use the right space for LaTeX
  ;(add-hook 'latex-mode-hook
    ;(lambda () (setq-local tildify-space-string "~"))))

;(use-package typo                       ; Automatically use typographic quotes
  ;:ensure t
  ;:bind (("C-c t t" . typo-mode)
  ;("C-c l L" . typo-change-language))
  ;:init
  ;(typo-global-mode)
  ;(add-hook 'text-mode-hook #'typo-mode)
  ;:config
  ;;; TODO: Automatically set from ispell dictionary in
  ;;; `adict-change-dictionary-hook', to update the typo language whenever the
  ;;; spelling language changed
  ;(setq-default typo-language "English")
  ;:diminish typo-mode)

;;;; LaTeX with AUCTeX
;(use-package tex-site                   ; AUCTeX initialization
  ;:ensure auctex)

;(use-package tex                        ; TeX editing/processing
  ;:ensure auctex
  ;:defer t
  ;:config
  ;(setq TeX-parse-self t                ; Parse documents to provide completion
                                        ;; for packages, etc.
        ;TeX-auto-save t                 ; Automatically save style information
        ;TeX-electric-sub-and-superscript t ; Automatically insert braces after
                                           ;; sub- and superscripts in math mode
        ;TeX-electric-math '("\\(" "\\)")
        ;;; Don't insert magic quotes right away.
        ;TeX-quote-after-quote t
        ;;; Don't ask for confirmation when cleaning
        ;TeX-clean-confirm nil
        ;;; Provide forward and inverse search with SyncTeX
        ;TeX-source-correlate-mode t
        ;TeX-source-correlate-method 'synctex)
        ;(setq-default TeX-master nil          ; Ask for the master file
                      ;TeX-engine 'luatex      ; Use a modern engine
                      ;;; Redundant in 11.88, but keep for older AUCTeX
                      ;TeX-PDF-mode t)

  ;;; Move to chktex
  ;(setcar (cdr (assoc "Check" TeX-command-list)) "chktex -v6 %s"))

;(use-package tex-buf                    ; TeX buffer management
  ;:ensure auctex
  ;:defer t
  ;;; Don't ask for confirmation when saving before processing
  ;:config
  ;(setq TeX-save-query nil))

;(use-package tex-style                  ; TeX style
  ;:ensure auctex
  ;:defer t
  ;:config
  ;;; Enable support for csquotes
  ;(setq LaTeX-csquotes-close-quote "}"
        ;LaTeX-csquotes-open-quote "\\enquote{"))

;(use-package tex-fold                   ; TeX folding
  ;:ensure auctex
  ;:defer t
  ;:init
  ;(add-hook 'TeX-mode-hook #'TeX-fold-mode))

;(use-package tex-mode                   ; TeX mode
  ;:ensure auctex
  ;:defer t
  ;:config
  ;(font-lock-add-keywords 'latex-mode
                          ;`((,(rx "\\"
                                  ;symbol-start
                                  ;"fx" (1+ (or (syntax word) (syntax symbol)))
                                  ;symbol-end)
                             ;. font-lock-warning-face))))

;(use-package latex                      ; LaTeX editing
  ;:ensure auctex
  ;:defer t
  ;:config
  ;;; Teach TeX folding about KOMA script sections
  ;(setq TeX-outline-extra `((,(rx (0+ space) "\\section*{") 2)
                            ;(,(rx (0+ space) "\\subsection*{") 3)
                            ;(,(rx (0+ space) "\\subsubsection*{") 4)
                            ;(,(rx (0+ space) "\\minisec{") 5))
        ;;; No language-specific hyphens please
        ;LaTeX-babel-hyphen nil)

  ;(add-hook 'LaTeX-mode-hook #'LaTeX-math-mode))    ; Easy math input

;(use-package auctex-latexmk             ; latexmk command for AUCTeX
  ;:ensure t
  ;:defer t
  ;:after latex
  ;:config (auctex-latexmk-setup))

;(use-package bibtex                     ; BibTeX editing
  ;:defer t
  ;:config
  ;;; Run prog mode hooks for bibtex
  ;(add-hook 'bibtex-mode-hook (lambda () (run-hooks 'prog-mode-hook)))

  ;;; Use a modern BibTeX dialect
  ;(bibtex-set-dialect 'biblatex))

;(use-package reftex                     ; TeX/BibTeX cross-reference management
  ;:defer t
  ;:init (add-hook 'LaTeX-mode-hook #'reftex-mode)
  ;:config
  ;;; Plug into AUCTeX
  ;(setq reftex-plug-into-AUCTeX t
        ;;; Automatically derive labels, and prompt for confirmation
        ;reftex-insert-label-flags '(t t)
        ;reftex-label-alist
        ;'(
          ;;; Additional label definitions for RefTeX.
          ;("definition" ?d "def:" "~\\ref{%s}"
           ;lunaryorn-reftex-find-ams-environment-caption
          ;("definition" "def.") -3)
          ;("theorem" ?h "thm:" "~\\ref{%s}"
           ;lunaryorn-reftex-find-ams-environment-caption
          ;("theorem" "th.") -3)
          ;("example" ?x "ex:" "~\\ref{%s}"
           ;lunaryorn-reftex-find-ams-environment-caption
          ;("example" "ex") -3)
          ;;; Algorithms package
          ;("algorithm" ?a "alg:" "~\\ref{%s}"
           ;"\\\\caption[[{]" ("algorithm" "alg") -3)))
  ;;; Provide basic RefTeX support for biblatex
  ;(unless (assq 'biblatex reftex-cite-format-builtin)
    ;(add-to-list 'reftex-cite-format-builtin
                 ;'(biblatex "The biblatex package"
                            ;((?\C-m . "\\cite[]{%l}")
                            ;(?t . "\\textcite{%l}")
                            ;(?a . "\\autocite[]{%l}")
                            ;(?p . "\\parencite{%l}")
                            ;(?f . "\\footcite[][]{%l}")
                            ;(?F . "\\fullcite[]{%l}")
                            ;(?x . "[]{%l}")
                            ;(?X . "{%l}"))))
    ;(setq reftex-cite-format 'biblatex))
  ;:diminish reftex-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Programming - Markdown/JSON/etc.
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(use-package markdown-mode              ; Markdown
  ;:ensure t
  ;;; Just no, dear Markdown Mode.  Don't force that bastard Github dialect upon
  ;;; me!
  ;:mode ("\\.md\\'" . markdown-mode)
  ;:config
  ;;; Process Markdown with Pandoc, using a custom stylesheet for nice output
  ;(let ((stylesheet (expand-file-name
    ;(locate-user-emacs-file "etc/pandoc.css"))))
    ;(setq markdown-command
    ;(mapconcat #'shell-quote-argument
               ;`("pandoc" "--toc" "--section-divs"
                 ;"--css" ,(concat "file://" stylesheet)
                 ;"--standalone" "-f" "markdown" "-t" "html5")
                 ;" ")))

  ;;; No filling in GFM, because line breaks are significant.
  ;(add-hook 'gfm-mode-hook #'turn-off-auto-fill)
  ;;; Use visual lines instead
  ;(add-hook 'gfm-mode-hook #'visual-line-mode)
  ;(add-hook 'gfm-mode-hook #'lunaryorn-whitespace-style-no-long-lines)
  ;(bind-key "C-c C-s C" #'markdown-insert-gfm-code-block markdown-mode-map)
  ;(bind-key "C-c C-s P" #'markdown-insert-gfm-code-block markdown-mode-map)

  ;;; Fight my habit of constantly pressing M-q.  We should not fill in GFM
  ;;; Mode.
  ;(bind-key "M-q" #'ignore gfm-mode-map))

;(use-package yaml-mode                  ; YAML
  ;:ensure t
  ;:defer t
  ;:config
  ;(add-hook 'yaml-mode-hook (lambda () (run-hooks 'prog-mode-hook))))

;(use-package json-mode                  ; JSON files
  ;:ensure t
  ;:defer t
  ;:config
  ;(add-hook 'json-mode-hook
    ;;; Fix JSON mode indentation
    ;(lambda () (setq-local js-indent-level 4))))

;(use-package json-reformat              ; Reformat JSON
  ;:ensure t
  ;:defer t
  ;:bind
  ;(("C-c x j" . json-reformat-region)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Programming - Python
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package python                     ; Python editing
  :defer t
  :config
  ;; PEP 8 compliant filling rules, 79 chars maximum
  (add-hook 'python-mode-hook (lambda () (setq fill-column 79)))
  (add-hook 'python-mode-hook #'subword-mode)
  (let ((ipython (executable-find "ipython")))
    (if ipython
      (setq python-shell-interpreter ipython)
      (warn "IPython is missing, falling back to default python"))))

(use-package anaconda-mode              ; Powerful Python backend for Emacs
  :ensure t
  :defer t
  :init
  (add-hook 'python-mode-hook #'anaconda-mode))

(use-package pip-requirements           ; requirements.txt files
  :ensure t
  :defer t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Programming - SQL
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Databases
(use-package sql                        ; SQL editing and REPL
  :bind
  (("C-c a s" . sql-connect)
   :map sql-mode-map
        ("C-c m p" . sql-set-product)))

(use-package sqlup-mode                 ; Upcase SQL keywords
  :ensure t
  :bind
  (:map sql-mode-map
        ("C-c m u" . sqlup-capitalize-keywords-in-region))
  :config (add-hook 'sql-mode-hook #'sqlup-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Programming - Emacs Lisp
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(bind-key "C-c t d" #'toggle-debug-on-error)

;(use-package elisp-slime-nav            ; Jump to definition of symbol at point
  ;:ensure t
  ;:init (add-hook 'emacs-lisp-mode-hook #'elisp-slime-nav-mode)
  ;:bind
  ;(:map elisp-slime-nav-mode-map
        ;("C-c h ." . elisp-slive-nav-describe-elisp-thing-at-point))
  ;:config
  ;(dolist (key '("C-c C-d d" "C-c C-d C-d"))
    ;(define-key elisp-slime-nav-mode-map (kbd key) nil))
  ;:diminish elisp-slime-nav-mode)

;(use-package pcre2el                    ; Convert regexps to RX and back
  ;:disabled t
  ;:ensure t
  ;:init (rxt-global-mode))

;(use-package ielm                       ; Emacs Lisp REPL
  ;:bind
  ;(("C-c a '" . ielm)))

;(use-package elisp-mode                 ; Emacs Lisp editing
  ;:defer t
  ;:interpreter ("emacs" . emacs-lisp-mode)
  ;:bind (:map emacs-lisp-mode-map
              ;("C-c m e r" . eval-region)
              ;("C-c m e b" . eval-buffer)
              ;("C-c m e e" . eval-last-sexp)
              ;("C-c m e f" . eval-defun)))

;; (use-package el-search                  ; pcase-based search for elisp
;;   :ensure t
;;   :bind (:map emacs-lisp-mode-map
;;               ("C-c m s" . el-search-pattern)
;;               ("C-c m r" . el-search-query-replace)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Documents
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(use-package doc-view
  ;:defer t
  ;:config
  ;;; Render PDFs at 300dpi
  ;(setq doc-view-resolution 300)

  ;(defconst lunaryorn-doc-view-mutool-program "mutool")

  ;(defun lunaryorn-doc-view-pdf->png-converter-mutool (pdf png page callback)
    ;"Convert a PDF file to PNG at PAGE.
;After conversion invoke CALLBACK.  See `doc-view-start-process'
;for more information about CALLBACK."
    ;(doc-view-start-process
     ;"pdf->png" lunaryorn-doc-view-mutool-program
     ;`("draw"
       ;,(concat "-o" png)
       ;,(format "-r%d" (round doc-view-resolution))
       ;,pdf
       ;,@(if page `(,(format "%d" page))))
     ;callback))

  ;;; If `mutool' exists use our own converter function to call "mutool draw".
  ;;; Otherwise check whether docview found mudraw and warn if it didn't
  ;(if (executable-find lunaryorn-doc-view-mutool-program)
    ;(setq doc-view-pdf->png-converter-function
          ;#'lunaryorn-doc-view-pdf->png-converter-mutool)
    ;;; Warn if Doc View falls back to Ghostscript for rendering
    ;(unless (eq doc-view-pdf->png-converter-function
                ;'doc-view-pdf->png-converter-mupdf)
      ;(warn "Doc View is not using mupdf!"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; File management
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Core file commands
(use-package files
  :bind
  (("C-c f z" . revert-buffer)
   ("C-c f /" . revert-buffer))
)

;; Find files at point
(use-package ffap
  :defer t
  :config
  (setq ffap-machine-p-known 'reject))

;; Server for emacsclient
(use-package server
  :defer t
  :init
  (server-mode)
  :diminish
  server-buffer-clients)

;; Dired
(use-package dired
  :defer t
  :config
  (setq dired-auto-revert-buffer t
        dired-listing-switches "-alhF --group-directories-first -v"
        dired-ls-F-marks-symlinks t
        dired-recursive-copies 'always
        dired-recursive-deletes 'top    ;; Only ask for top dir when deleting
        dired-dwim-target t))           ;; Use the dired buffer in other window if it exists

;; Dired enhancements
(use-package dired-x
  ;; built-in
  :init
  (add-hook 'dired-mode-hook #'dired-omit-mode)
  :after dired
  :config
  (put 'dired-find-alternate-file 'disabled nil)  ;; Re-use current buffer when pressing 'a'
  (setq dired-dwim-target t)
  ;; this is a hack to correctly diminish `dired-omit-mode'
  (add-function :after (symbol-function 'dired-omit-startup)
                (lambda () (diminish 'dired-omit-mode))
                '((name . dired-omit-mode-diminish))))

;; Neotree
;(use-package neotree
  ;:ensure t
  ;:bind
  ;(("C-c f t" . neotree-toggle))
  ;:config
  ;(setq neo-window-width 32
        ;neo-create-file-auto-open t
        ;neo-banner-message nil
        ;neo-show-updir-line nil
        ;neo-mode-line-type 'neotree
        ;neo-smart-open t
        ;neo-dont-be-alone t
        ;neo-persist-show nil
        ;neo-show-hidden-files t
        ;neo-auto-indent-point t))

;; Ignore uninteresting files
(use-package ignoramus
  :ensure t
  :config
  (ignoramus-setup))

;; Protect delicate files
(use-package hardhat
  :ensure t
  :init
  (global-hardhat-mode)
  :diminish
  hardhat-mode)

;; Bookmarks for buffers
;(use-package bookmark
  ;:bind (("C-c f b" . list-bookmarks))
  ;:config
  ;(setq bookmark-save-flag 1))

;; Save recently visited files
(use-package recentf
  :init (recentf-mode)
  :config
  (setq recentf-max-saved-items 200
        recentf-max-menu-items 15
        ;; Cleanup recent files only when Emacs is idle, but not when the mode
        ;; is enabled, because that unnecessarily slows down Emacs. My Emacs
        ;; idles often enough to have the recent files list clean up regularly
        ;; TODO: watch for this to have an effect
        recentf-auto-cleanup 300
        recentf-exclude (list "/\\.git/.*\\'" ; Git contents
        "/elpa/.*\\'" ; Package files
        ;; And all other kinds of boring files
        #'ignoramus-boring-p)))

;; Save position in files
;(use-package saveplace
  ;:init
  ;(save-place-mode 1))

;; View read-only files
(setq view-read-only t)

;; Auto-revert buffers of changed files
(use-package autorevert
  :init (global-auto-revert-mode)
  :config
  (setq auto-revert-verbose nil         ; Shut up, please!
        ;; Revert Dired buffers, too
        global-auto-revert-non-file-buffers t)

  :diminish auto-revert-mode)

;; Visit images as images
(use-package image-file
  :init
  (auto-image-file-mode))

;; Open files in external programs
(use-package launch
  :ensure t
  :defer t)

;; Edit files as root (uses Tramp)
(use-package sudo-edit
  :ensure t
  :defer t
  :bind
  (("C-c f s" . sudo-edit)
   ("C-c f S" . sudo-edit-current-file)))

;; File tools from lunaryorn
(use-package files
  :commands (recompile-packages)
  :bind (("C-c f D" . delete-file-and-buffer)
         ("C-c f i" . open-in-intellij)
         ("C-c f o" . launch-dwim)
         ("C-c f R" . rename-file-and-buffer)
         ("C-c f w" . copy-filename-as-kill)
         ("C-c f u" . find-user-init-file-other-window)
         ("C-c f ." . browse-feature-url)))

;; Additional bindings for built-ins
(bind-key "C-c f v d" #'add-dir-local-variable)
(bind-key "C-c f v d" #'add-file-local-variable)
(bind-key "C-c f v d" #'add-file-local-variable-prop-line)

;; Page navigation
(use-package page
  :bind
  (("C-x ]" . hydras/pages/forward-page)
   ("C-x [" . hydras/pages/backward-page))
  :init
  (defhydra hydras/pages ()
    "Pages"
    ("[" backward-page "backward")
    ("]" forward-page  "forward")))

;; Page outlines
;(use-package outline
  ;:defer t
  ;:init
  ;(dolist (hook '(text-mode-hook prog-mode-hook))
    ;(add-hook hook #'outline-minor-mode))
  ;:diminish outline-minor-mode)

;; Line numbers in margin
(use-package nlinum
  :ensure t
  :bind
  (("C-c t l" . nlinum-mode)))

;; Search buffers
(use-package "isearch"
  :defer t
  :init
  (diminish 'isearch-mode))

;; Visual regex
(use-package visual-regexp
  :ensure t
  :bind
  (("C-c s r" . vr/query-replace)
   ("C-c s R" . vr/replace)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Documentation
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(bind-key "C-c h b" #'describe-personal-keybindings)

;;; Online Help
(use-package find-func                  ; Find function/variable definitions
  :bind
  (("C-c h F"   . find-function)
   ("C-c h 4 F" . find-function-other-window)
   ("C-c h K"   . find-function-on-key)
   ("C-c h V"   . find-variable)
   ("C-c h 4 V" . find-variable-other-window)))

(use-package info                       ; Info manual viewer
  :defer t
  :config
  ;; Fix the stupid `Info-quoted' face.  Courier is an abysmal face, so go back
  ;; to the default face.
  ;;(set-face-attribute 'Info-quoted nil :family 'unspecified
  ;;                    :inherit font-lock-type-face))
  )

(use-package niceify-info               ; Prettify Info rendering
  :disabled t
  :ensure t
  ;; Adds emphasis to text between * and _, tries to fontify Emacs Lisp code,
  ;; tries to cross-reference symbol names in backticks, tries to fontify
  ;; headers, etc.q
  :init (add-hook 'Info-selection-hook #'niceify-info))

;(use-package ansible-doc                ; Documentation lookup for Ansible
  ;:ensure t
  ;:defer t
  ;:init
  ;(add-hook 'yaml-mode-hook #'ansible-doc-mode)
  ;:diminish ansible-doc-mode)

;(use-package dash-at-point              ; Jump to Dash docset at point
  ;:ensure t
  ;:defer t
  ;:bind
  ;(("C-c h d" . dash-at-point)
   ;("C-c h D" . dash-at-point-with-docset)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Version Control
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package vc-hooks                   ; Simple version control
  :defer t
  :config
  ;; Always follow symlinks to files in VCS repos
  (setq vc-follow-symlinks t))

;(use-package what-the-commit            ; Insert random commit messages
  ;:ensure t
  ;:bind
  ;(("C-c i w" . what-the-commit-insert)
   ;("C-c g w" . what-the-commit)))

;; Highlighting for diffs
(use-package diff-hl
  :ensure t
  :defer t
  :init
  (global-diff-hl-mode 1)
  :config
  (add-hook 'dired-mode-hook 'diff-hl-dired-mode)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  (unless (display-graphic-p)
    (diff-hl-margin-mode)))

;; Magit needs no explanation
(use-package magit
  :ensure t
  :bind
  (("C-c g l" . magit-log)
   ("C-c g f" . magit-file-log)
   ("C-c g B" . magit-blame-mode)
   ("C-c g b" . magit-branch)
   ("C-c g c" . magit-checkout)
   ("C-c g d" . magit-ediff-show-working-tree)
   ("C-c g s" . magit-status)
   ("C-c g r" . magit-rebase)
   ("C-c g t" . magit-tag))
  :config
  (add-hook 'magit-mode-hook 'magit-load-config-extensions)
  (add-hook 'after-save-hook 'magit-after-save-refresh-status)
  (set-default 'magit-stage-all-confirm nil)
  (setq magit-save-repository-buffers 'dontask
        magit-refs-show-commit-count 'all
        ;; Use separate buffers for one-file logs to prevent
        ;; having to reset the filter in the full log view
        magit-log-buffer-file-locked t
        magit-revision-show-gravatars nil))

(use-package git-commit                 ; Git commit message mode
  :ensure t
  :defer t
  :config
  ;; Oh, really?  Come on… I know what I'm doing…
  (remove-hook 'git-commit-finish-query-functions
               #'git-commit-check-style-conventions))

(use-package gitattributes-mode         ; Git attributes mode
  :ensure t
  :defer t)

(use-package git-timemachine            ; Go back in Git time
  :ensure t
  :bind
  (("C-c g m" . git-timemachine)))

;; gitconfig mode
(use-package gitconfig-mode
  :ensure t)

;; gitignore mode
(use-package gitignore-mode
  :ensure t)

;(use-package gh                         ; Github API library
  ;:defer t
  ;;; Change the default profile.  The profile itself is set up via customize,
  ;;; and includes auth data, to prevent it from storing tokens in Git config
  ;:config (setq gh-profile-default-profile "jseba"))

;; (use-package gist                       ; Create and list Gists
;;   :ensure t
;;   :bind
;;   (("C-c g g l" . gist-list)
;;    ("C-c g g b" . gist-region-or-buffer)))

;; (use-package magit-gh-pulls             ; Show Github PRs in Magit
;;   :ensure t
;;   :defer t
;;   :init
;;   (add-hook 'magit-mode-hook #'turn-on-magit-gh-pulls))

;; (use-package github-clone               ; Clone and fork from Github
;;   :ensure t
;;   :bind
;;   ("C-c g g c" . github-clone))

(use-package smartparens
  :ensure t
  :bind
  (("C-c k" . hydras/smartparens/body)
    :map smartparens-strict-mode-map
    ("M-q" . sp-indent-defun))
  :init
  ;; Hydra for Smartparens
  (defhydra hydras/smartparens (:hint nil)
    "
Sexps (quit with _q_)
^Nav^            ^Barf/Slurp^                 ^Depth^
^---^------------^----------^-----------------^-----^-----------------
_f_: forward     _→_:          slurp forward   _R_: splice
_b_: backward    _←_:          barf forward    _r_: raise
_u_: backward ↑  _C-<right>_:  slurp backward  _↑_: raise backward
_d_: forward ↓   _C-<left>_:   barf backward   _↓_: raise forward
_p_: backward ↓
_n_: forward ↑
^Kill^           ^Misc^                       ^Wrap^
^----^-----------^----^-----------------------^----^------------------
_w_: copy        _j_: join                    _(_: wrap with ( )
_k_: kill        _s_: split                   _{_: wrap with { }
^^               _t_: transpose               _'_: wrap with ' '
^^               _c_: convolute               _\"_: wrap with \" \"
^^               _i_: indent defun"
    ("q" nil)
    ;; Wrapping
    ("(" (lambda (_) (interactive "P") (sp-wrap-with-pair "(")))
    ("{" (lambda (_) (interactive "P") (sp-wrap-with-pair "{")))
    ("'" (lambda (_) (interactive "P") (sp-wrap-with-pair "'")))
    ("\"" (lambda (_) (interactive "P") (sp-wrap-with-pair "\"")))
    ;; Navigation
    ("f" sp-forward-sexp )
    ("b" sp-backward-sexp)
    ("u" sp-backward-up-sexp)
    ("d" sp-down-sexp)
    ("p" sp-backward-down-sexp)
    ("n" sp-up-sexp)
    ;; Kill/copy
    ("w" sp-copy-sexp)
    ("k" sp-kill-sexp)
    ;; Misc
    ("t" sp-transpose-sexp)
    ("j" sp-join-sexp)
    ("s" sp-split-sexp)
    ("c" sp-convolute-sexp)
    ("i" sp-indent-defun)
    ;; Depth changing
    ("R" sp-splice-sexp)
    ("r" sp-splice-sexp-killing-around)
    ("<up>" sp-splice-sexp-killing-backward)
    ("<down>" sp-splice-sexp-killing-forward)
    ;; Barfing/slurping
    ("<right>" sp-forward-slurp-sexp)
    ("<left>" sp-forward-barf-sexp)
    ("C-<left>" sp-backward-barf-sexp)
    ("C-<right>" sp-backward-slurp-sexp))

  (smartparens-global-mode)
  (show-smartparens-global-mode)

  (dolist (hook '(inferior-emacs-lisp-mode-hook
                  emacs-lisp-mode-hook))
    (add-hook hook #'smartparens-strict-mode))
  :config
  (require 'smartparens-config)
  (setq sp-autoskip-closing-pair 'always
        ;; Don't kill entire symbol on C-k
        sp-hybrid-kill-entire-symbol nil)
  :diminish smartparens-mode)

(use-package highlight-numbers
  :ensure t
  :init
  (add-hook 'prog-mode-hook #'highlight-numbers-mode))

;; Highlight delimiters by depth
(use-package rainbow-delimiters
  :ensure t
  :defer t
  :init
  (dolist (hook '(text-mode-hook prog-mode-hook))
    (add-hook hook #'rainbow-delimiters-mode)))

;; Highlight symbols
;(use-package highlight-symbol
  ;:ensure t
  ;:defer t
  ;:bind
  ;(("C-c s %" . highlight-symbol-query-replace)
   ;("C-c s n" . highlight-symbol-next-in-defun)
   ;("C-c s p" . highlight-symbol-prev-in-defun))
  ;;; Navigate occurrences of the symbol under point with M-n and M-p, and
  ;;; highlight symbol occurrences
  ;:init
  ;(dolist (fn '(highlight-symbol-nav-mode highlight-symbol-mode))
    ;(add-hook 'prog-mode-hook fn))
  ;:config
  ;(setq highlight-symbol-idle-delay 0.4     ; Highlight almost immediately
        ;highlight-symbol-on-navigation-p t) ; Highlight immediately after navigation
  ;:diminish highlight-symbol-mode)

;; Highlight TODOs in buffers
(use-package hl-todo
  :ensure t
  :defer t
  :init
  (global-hl-todo-mode))

; Wrap semantic units with pairs
(use-package embrace
  :ensure t
  :bind
  (("C-c y" . lunaryorn-embrace/body)
   ("C-c x e" . lunaryorn-embrace/body))
  :init
  (defhydra lunaryorn-embrace (:hint nil)
    "
Add (_a_), change (_c_) or delete (_d_) a pair.  Quit with _q_.
"
    ("a" embrace-add)
    ("c" embrace-change)
    ("d" embrace-delete)
    ("q" nil)))

;; Highlight current-line
(use-package hl-line
  ; built-in
  :init
  (global-hl-line-mode 1))

;; Regexp highlights
;(use-package hi-lock
  ;:init
  ;(global-hi-lock-mode))

;; Window movement
(bind-key "C-c w =" #'balance-windows)
(bind-key "C-c w k" #'delete-window)
(bind-key "C-c w /" #'split-window-right)
(bind-key "C-c w -" #'split-window-below)
(bind-key "C-c w m" #'delete-other-windows)

;; Window utilities from lunaryorn
(use-package lunaryorn-window
  :defer t
  :bind
  (("C-c w q" . lunaryorn/quit-all-side-windows)
   ("C-c w d" . lunaryorn/toggle-current-window-dedication)
   ("C-c w b" . lunaryorn/switch-to-mini-buffer-window)))

;; windmove
(use-package windmove
  :bind
  (("C-c w <left>"  . windmove-left)
   ("C-c w <right>" . windmove-right)
   ("C-c w <up>"    . windmove-up)
   ("C-c w <down>"  . windmove-down)))

;; Fast window switching
(use-package ace-window
  :ensure t
  :bind
  (("C-x o" . ace-window) ;; replace the default as well
   ("C-c w w" . ace-window)))

;; Auto resize windows
;(use-package golden-ratio
  ;:ensure t
  ;:init
  ;(defun personal/toggle-golden-ratio ()
    ;(interactive)
    ;(if (bound-and-true-p golden-ratio-mode)
      ;(progn
        ;(golden-ratio-mode -1)
        ;(balance-windows))
      ;(golden-ratio-mode)
      ;(golden-ratio)))
  ;:bind
  ;(("C-c t g" . personal/toggle-golden-ratio))
  ;:config
  ;(setq golden-ratio-extra-commands '(windmove-up
                                      ;windmove-down
                                      ;windmove-left
                                      ;windmove-right
                                      ;ace-window
                                      ;ace-delete-window
                                      ;ace-select-window
                                      ;ace-swap-window
                                      ;ace-maximize-window)
        ;golden-ratio-exclude-modes '(flycheck-error-list
                                      ;calc-mode
                                      ;dired-mode
                                      ;ediff-mode)
        ;golden-ratio-exclude-buffer-regexp '(,(rx bos "*" (any "h" "H") "elm*" eos)
                                             ;,(rx bos "*which-key*" eos)
                                             ;,(rx box "*NeoTree*" eos)))
  ;:diminish golden-ratio-mode)

;; ediff windows
(use-package ediff-wind
  :defer t
  :config
  (setq ediff-window-setup-function #'ediff-setup-windows-plain
        ediff-split-window-function #'split-window-horizontally))

;; Auto save buffers/windows/frames
(use-package desktop
  :ensure t
  :disabled nil
  :init
  (desktop-save-mode)
  :config
  (setq desktop-auto-save-timeout 60)
  (dolist (mode '(magit-mode magit-log-mode))
    (add-to-list 'desktop-modes-not-to-save mode))
  (add-to-list 'desktop-files-not-to-save (rx bos "COMMIT_EDITMSG")))

;; Keymappings
(global-set-key [remap move-beginning-of-line]
        'smarter-move-beginning-of-line)
(global-set-key (kbd "C-k") 'close-and-kill-this-pane)
(define-key prog-mode-map (kbd "C-c c") 'comment-region)
(define-key prog-mode-map (kbd "C-c u") 'uncomment-region)

;;; Net & Web
(use-package browse-url                 ; Browse URLs
  :bind
  (("C-c a u" . browse-url)))

;(use-package bug-reference              ; Turn bug refs into browsable buttons
  ;:defer t
  ;:init
  ;(add-hook 'prog-mode-hook #'bug-reference-prog-mode)
  ;(add-hook 'text-mode-hook #'bug-reference-mode))

(use-package goto-addr                  ; Make links clickable
  :defer t
  :bind
  (("C-c t a" . goto-address-mode)
   ("C-c t A" . goto-address-prog-mode))
  :init
  (add-hook 'prog-mode-hook #'goto-address-prog-mode)
  (add-hook 'text-mode-hook #'goto-address-mode))

;(use-package eww                        ; Emacs' built-in web browser
  ;:bind
  ;(("C-c a w b" . eww-list-bookmarks)
   ;("C-c a w w" . eww)
   ;("C-c a w u" . eww-browse-url)))

;(use-package sx                         ; StackExchange client for Emacs
  ;:ensure t
  ;:bind
  ;(("C-c a S a" . sx-ask)
   ;("C-c a S s" . sx-tab-all-questions)
   ;("C-c a S q" . sx-tab-all-questions)
   ;("C-c a S f" . sx-tab-all-questions)
   ;("C-c a S n" . sx-tab-newest)))

;(use-package sx-compose                 ; Write questions/answers for Stack Exchange
   ;:ensure sx
   ;:defer t
   ;:config
   ;;; Don't fill in SX questions/answers, and use visual lines instead.  Plays
   ;;; more nicely with the website.
   ;(add-hook 'sx-compose-mode-hook #'turn-off-auto-fill)
   ;(add-hook 'sx-compose-mode-hook #'visual-line-mode)

   ;;; Clean up whitespace before sending questions
   ;(add-hook 'sx-compose-before-send-hook
     ;(lambda () (whitespace-cleanup) t))

   ;(bind-key "M-q" #'ignore sx-compose-mode-map))

;(use-package sx-question-mode           ; Show Stack
  ;:ensure sx
  ;:defer t
  ;;; Display questions in the same window
  ;:config (setq sx-question-mode-display-buffer-function #'switch-to-buffer))

;; Backups
(defvar backup-directory "~/.emacs.d/backups/")
(unless (file-exists-p backup-directory)
  (make-directory backup-directory t))
(setq savehist-additional-variables '(search ring regexg-search-ring)
      savehist-autosave-interval 60
      make-backup-files t
      backup-directory-alist `(("." . ,backup-directory))
      backup-by-copying t
      version-control t
      delete-old-versions t
      delete-by-moving-to-trash t
      kept-old-versions 6
      kept-new-versions 9
      auto-save-default t
      auto-save-timeout 20
      auto-save-interval 200)
      ;auto-save-file-name-transforms '((".*" ,temporary-file-directory t)))
(winner-mode 1)

;; Modeline
(column-number-mode 1)
(use-package spaceline-config
  :ensure spaceline
  :config
  (spaceline-helm-mode)
  (spaceline-compile
    'jseba
    ;; Left side
    '(((buffer-modified buffer-size input-method) :face highlight-face)
    anzu
    '(buffer-id remote-host buffer-encoding-abbrev)
    ((point-position line-column buffer-position selection-info)
     :separator " | ")
    major-mode
    process
    (flycheck-error flycheck-warning flycheck-info)
    ((which-function projectile-root) : separator " @ ")
    ((minor-modes :separator spaceline-minor-modes-separator) :when active))
    ;; Right side
    '((version-control :when active)))
  (setq spaceline-highlight-face-func #'spaceline-highlight-face-evil-state)
  (setq-default mode-line-format '("%e" (:eval (spaceline-ml-jseba)))))

(use-package powerline
  :ensure t
  :after spaceline-config
  :config
  (setq powerline-height (truncate (* 1.0 (frame-char-height)))
        powerline-default-separator 'utf-8))


;;; Setup programming modes
(add-hook 'prog-mode-hook '(lambda ()
               (interactive)
               (setq show-trailing-whitespace 1)))
(define-key prog-mode-map (kbd "C-c w") 'whitespace-mode)

;;; Diff mode
(add-hook 'diff-mode-hook (lambda ()
              (setq-local whitespace-style
                    '(face
                    tabs
                    tab-mark
                    spaces
                    space-mark
                    trailing
                    indentation::space
                    indentation::tab
                    newline
                    newline-mark))
              (whitespace-mode 1)))

;;; Dired settings
(use-package stripe-buffer
  :ensure t
  :init
  (add-hook 'dired-mode-hook #'stripe-buffer-mode))

(setq dired-dwim-target t
      dired-recursive-copies 'always   ; don't ask for copies
      dired-recursive-deletes 'top     ; only ask for the top directory
      dired-listing-switches "-lha")
(add-hook 'dired-mode-hook 'auto-revert-mode)
(add-hook 'dired-mode-hook 'helm-gtags-mode)

;;; GDB settings
(setq gdb-many-windows t
      gdb-show-main t)

;; Package manager
(use-package paradox
  :ensure t
  :bind
  (("C-c a p" . paradox-list-packages)
   ("C-c a P" . paradox-list-packages-no-fetch))
  :config
  (setq paradox-execute-asynchronously nil ; don't update async
        paradox-spinner-type 'moon         ; fancy!!
        paradox-display-download-count t
        paradox-display-star-count t
        paradox-automatically-star nil
        paradox-use-homepage-buttons nil   ; type `v` instead
        paradox-hide-wiki-packages t))

;;; Processes and commands
(use-package proced                     ; Edit system processes
  ;; Proced isn't available on OS X
  :if (not (eq system-type 'darwin))
  :bind ("C-x p" . proced))

;;; Date and time
(use-package calendar                   ; Built-in calendar
  :bind
  ("C-c a c" . calendar)
  :config
  (setq calendar-week-start-day 0))

(use-package time                       ; Show current time
   :bind
   (("C-c a c" . display-time-world))
   :config
   (setq display-time-world-time-format "%H:%M %Z, %d. %b"
         display-time-world-list '(("America/Chicago"  "Chicago (USA)")
                                   ("Europe/London"    "London")
                                   ("America/New_York" "New York (USA)")
                                   )))

;; Set the theme
(use-package molokai-theme
  :ensure t
  :init
  (load-theme 'molokai t))

(provide 'init)
;;; init.el ends here

;;; init.el -- JSeba's Emacs -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(let ((minver "25.0"))
  (when (version< emacs-version minver)
    (error "Current Emacs is too old -- this config requires v%s or higher" minver)))

;; TODO: use more of this
(defvar nx-debug-mode (or (getenv "DEBUG") init-file-debug)
  "If non-nil, make functions more verbose.")

;; Directories
(defvar nx-emacs-dir (expand-file-name user-emacs-directory)
  "The path to the .emacs.d directory.")
(defvar nx-lisp-dir (concat nx-emacs-dir "lisp/")
  "The path to the configuration files.")
(defvar nx-etc-dir (concat nx-emacs-dir "etc/")
  "Path to non-volatile storage.")
(defvar nx-cache-dir (concat nx-emacs-dir "cache/")
  "Path to volatile storage.")
(defvar nx-package-dir (concat nx-emacs-dir "elpa/")
  "Path to installed packages.")

;; Better hooks
(defvar nx-init-hook nil
  "A list of hooks to run when Emacs is initialized.")
(defvar nx-post-init-hook nil
  "A list of hooks to run after Emacs is initialized (after `nx-init-hook').")

(defvar nx-init-p nil
  "If non-nil, Emacs has been fully initialized.")

(defvar nx-custom-file (concat nx-etc-dir "custom.el")
  "The path to the custom.el file.")
(setq custom-file nx-custom-file)
(load custom-file t t)

(defconst nx--no-ssl-p (and (memq system-type '(windows-nt ms-dos))
                            (not (gnutls-available-p)))
  "Whether or not SSL/TLS is available.")

(add-to-list 'load-path nx-lisp-dir)

;; Increase memory limits during init
(defvar nx--file-name-handler-alist file-name-handler-alist)
(setq gc-cons-threshold (* 384 1024 1024)
      gc-cons-percentage 0.6
      file-name-handler-alist nil
      byte-compile-warnings '(not free-vars unresolved noruntime lexical make-local))

;; Make startup more quiet
(setq inhibit-startup-screen t
      inhibit-startup-message t
      inhibit-startup-echo-area-message user-login-name
      inhibit-default-init t
      initial-major-mode 'fundamental-mode
      initial-scratch-message nil
      mode-line-format nil
      visible-bell nil)
(advice-add #'display-startup-echo-area-message :override #'ignore)

;; Remove unwanted UI elements early
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(when (fboundp 'menu-bar-mode)   (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode)   (tool-bar-mode -1))
(when (fboundp 'horizontal-scroll-bar-mode) (horizontal-scroll-bar-mode -1))

;; Basic quality of life improvement
(fset #'yes-or-no-p #'y-or-n-p)

;; Set some defaults
(setq-default ad-redefinition-action 'accept
              apropos-do-all t
              compilation-always-kill t
              compilation-ask-about-save nil
              compilation-scroll-output t
              confirm-non-existent-file-or-buffer t
              enable-recursive-minibuffers nil
              debug-on-error nx-debug-mode
              history-length 500

              abbrev-file-name (concat nx-etc-dir "abbrev.el")
              pcache-directory (concat nx-cache-dir "pcache/")
              mc/list-file (concat nx-etc-dir "mc-lists.el")
              server-auth-dir (concat nx-cache-dir "server/"))

(defun nx--try-run-hook (fn hook)
  "Run FN from HOOK wrapped in a `condition-case-unless-debug' block.

This provides more information in the error message and still provides
for invoking the debugger in debug mode."
  (condition-case-unless-debug ex
      (funcall fn)
    ('error
     (lwarn hook :error
	        "%s in '%s' -> %s"
	        (car ex) fn (error-message-string ex))))
  nil)

(defun nx|finalize ()
  "Finalize initialization."
  (unless nx-init-p
    (dolist (hook '(nx-init-hook nx-post-init-hook))
      (run-hook-wrapped hook #'nx--try-run-hook hook))
    (setq nx-init-p t))
  (setq gc-cons-threshold (* 16 1024 1024)
	    gc-cons-percentage 0.1
        file-name-handler-alist nx--file-name-handler-alist)
  (message "init completed in %.2fms"
	       (* 1000.0 (float-time (time-subtract after-init-time before-init-time)))))
(add-hook 'emacs-startup-hook #'nx|finalize)

;; Setup package.el
(eval-and-compile
  (setq load-prefer-newer t
        package-user-dir (concat nx-package-dir emacs-version "/")
        package-enable-at-startup nil))

(require 'package)
(add-to-list 'package-archives `("melpa" . ,(if nx--no-ssl-p
					        "http://melpa.org/packages/"
					      "https://melpa.org/packages/")))
(unless nx--no-ssl-p
  (setcdr (assoc "gnu" package-archives) "https://elpa.gnu.org/packages/"))

(package-initialize)
(unless (file-directory-p package-user-dir)
  (make-directory package-user-dir t)
  (package-refresh-contents))
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (let ((inhibit-message t))
    (package-install 'use-package)))
(require 'use-package)
(setq use-package-always-ensure nil
      use-package-always-defer t
      use-package-debug nil
      use-package-verbose nx-debug-mode
      use-package-minimum-reported-time (if nx-debug-mode 0 0.1))

(defun recompile-packages ()
  "Recompile all packages."
  (interactive)
  (byte-recompile-directory package-user-dir nil 'force))

;;;
;;  BEGIN USE-PACKAGE
;;;

(use-package dash
  :demand t
  :ensure t)

(use-package persistent-soft
  :demand t
  :ensure t)

(use-package diminish
  :demand t
  :ensure t)

(use-package bind-key
  :demand t
  :ensure t)

;;; Benchmark Startup
(defvar sanityinc/require-times nil
  "A list of (FEATURE LOAD-START-TIME LOAD-DURATION).

LOAD-DURATION is the time taken (in milliseconds) to load FEATURE.")

(defun sanityinc-time-subtract-millis (rhs lhs)
  "Subtract LHS from RHS and convert to milliseconds."
  (* 1000.0 (float-time (time-subtract rhs lhs))))

(defadvice require (around sanityinc/build-require-times (feature &optional filename noerror) activate)
  "Note in `sanityinc/require-times' the time take to require each FEATURE."
  (let* ((already-loaded (memq feature features))
	 (require-start-time (and (not already-loaded) (current-time))))
    (prog1
	ad-do-it
      (when (and (not already-loaded) (memq feature features))
	(let ((time (sanityinc-time-subtract-millis (current-time) require-start-time)))
	  (add-to-list 'sanityinc/require-times
		       (list feature require-start-time time)
		       t))))))

(define-derived-mode sanityinc/require-times-mode tabulated-list-mode "Require-Times"
  "Show times taken to `require' packages."
  (setq tabulated-list-format
	[("Start time (ms)" 20 sanityinc/require-times-sort-by-start-time-pred)
	 ("Feature" 30 t)
	 ("Time (ms)" 12 sanityinc/require-times-sort-by-load-time-pred)]
	tabulated-list-sort-key (cons "Start time (ms)" nil)
	tabulated-list-entries #'sanityinc/require-times-tabulated-list-entries)
  (tabulated-list-init-header)
  (tablist-minor-mode))

(defun sanityinc/require-times-sort-by-start-time-pred (entry1 entry2)
  "Predicate to sort `require-times' by `start-time'."
  (< (string-to-number (elt (nth 1 entry1) 0))
     (string-to-number (elt (nth 1 entry2) 0))))

(defun sanityinc/require-times-sort-by-load-time-pred (entry1 entry2)
  "Predicate to sort `require-times' by `start-time'."
  (> (string-to-number (elt (nth 1 entry1) 2))
     (string-to-number (elt (nth 1 entry2) 2))))

(defun sanityinc/require-times-tabulated-list-entries ()
  "Convert `require-times' to a tabulated list."
  (cl-loop for (feature start-time millis) in sanityinc/require-times
	   with order = 0
	   do (incf order)
	   collect (list order
			 (vector
			  (format "%.3f" (sanityinc-time-subtract-millis start-time before-init-time))
			  (symbol-name feature)
			  (format "%.3f" millis)))))

(defun sanityinc/require-times ()
  "Show a tabular view of how long various libraries took to load."
  (interactive)
  (with-current-buffer (get-buffer-create "*Require Times*")
    (sanityinc/require-times-mode)
    (tabulated-list-revert)
    (display-buffer (current-buffer))))

;;; Packages
(use-package tablist
  :ensure t)

;;; Configure editing prefs
(setq-default auto-save-list-file-name (concat nx-cache-dir "autosave")
              auto-save-default nil
              bookmark-default-file (concat nx-etc-dir "bookmarks")
              bookmark-save-flag t
              create-lockfiles nil
              make-backup-files nil
              ring-bell-function #'ignore
              save-interprogram-paste-before-kill t
              tooltip-delay 1.5
              buffers-menu-max-size 30
              case-fold-search t
              column-number-mode t
              delete-selection-mode t
              delete-trailing-lines nil
              fill-column 140
              indicate-empty-lines t
              sentence-end-double-space nil
              word-wrap t
              hscroll-margin 1
              hscroll-step 1
              scroll-conservatively 1001
              scroll-margin 0
              scroll-preserve-screen-position t
              mouse-wheel-scroll-amount '(1)
              mouse-wheel-progressive-speed nil
              mouse-yank-at-point t
              indent-tabs-mode nil
              require-final-newline nil
              tab-always-indent 'complete
              tab-width 4
              tabify-regex "^\t [ \t]+"
              truncate-lines t
              truncate-partial-width-windows 50
              show-trailing-whitespace nil
              whitespace-line-column fill-column
              whitespace-style '(face indentation tab-mark spaces space-mark
                                      newline newline-mark trailing lines-tail)
              whitespace-display-mappings '((tab-mark ?\t [?� ?\t])
                                            (newline-mark ?\n [?� ?\n])
                                            (space-mark ?\  [?�] [?.])))

;; Bring back region casing
(put 'upcase-region   'disabled nil)
(put 'downcase-region 'disabled nil)

;; Bring back narrowing
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page   'disabled nil)
(put 'narrow-to-defun  'disabled nil)

;; Set system encoding to UTF-8 everywhere
(when (fboundp 'set-charset-priority)
  (set-charset-priority 'unicode))
(prefer-coding-system        'utf-8)
(set-terminal-coding-system  'utf-8)
(set-keyboard-coding-system  'utf-8)
(set-selection-coding-system 'utf-8)
(setq locale-coding-system   'utf-8)
(setq-default buffer-file-coding-system 'utf-8)

;;; Define variables
(defvar nx-large-file-size 1
  "Size (in MB) above which we will prompt to open a file literally.")
(defvar nx-large-file-modes-list '(archive-mode tar-mode jka-compr
                                                doc-view-mode doc-view-mode-maybe
                                                image-mode git-commit-mode pdf-view-mode
                                                ebrowse-tree-mode)
  "Major modes that `nx|check-large-file' will ignore.")
(defvar nx-auto-minor-mode-alist '()
  "Alist of filename patterns to corresponding minor mode functions.")

;;; Functions
(defun nx|dont-kill-scratch-buffer ()
  "Don't kill the scratch buffer."
  (or (not (string= (buffer-file-name) "*scratch*"))
      (ignore (bury-buffer))))

(defun nx|check-large-file ()
  "Check if the buffer's file is larger than `nx-large-file-size'."
  (let* ((filename (buffer-file-name))
         (size (nth 7 (file-attributes filename))))
    (when (and (not (memq major-mode nx-large-file-modes-list))
               size
               (> size (* 1024 1024 nx-large-file-size))
               (y-or-n-p (concat "%s is a large file, open literally?"
                                 (file-relative-name filename))))
      (setq buffer-read-only t)
      (buffer-disable-undo)
      (fundamental-mode))))

(defun nx|enable-minor-mode-maybe ()
  "Check file name against `nx-auto-minor-mode-alist'."
  (when buffer-file-name
    (let ((name buffer-file-name)
          (remote-id (file-remote-p buffer-file-name))
          (alist nx-auto-minor-mode-alist))
      (setq name (file-name-sans-versions name))
      (when (and (stringp remote-id)
                 (string-match-p (regexp-quote remote-id) name))
        (setq name (substring name (match-end 0))))
      (while (and alist (caar alist) (cdar alist))
        (if (string-match-p (caar alist) name)
            (funcall (cdar alist) 1)
          (setq alist (cdr alist)))))))

(defun nx*set-indirect-buffer-filename (orig-fn base-buffer name &optional clone)
  "Set `buffer-file-name' in indirect buffers.

In indirect buffers, `buffer-file-name' is nil, which can cause problems
with functions that require it (like modeline segments)."
  (let ((file-name (buffer-file-name base-buffer))
        (buffer (funcall orig-fn base-buffer name clone)))
    (when (and file-name buffer)
      (with-current-buffer buffer
        (unless buffer-file-name
          (setq buffer-file-name file-name
                buffer-file-truename (file-truename file-name)))))
    buffer))

(defun nx*delete-trailing-whitespace (orig-fn &rest args)
  "Don't affect trailing whitespace on current line."
  (let ((linestr (buffer-substring-no-properties
                  (line-beginning-position)
                  (line-end-position))))
    (apply orig-fn args)
    (when (string-match-p "^[\s\t]*$" linestr)
      (insert linestr))))

(defun nx/smart-newline-and-indent ()
  "Insert a newline and possibly indent it.

Also continues comments if executed from a commented line and handles
special cases for certain languages with weak native support."
  (interactive)
  (cond ((sp-point-in-string)
         (newline))
        ((sp-point-in-comment)
         (pcase major-mode
           ((or 'js2-mode 'rjsx-mode)
            (call-interactively #'js2-line-break))
           ((or 'java-mode 'php-mode)
            (c-indent-new-comment-line))
           ((or 'c-mode 'c++-mode 'objc-mode 'css-mode 'scss-mode 'js2-mode)
            (newline-and-indent)
            (insert "* ")
            (indent-according-to-mode))
           (_
            ;; Fix an off-by-one cursor-positioning issue
            ;; with `indent-new-comment-line'
            (let ((col (save-excursion (comment-beginning) (current-column))))
              (indent-new-comment-line)
              (unless (= col (current-column))
                (insert " "))))))
        (t
         (newline nil t)
         (indent-according-to-mode))))

(defun nx/newline-at-end-of-line ()
  "Move to end of line, enter a newline and reindent."
  (interactive)
  (move-end-of-line 1)
  (nx/smart-newline-and-indent))

(defun nx/smart-kill-whole-line (&optional arg)
  "Kill whole line and move back to indentation."
  (interactive "p")
  (kill-whole-line arg)
  (back-to-indentation))

(defun nx/smart-move-beginning-of-line ()
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between first non-whitespace character and
the beginning of the line."
  (interactive)
  (if (bound-and-true-p visual-line-mode)
      (beginning-of-visual-line)
    (let ((ci (current-indentation))
          (cc (current-column)))
      (cond ((or (> cc ci) (= cc 0))
             (back-to-indentation))
            ((<= cc ci)
             (beginning-of-visual-line))))))

(defun nx/smart-move-end-of-line ()
  "Move forward to the last non-blank character in the line, ignoring comments
and trailing whitespace. If already there, move to the real end of the line.
If already there, do nothing."
  (interactive)
  (let* ((point (point))
         (eol (save-excursion (end-of-visual-line) (point)))
         (bol (save-excursion (beginning-of-visual-line) (point)))
         (eoc (or (if (not comment-use-syntax)
                      (when (re-search-forward comment-start-skip eol t)
                        (or (match-end 1) (match-beginning 0)))
                    (save-excursion
                      (goto-char eol)
                      (while (and (sp-point-in-comment)
                                  (> (point) point))
                        (backward-char))
                      (when (> (point) point)
                        (skip-chars-backward " " bol)
                        (point))))
                  eol)))
    (if (= eoc point)
	    (goto-char eol)
      (unless (= eol point)
	    (goto-char eoc)))))

(defun nx/smart-back-to-indentation (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line. If point is
already there, move to the beginning of the line. Effectively toggle between
the first non-whitespace character and the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first. If point reaches the
beginning or end of the buffer, stop there."
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

(defun nx/kill-back-to-indentation ()
  "Kill from point back to the first non-whitespace character on the current line."
  (interactive)
  (let ((prev-pos (point)))
    (back-to-indentation)
    (kill-region (point) prev-pos)))

(defun nx/smart-backward-up-sexp (arg)
  "Jump up to the start of the ARG'th enclosing sexp."
  (interactive "p")
  (let ((ppss (syntax-ppss)))
    (cond
     ((elt ppss 3)
      (goto-char (elt ppss 8))
      (backward-up-sexp (1- arg)))
     (backward-up-list arg))))

(defun nx--surrounded-p ()
  "Return non-nil if point is surrounded."
  (and (looking-back "[[{(]\\(\s+\\|\n\\)?\\(\s\\|\t\\)*" (line-beginning-position))
       (let* ((whitespace (match-string 1))
              (match-str (concat whitespace (match-string 2) "[])}]")))
         (looking-at-p match-str))))

(defun nx/dumb-indent ()
  "Insert a tab character (or spaces x `tab-width')."
  (interactive)
  (if indent-tabs-mode
      (insert "\t")
    (let* ((movement (% (current-column) tab-width))
           (spaces (if (= 0 movement) tab-width (- tab-width movement))))
      (insert (make-string spaces ? )))))

(defun nx/dumb-dedent ()
  "Dedents the current line."
  (interactive)
  (if indent-tabs-mode
      (call-interactively #'backward-delete-char)
    (unless (bolp)
      (save-excursion
        (when (> (current-column) (current-indentation))
          (back-to-indentation))
        (let ((movement (% (current-column) tab-width)))
          (delete-char
           (- (if (= 0 movement)
                  tab-width
                (- tab-width movement)))))))))

(defun nx/smart-backward-kill-line ()
  "Kill line to the first non-blank character.

If invoked again afterwards, kill line to column 1."
  (interactive)
  (let ((empty-line-p (save-excursion (beginning-of-line)
                                      (looking-at-p "[ \t]*$"))))
    (delete-region (point-at-bol) (point))
    (unless empty-line-p
      (indent-according-to-mode))))

(defun nx/backward-delete-whitespace-to-column ()
  "Delete back to the previous column of whitespace.

Fallback to either as much whitespace as possible or just one char."
  (interactive)
  (let* ((delete-backward-char (if (derived-mode-p 'org-mode)
                                   #'org-delete-backward-char
                                 #'delete-backward-char))
         (context (sp--get-pair-list-context 'navigate))
         (open-pair-re (sp--get-opening-regexp context))
         (close-pair-re (sp--get-closing-regexp context))
         open-len close-len)
    (cond ;; When in strings (sp acts weird with quotes; this is the fix)
     ;; Also, skip closing delimiters
     ((and (and (sp--looking-back open-pair-re)
		        (setq open-len (- (match-beginning 0) (match-end 0))))
	       (and (looking-at close-pair-re)
		        (setq close-len (- (match-beginning 0) (match-end 0))))
	       (string= (plist-get (sp-get-thing t) :op)
		            (plist-get (sp-get-thing) :cl)))
      (delete-char (- 0 open-len))
      (delete-char close-len))

     ;; Delete up to the nearest tab column IF only whitespace between
     ;; point and bol.
     ((save-match-data (looking-back "^[\\t ]*" (line-beginning-position)))
      (let ((movement (% (current-column) tab-width))
	        (p (point)))
	    (when (= movement 0)
	      (setq movement tab-width))
	    (save-match-data
	      (if (string-match "\\w*\\(\\s-+\\)$"
			                (buffer-substring-no-properties (max (point-min) (- p movement)) p))
	          (sp-delete-char
	           (- 0 (- (match-end 1)
		               (match-beginning 1))))
	        (call-interactively delete-backward-char)))))

     ;; Otherwise do a regular delete
     (t (call-interactively delete-backward-char)))))

(defun nx/inflate-space-maybe ()
  "Add a space if point is surrounded by {} [] () delimiters."
  (interactive)
  (let ((command (or (command-remapping #'self-insert-command)
                     #'self-insert-command)))
    (cond ((nx--surrounded-p)
           (call-interactively command)
           (save-excursion (call-interactively command)))
          (t
           (call-interactively command)))))

(defun nx/deflate-space-maybe ()
  "Add a space if point is surrounded by {} [] () delimiters.

Resorts to `nx/backward-delete-whitespace-to-column' otherwise."
  (interactive)
  (save-match-data
    (if (nx--surrounded-p)
        (let ((whitespace-match (match-string 1)))
          (cond ((not whitespace-match)
                 (call-interactively #'delete-backward-char))
                ((string-match "\n" whitespace-match)
		         (delete-region (point-at-bol) (point))
                 (call-interactively #'delete-backward-char)
                 (save-excursion (call-interactively #'delete-char)))
                (t (just-one-space 0))))
      (nx/backward-delete-whitespace-to-column))))

(defun nx/retab (&optional beg end)
  "Change all tabs to spaces or spaces to tabs, depending on `indent-tab-mode'.

BEG and END mark the region to perform on which to perform this operation."
  (interactive "r")
  (unless (and beg end)
    (setq beg (point-min)
          end (point-max)))
  (if indent-tabs-mode
      (tabify beg end)
    (untabify beg end)))

(defun nx|enable-delete-trailing-whitespace ()
  "Attaches `delete-trailing-whitespace' to a buffer-local `before-save-hook'."
  (add-hook 'before-save-hook #'delete-trailing-whitespace nil t))

;;; Add hook/advice
(add-hook 'kill-buffer-query-functions #'nx|dont-kill-scratch-buffer)
(add-hook 'find-file-hook #'nx|check-large-file)
(add-hook 'find-file-hook #'nx|enable-minor-mode-maybe)
(advice-add #'make-indirect-buffer :around #'nx*set-indirect-buffer-filename)
(advice-add #'delete-trailing-whitespace :around #'nx*delete-trailing-whitespace)

;;; Editor packages

;;; Built-ins
(use-package autorevert
  :demand t
  :config
  (setq auto-revert-verbose nil)
  (add-hook 'nx-post-init-hook #'global-auto-revert-mode)
  :diminish auto-revert-mode)

(use-package electric
  :init
  (add-hook 'prog-mode-hook #'electric-indent-mode)
  :diminish electric-indent-mode)

(use-package savehist
  :demand t
  :config
  (setq savehist-file (concat nx-cache-dir "savehist")
        savehist-save-minibuffer-history t
        savehist-autosave-interval nil
        savehist-additional-variables '(kill-ring search-ring regexp-search-ring))
  (add-hook 'nx-post-init-hook #'savehist-mode)
  :diminish savehist-mode)

(use-package saveplace
  :demand t
  :config
  (setq save-place-file (concat nx-cache-dir "saveplace"))
  (add-hook 'nx-post-init-hook #'save-place-mode)
  :diminish save-place-mode)

(use-package recentf
  :demand t
  :config
  (setq recentf-save-file (concat nx-etc-dir "recentf")
        recentf-max-menu-items 0
        recentf-max-saved-items 300
        recentf-exclude (list  "^/tmp/" "^/ssh:" "\\.?ido\\.last$" "\\.revive$" "/TAGS$"
                               "^/var/folders/.+$"))
  (add-hook 'nx-post-init-hook #'recentf-mode)
  :diminish recentf-mode)

(use-package subword
  :demand t
  :config (add-hook 'nx-post-init-hook #'subword-mode)
  :diminish subword-mode)

(use-package align
  :bind
  (("C-c x a a" . align)
   ("C-c x a c" . align-current)))

;;; ELPA
(use-package expand-region
  :ensure t
  :bind
  (("C-=" . er/expand-region)))

(use-package editorconfig
  :ensure t
  :demand t
  :config
  (add-hook 'nx-post-init-hook #'editorconfig-mode)
  :diminish editorconfig-mode)

(use-package smartparens
  :ensure t
  :config
  (require 'smartparens-config)
  (setq sp-autowrap-region t
        sp-cancel-autoskip-on-backward-movement nil
        sp-highlight-pair-overlay nil
        sp-show-pair-delay 0
        sp-max-pair-length 6
        sp-autoskip-closing-pair 'always
        sp-hybrid-kill-entire-symbol nil)
  (add-hook 'nx-init-hook #'smartparens-global-mode)
  :diminish smartparens-mode)

(use-package unfill
  :ensure t)

(use-package undo-tree
  :ensure t
  :demand t
  :config
  (setq undo-tree-auto-save-history nil
        undo-tree-history-directory-alist (list (cons "." (concat nx-cache-dir  "undo-tree-hist/"))))
  (add-hook 'nx-init-hook #'global-undo-tree-mode)
  :diminish undo-tree-mode)

(use-package beacon
  :ensure t
  :init
  (setq-default beacon-lighter ""
                beacon-size 5)
  (add-hook 'nx-after-init-ui-hook #'beacon-mode)
  :diminish beacon-mode)

(use-package nlinum
  :ensure t)

(use-package browse-kill-ring
  :ensure t
  :init
  (setq browse-kill-ring-separator "\f")
  :bind
  (("M-Y" . browse-kill-ring)
   :map browse-kill-ring-mode-map
   ("C-g" . browse-kill-ring-quit)
   ("M-n" . browse-kill-ring-forward)
   ("M-p" . browse-kill-ring-previous)))

(use-package zop-to-char
  :ensure t
  :bind
  (("M-z" . zop-to-char)
   ("M-Z" . zop-up-to-char)))

(use-package avy
  :ensure t
  :init
  (setq avy-background t
	    avy-style 'at-full)
  :bind
  (("C-;" . avy-goto-char-timer)
   ("C-c j j" . avy-goto-word-or-subword-1)
   ("C-c j w" . avy-goto-word-1)
   ("C-c j l" . avy-goto-line)
   ("C-c j m" . avy-pop-mark)
   ("C-c j c" . avy-goto-char-2)))

(use-package which-key
  :ensure t
  :init
  (setq which-key-idle-delay 0.3
	    which-key-sort-order 'which-key-prefix-then-key-order
	    which-key-popup-type 'side-window
	    which-key-popup-window-location 'bottom
	    which-key-side-windows-max-height 0.15)
  ;;       which-key-replacement-alist
  ;;       '(("<\\([[:alnum:]-]+\\)>" . "\\1")
  ;;         ("up"                    . "↑")
  ;;         ("right"                 . "→")
  ;;         ("down"                  . "↓")
  ;;         ("left"                  . "←")
  ;;         ("DEL"                   . "⌫")
  ;;         ("deletechar"            . "⌦")
  ;;         ("RET"                   . "⏎"))
  ;;       which-key-replacement-alist
  ;;       '(("Prefix Command" . "prefix")
  ;;         ;; Lambdas
  ;;         ("\\`\\?\\?\\'"   . "λ")
  ;;         ;; Prettify hydra entry points
  ;;         ("/body\\'"       . "|=")
  ;;         ;; Drop/shorten package prefixes
  ;;         ("\\`lunaryorn/"  . "")
  ;;         ("\\`sanityinc/"  . "")
  ;;         ("\\`doom/"       . "")
  ;;         ("projectile-"    . "proj-")
  ;;         ("magit-"         . "ma-")))
  (which-key-add-key-based-replacements
    ;; Prefixes for global prefixes and minor modes
    "C-c @" "outline"
    "C-c !" "flycheck"
    "C-c 8" "typo"
    "C-c 8 -" "typo/dashes"
    "C-c 8 <" "typo/left-brackets"
    "C-c 8 >" "typo/right-brackets"
    "C-c a" "applications"
    "C-c b" "buffers"
    "C-c c" "compile-and-comments"
    "C-c e" "errors"
    "C-c f" "files"
    "C-c v" "variables"
    "C-c g" "git"
    "C-c h" "helm"
    "C-c i" "insert"
    "C-c j" "jump"
    "C-c k" "smartparens"
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
    "C-c u" "gtags"
    "C-c w" "windows/frames"
    "C-c x" "text")

  (add-hook 'nx-init-hook #'which-key-mode)
  :diminish which-key-mode)

;; UI
;;; Base configuration
(setq-default minibuffer-prompt-properties '(read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt)
              idle-update-delay 2
              bidi-display-reordering nil
              blink-matching-paren nil
              cursor-in-non-selected-windows nil
              display-line-numbers-width 3
              frame-inhibit-implied-resize t)
(setq use-file-dialog nil
      use-dialog-box nil
      frame-title-format '("" invocation-name " - "
			               (:eval (if (buffer-file-name)
                                      (abbreviate-file-name (buffer-file-name "%b"))))))

(let ((no-border '(internal-border-width . 0)))
  (add-to-list 'default-frame-alist no-border)
  (add-to-list 'initial-frame-alist no-border))

;;; Define variables/constants
(defvar nx-after-make-console-frame-hook nil
  "A list of hooks to run after making a console frame.")
(defvar nx-after-make-system-frame-hook nil
  "A list of hooks to run after making a window system frame.")
(defvar nx-theme 'ample
  "Default theme.")
(defvar nx-font (font-spec :family "Source Code Pro" :size 12)
  "The default font. Should be a FONT-SPEC.")
(defvar nx-big-font (font-spec :family "Source Code Pro" :size 14)
  "The default big font. Should be a FONT-SPEC.")
(defvar nx-variable-pitch-font nil
  "The default font to use for variable-pitch text. Should be a FONT-SPEC.")
(defvar nx-unicode-font nil
  "The default font to use for unicode text. Should be a FONT-SPEC.")
(defvar nx-major-mode-names '((emacs-lisp-mode . "Elisp"))
  "An alist mapping major modes symbols to strings or functions that will return a string).")
(defconst nx--initial-frame (selected-frame)
  "The frame (if any) active during Emacs initialization.")

;; Define functions/macros
(defun nx|init-ui (&optional frame)
  "Set the theme and load the font on FRAME (in that order)."
  (when nx-theme
    (load-theme nx-theme t))
  (condition-case-unless-debug ex
      (when (display-graphic-p)
        (when (fontp nx-font)
          (set-frame-font nx-font nil (if frame (list frame) t))
          (set-face-attribute 'fixed-pitch frame :font nx-font))
        (when (fontp nx-unicode-font)
          (set-fontset-font t 'unicode nx-unicode-font frame))
        (when (fontp nx-variable-pitch-font)
          (set-face-attribute 'variable-pitch frame :font nx-variable-pitch-font)))
    ('error
     (lwarn 'nx-ui :error
            "Failed to set fonts because %s"
            (error-message-string ex)))))

(defun nx|run-after-make-frame-hooks (frame)
  "Run configured hooks in response to the newly created FRAME."
  (with-selected-frame frame
    (dolist (hook (if window-system
		              nx-after-make-system-frame-hook
		            nx-after-make-console-frame-hook))
      (run-hook-wrapped hook #'nx--try-run-hook hook))))

(defun nx|run-after-make-initial-frame-hooks ()
  "Run configured hooks on the initial frame."
  (nx|run-after-make-frame-hooks nx--initial-frame))

(defun nx|disable-line-spacing ()
  "Turn off line spacing."
  (setq line-spacing 0))

(autoload 'mwheel-install "mwheel")
(defun nx|console-frame-setup ()
  "Setup a console frame."
  (xterm-mouse-mode 1)
  (mwheel-install))

(defun nx|set-mode-name ()
  "Set the major mode's `mode-name' as seen in `nx-major-mode-names'."
  (-when-let (name (cdr (assq major-mode nx-major-mode-names)))
    (setq mode-name
          (cond ((functionp name) (funcall name))
                ((stringp name) name)
                (t (error "'%s' isn't a valid name for %s" name major-mode))))))

(defun nx|clear-background-term ()
  "Clear terminal background."
  (unless (display-graphic-p (selected-frame))
    (set-face-background 'default "unspecified-bg" (selected-frame))))

(defun nx/toggle-fullscreen ()
  "Toggle fullscreen Emacs."
  (interactive)
  (set-frame-parameter
   nil
   'fullscreen
   (unless (frame-parameter nil 'fullscreen)
     'fullboth)))

(defun nx/toggle-line-numbers (&optional arg)
  "Toggle `linum-mode'.

ARG controls the package used to display line numbers."
  (interactive "P")
  (cond ((boundp 'display-line-numbers)
	     (setq display-line-numbers
	           (pcase arg
		         ('(4) 'relative)
		         (1 t)
		         (-1 nil)
		         (_ (not display-line-numbers)))))
	    ((featurep 'nlinum)
	     (nlinum-mode (or arg (if nlinum-mode -1 +1))))
	    (t
	     (error "No line number plugin detected"))))

(defun nx-resize-window (window new-size &optional horizontal force-p)
  "Resize WINDOW to NEW-SIZE. If HORIZONTAL, do it width-wise.

If FORCE-P is non-nil, do it even if the window is a fixed size."
  (with-selected-window (or window (selected-window))
    (let ((window-size-fixed (unless force-p window-size-fixed)))
      (enlarge-window (- new-size (if horizontal (window-width) (window-height)))
		              horizontal))))

(defun nx/window-zoom ()
  "Maximize and isolate the current buffer. Activate again to undo.

If the window changes before then, the undo expires."
  (interactive)
  (if (and (one-window-p)
	       (assoc ?_ register-alist))
      (jump-to-register ?_)
    (window-configuration-to-register ?_)
    (delete-other-windows)))

(defvar nx--window-enlargened-p nil)
(defun nx/window-enlargen ()
  "Enlargen the current window. Activate again to undo."
  (interactive)
  (setq nx--window-enlargened-p
	    (if (and nx--window-enlargened-p
		         (assoc ?_ register-alist))
	        (ignore (jump-to-register ?_))
	      (window-configuration-to-register ?_)
	      (nx-resize-window nil (truncate (/ (frame-width) 1.2)) t)
	      (nx-resize-window nil (truncate (/ (frame-heigth) 1.2)))
	      t)))

(defun nx/delete-frame ()
  "Delete the current-frame, but confirm if it isn't empty."
  (interactive)
  (if (cdr (frame-alist))
      (when (nx-quit-p "Close frame?")
	    (delete-frame))
    (save-buffers-kill-emacs)))

(defun nx/split-window-horizontally-with-other-buffer (&optional arg)
  "Split this window horizontally and switch to the new window unless ARG is provided."
  (interactive "P")
  (split-window-horizontally)
  (let ((target-window (next-window)))
    (set-window-buffer target-window (other-window 1))
    (unless arg
      (select-window target-window))))

(defun nx/split-window-vertically-with-other-buffer (&optional arg)
  "Split this window vertically and switch to the new window unless ARG is provided."
  (interactive "P")
  (split-window-vertically)
  (let ((target-window (next-window)))
    (set-window-buffer target-window (other-buffer 1))
    (unless arg
      (select-window target-window))))

(defun nx/toggle-delete-other-windows ()
  "Delete other windows in frame (if any) or restore previous window config."
  (interactive)
  (if (and winner-mode
	       (equal (selected-window) (next-window)))
      (winner-undo)
    (delete-other-windows)))

(defun nx/split-window-horizontally-instead ()
  "Change window split to horizontal."
  (interactive)
  (save-excursion
    (delete-other-windows)
    (nx/split-window-horizontally-with-other-buffer)))

(defun nx/split-window-vertically-instead ()
  "Change window split to vertical."
  (interactive)
  (save-excursion
    (delete-other-windows)
    (nx/split-window-vertically-with-other-buffer)))

(defun nx/split-window ()
  "Split the window to see the most recent buffer in the other window.
Call a second time to restore the orignal window configuration."
  (interactive)
  (if (eq last-command 'nx/split-window)
      (progn
	    (jump-to-register :nx/split-window)
	    (setq this-command 'nx/unsplit-window))
    (window-configuration-to-register :nx/split-window)
    (switch-to-buffer-other-window nil)))

(defun nx/toggle-current-window-dedication ()
  "Toggle whether the current window is dedicated to its current buffer."
  (interactive)
  (let* ((window (selected-window))
	     (was-dedicated (window-dedicated-p window)))
    (set-window-dedicated-p window (not was-dedicated))
    (message "Window %sdedicated to %s"
	         (if was-dedicated "no longer " "")
	         (buffer-name))))

(define-minor-mode nx-big-font-mode
  "A global mdoe that resizes the font."
  :init-value nil
  :lighter " BIG"
  :global t
  (unless (fontp nx-big-font)
    (user-error "`nx-big-font is not set to a valid font"))
  (if nx-big-font-mode
      (set-frame-font nx-big-font t t)
    (set-frame-font nx-font t t)))

(defun sanityinc|maybe-adjust-visual-fill-column ()
  "Readjust visual fill column when the global font size is modified."
  (if visual-fill-column-mode
	  (add-hook 'after-setting-font-hook 'visual-fill-column--adjust-window nil t)
    (remove-hook 'after-setting-font-hook 'visual-fill-column--adjust-window t)))

;;; Add hooks/advice
(add-hook 'nx-post-init-hook #'nx|init-ui)
(add-hook 'after-make-frame-functions #'nx|init-ui)
(add-hook 'after-make-frame-functions #'nx|run-after-make-frame-hooks)
(add-hook 'nx-post-init-hook #'nx|run-after-make-initial-frame-hooks)
(add-hook 'nx-after-make-console-frame-hook #'nx|console-frame-setup)
(add-hook 'nx-after-make-console-frame-hook #'nx|clear-background-term)
(add-hook 'term-mode-hook #'nx|disable-line-spacing)
(add-hook 'after-change-major-mode-hook #'nx|set-mode-name)
(add-hook 'visual-fill-column-mode-hook #'sanityinc|maybe-adjust-visual-fill-column)

;;; Packages
;;;; Built-ins
(use-package winner
  :demand t
  :config
  (setq winner-dont-bind-my-keys t)
  (add-hook 'nx-post-init-hook #'winner-mode))

(use-package paren
  :demand t
  :config
  (setq show-paren-delay 0.2
        show-paren-highlight-openparen t
        show-paren-when-point-inside-paren t)
  (add-hook 'nx-post-init-hook #'show-paren-mode))

(use-package frame
  :demand t
  :config
  (setq-default window-divider-default-places t
                window-divider-default-bottom-width 0
                window-divider-default-right-width 1)
  (add-hook 'nx-post-init-hook #'window-divider-mode))

(use-package windmove
  :demand t
  :bind
  (([remap split-window-vertically] . nx/split-window-vertically-with-other-buffer)
   ([remap split-window-horizontally] . nx/split-window-horizontally-with-other-buffer)
   ([remap delete-other-windows] . nx/toggle-delete-other-windows)
   ("C-c w |" . nx/split-window-horizontally-instead)
   ("C-c w _" . nx/split-window-vertically-instead)
   ("C-c w S" . nx/split-window)
   ("C-c w d" . nx/toggle-current-window-dedication)))

(use-package hideshow
  :diminish hs-minor-mode)

(use-package default-text-scale
  :ensure t
  :init
  (add-hook 'nx-post-init-hook #'default-text-scale-mode))

(use-package visual-fill-column
  :ensure t
  :init
  (add-hook 'nx-post-init-hook #'visual-fill-column-mode))

(use-package ample-theme
  :ensure t)

(use-package hl-todo
  :ensure t
  :commands hl-todo-mode
  :init
  (setq hl-todo-keyword-faces `(("TODO"  . ,(face-foreground 'warning))
                                ("FIXME" . ,(face-foreground 'error))
                                ("NOTE"  . ,(face-foreground 'success))))
  (add-hook 'prog-mode-hook #'hl-todo-mode))

(use-package ace-window
  :ensure t
  :commands (ace-window ace-swap-window ace-delete-window
                        ace-select-window ace-delete-other-windows)
  :init
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)
        aw-scope 'frame
        aw-background t)
  :bind
  (([remap other-window] . ace-window)))

(use-package telephone-line
  :ensure t
  :init
  (add-hook 'nx-post-init-hook #'telephone-line-mode))

;; Ivy
;;; Packages
(use-package flx
  :ensure t)

(use-package ivy
  :ensure t
  :diminish ivy-mode
  :init
  (defun sanityinc/enable-ivy-flx-matching ()
    "Make `ivy' matching work more like IDO."
    (interactive)
    (require 'flx)
    (setq-default ivy-re-builders-alist '((t . ivy--regex-fuzzy))))

  (setq ivy-use-virtual-buffers t
        ivy-virtual-abbreviate 'fullpath
        ivy-count-format ""
        ivy-height 12
        ivy-wrap t
        ivy-do-completion-in-region t
        ivy-fixed-height-minibuffer t
        ivy-initial-inputs-alist nil
        ivy-format-function #'ivy-format-function-line)
  ;; projectile-completion-system 'ivy

  (add-hook 'nx-post-init-hook #'ivy-mode)
  :bind
  (:map ivy-mode-map
    ([remap switch-to-buffer] . ivy-switch-buffer)
    ([remap imenu-anywhere]   . ivy-imenu-everywhere)
    :map ivy-minibuffer-map
	("<return>" . ivy-alt-done)
	("C-j" . ivy-immediate-done)
	("C-<return>" . ivy-immediate-done)))

(use-package counsel
  :ensure t
  :diminish counsel-mode
  :init
  (defun sanityinc/counsel-search-project (initial-input &optional use-current-dir)
    (interactive (list (thing-at-point 'symbol)
                       current-prefix-arg))
    (let ((current-prefix-arg)
          (dir (if use-current-dir
                   default-directory
                 (condition-case err
                     (projectile-project-root)
                   (error default-directory)))))
      (counsel-ag initial-input dir)))
  (setq-default counsel-mode-override-describe-bindings t)
  (counsel-mode)
  :bind
  (:map ivy-mode-map
    ([remap apropos] . counsel-apropos)
    ([remap describe-face] . counsel-describe-face)
    ([remap find-file] . counsel-find-file)
    ([remap recentf-open-files] . counsel-recentf)
    ([remap imenu] . counsel-imenu)
    ([remap bookmark-jump] . counsel-bookmark)
    ([remap execute-extended-command] . counsel-M-x)
    ([remap describe-function] . counsel-describe-function)
    ([remap describe-variable] . counsel-describe-variable)))

(use-package counsel-projectile
  :ensure t
  :after (counsel projectile))

(use-package swiper
  :ensure t
  :commands (swiper swiper-all)
  :init
  (defun sanityinc/swiper-at-point (sym)
    "Use `swiper' to search for the symbol at point."
    (interactive (list (thing-at-point 'symbol)))
    (swiper sym))
  :bind
  (:map ivy-mode-map
	("M-s /" . sanityinc/swiper-at-point)))

(use-package ivy-historian
  :ensure t
  :after ivy
  :init
  (ivy-historian-mode t))

(use-package smex
  :ensure t
  :after ivy
  :commands (smex smex-major-mode-commands)
  :init
  (setq-default smex-save-file (concat nx-cache-dir "smex-items"))
  (setq smex-completion-method 'ivy)
  (smex-initialize)
  :bind
  (([remap execute-extended-command] . smex)))

;; Projectile
;;; Packages
(use-package projectile
  :ensure t
  :init
  (setq projectile-completion-system 'ivy
	    projectile-enable-caching t
	    projectile-cache-file "~/.emacs.d/projectile.cache"
	    projectile-indexing-method 'alien
	    projectile-find-dir-includes-top-level t)
  (run-with-idle-timer 10 nil #'projectile-cleanup-known-projects)
  (add-hook 'nx-post-init-hook #'projectile-mode)
  :diminish projectile-mode)

;; Company
;;; Functions
(defun sanityinc--local-push-company-backend (backend)
  "Add BACKEND to a buffer-local version of `company-backends'."
  (make-local-variable 'company-backends)
  (push backend company-backends))

;;; Packages
(use-package company
  :ensure t
  :commands (company-mode global-company-mode company-complete
                          company-complete-common company-manual-begin
                          company-grab-line)
  :init
  (setq company-backends '(company-capf company-dabbrev-code company-dabbrev)
        company-frontends '(company-pseudo-tooltip-frontend company-echo-metadata-frontend)
        company-transformers '(company-sort-by-occurrence)
        company-global-modes '(not eshell-mode comint-mode erc-mode message-mode help-mode gud-mode)
        company-dabbrev-other-buffers 'all
        company-dabbrev-downcase nil
        company-dabbrev-ignore-case nil
        company-tooltip-align-annotation t
        company-idle-delay 0.2
        company-show-numbers t
        company-tooltip-limit 10
        company-tooltip-flip-when-above t
        company-tooltip-align-annotations t
        company-require-match 'never
        completion-cycle-threshold 2
        tab-always-indent 'complete)
  (add-hook 'nx-post-init-hook #'global-company-mode)
  :bind
  (([remap dabbrev-expand] . company-dabbrev)
   :map company-active-map
   ("C-n" . company-select-next)
   ("C-p" . company-select-prev))
  :diminish company-mode)

(use-package company-quickhelp
  :ensure t
  :after company
  :init (company-quickhelp-mode))

(use-package company-statistics
  :ensure t
  :after company
  :if window-system
  :init
  (setq company-statistics-file (concat nx-cache-dir "company-stats-cache.el"))
  (company-statistics-mode))

(use-package company-dict
  :ensure t
  :commands company-dict)

(use-package company-math
  :ensure t
  :after company
  :init
  (add-to-list 'company-backends 'company-math-symbols-unicode))

;; Isearch
;; Functions/Macros
(defun nx/isearch-yank-symbol ()
  "Put symbol at current point into search string."
  (interactive)
  (let ((sym (symbol-at-point)))
    (if sym
	    (progn
	      (setq isearch-regexp t
		        isearch-string (concat "\\_<" (regexp-quote (symbol-name sym)) "\\_>")
		        isearch-message (mapconcat 'isearch-text-char-description isearch-string "")
		        isearch-yank-flag t))
	  (ding)))
  (isearch-search-and-update))

(defun nx/isearch-exit-other-end (&optional rbeg rend)
  "Exit isearch, but at the other end of the search string.
This is useful when followed by an immediate kill.

RBEG and REND denote the `isearch' region."
  (interactive "r")
  (isearch-exit)
  (goto-char isearch-other-end))

;;; Packages
(use-package anzu
  :ensure t
  :diminish anzu-mode
  :init
  (add-hook 'nx-post-init-hook #'global-anzu-mode)
  (setq anzu-cons-mode-line-p nil
	    anzu-mode-lighter "")
  :bind
  (([remap query-replace] . anzu-query-replace)
   ([remap query-replace-regexp] . anzu-query-replace-regexp)
   :map isearch-mode-map
   ([remap isearch-query-replace] . anzu-isearch-query-replace)
   ([remap isearch-query-replace-regexp] . anzu-isearch-query-replace-regexp)
   ([remap isearch-delete-char] . isearch-del-char)
   ("C-M-w" . isearch-yank-symbol)
   ("C-<return>" . isearch-exit-other-end)))

;; Buffers
;;; Base configuration
(setq display-buffer-alist
      ;; magit status window in a fullscreen window
      `((,(rx "*magit: ")
         (display-buffer-fullframe)
         (reusable-frames . nil))
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
                  (and (1+ nonl) " output*")))
         (display-buffer-reuse-window
          display-buffer-in-side-window)
         (side . bottom)
         (reusable-frames . visible)
         (window-height . 0.33))
        ("." nil (reusable-frames . visible))))

;;; Variables/Constants
(defconst lunaryorn--do-not-kill-buffer-names '("*scratch*" "*Messages*")
  "Names of buffers that should not be killed.")

;;; Functions/Macros
(defun lunaryorn|do-not-kill-important-buffers ()
  "Inhibit killing of important buffers."
  (if (not (member (buffer-name) lunaryorn--do-not-kill-buffer-names))
	  t
    (message "Not allowed to kill %s, burying instead" (buffer-name))
    (bury-buffer)
    nil))

(defun lunaryorn/kill-this-buffer ()
  "Kill the current buffer."
  (interactive)
  (kill-buffer (current-buffer)))

;;; Add hooks/advice
(add-hook 'kill-buffer-query-functions #'lunaryorn|do-not-kill-important-buffers)

;;; Packages
(use-package uniquify
  :demand t
  :config
  (setq uniquify-buffer-name-style 'forward))

(use-package ibuffer
  :init
  (defun +ibuffer|set-up-preferred-filters ()
    (ibuffer-vc-set-filter-groups-by-vc-root)
    (unless (eq ibuffer-sorting-mode 'filename/process)
      (ibuffer-do-sort-by-filename/process)))
  (add-hook 'ibuffer-hook #'+ibuffer|set-up-preferred-filters)
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
          (mark " " (name 16 -1) " " filename))
	    ibuffer-filter-group-face 'font-lock-doc-face)
  (setq-default ibuffer-show-empty-filter-groups nil)
  :config
  (define-ibuffer-column size-h
    (:name "Size" :inline t)
    (cond
     ((> (buffer-size) 1000000) (format "%7.1fM" (/ (buffer-size) 1000000.0)))
     ((> (buffer-size) 1000) (format "%7.1fk" (/ (buffer-size) 1000.0)))
     (t (format ("%8d" (buffer-size))))))
  :bind
  (([remap list-buffers] . ibuffer)))

(use-package ibuffer-vc
  :ensure t
  :init
  (defun +ibuffer-vc|group-by-project-and-status ()
    "Group buffers by project and status."
    (ibuffer-vc-set-filter-groups-by-vc-root)
    (unless (eq ibuffer-sorting-mode 'alphabetic)
      (ibuffer-do-sort-by-alphabetic)))
  (add-hook  'ibuffer-hook #'ibuffer-vc|group-by-project-and-status))

;; File management
;;; Basic Configuration
(setq view-read-only t)

;;; Functions/Macros
(defun lunaryorn/delete-this-file ()
  "Delete the current file in this buffer and kill the buffer."
  (interactive)
  (let ((filename (buffer-file-name)))
    (when (yes-or-no-p (format "Really delete '%s'?"
			                   (file-name-nondirectory buffer-file-name)))
	  (cond
	   ((not filename) (kill-this-buffer))
	   ((vc-backend filename) (vc-delete-file filename))
	   (t
	    (delete-file filename)
	    (kill-this-buffer))))))

;; Rename current file
(defun lunaryorn/rename-this-file-and-buffer ()
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

(defun lunaryorn/copy-filename-as-kill (&optional arg)
  "Copy the name of the currently visited file to kill ring.

With a zero prefix ARG, copy the absolute file name.  With
\\[universal-argument], copy the file name relative to the
current Projectile project, or to the current buffer's
`default-directory', if the file is not part of any project.
Otherwise copy the non-directory part only."
  (interactive "P")
  (if-let* ((file-name (current-file))
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

(defun lunaryorn/launch-dwim ()
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


;;; Packages
(use-package dired
  :init
  (setq dired-auto-revert-buffer t
        dired-listing-switches "-alhF --group-directories-first -v"
        dired-ls-F-marks-symlinks t
        dired-recursive-copies 'always
        dired-recursive-deletes 'top    ;; Only ask for top dir when deleting
        dired-dwim-target t))           ;; Use the dired buffer in other window if it exists

;; Dired enhancements
(use-package dired-x
  :after dired
  :init
  (setq dired-dwim-target t)
  (add-hook 'dired-mode-hook #'dired-omit-mode)
  :config
  (put 'dired-find-alternate-file 'disabled nil)  ;; Re-use current buffer when pressing 'a'
  ;; this is a hack to correctly diminish `dired-omit-mode'
  (add-function :after (symbol-function 'dired-omit-startup)
                (lambda () (diminish 'dired-omit-mode))
                '((name . dired-omit-mode-diminish))))

(use-package dired-sort
  :ensure t
  :after dired)

;; Ignore uninteresting files
(use-package ignoramus
  :ensure t
  :init
  (add-hook 'nx-post-init-hook #'ignoramus-setup))

;; Grep
;;; Functions/Macros
(defun +grep|run-grep-from-here (&rest _)
  "Run grep recursively from the directory of the current buffer or the default directory."
  (interactive)
  (let* ((dir (file-name-directory (or load-file-name buffer-file-name default-directory)))
         (command (read-from-minibuffer "Run grep (like this): "
                                        (cons (concat "grep -nH -r  " dir) 13))))
    (grep command)))

;;; Packages
(use-package grep
  :if (executable-find "grep")
  :init
  (setq grep-highlight-matches t
	    grep-scroll-output t))

(use-package sift
  :if (executable-find "sift")
  :ensure t
  :init
  (defun +sift/grep-current-file-directory (query &optional args)
    "Run a Sift search with QUERY rooted at the directory of the current buffer (or the default directory).

ARGS provides Sift command line arguments."
    (interactive
     (list (read-from-minibuffer "Sift search for: " (thing-at-point 'symbol))))
    (let ((default-directory (file-name-directory (or load-file-name buffer-file-name default-directory))))
      (compilation-start
       (mapconcat 'identity
                  (append (list sift-executable)
                          sift-arguments
                          args
                          '("--color" "-n" "--stats")
                          (list query ".")) " ")
       'sift-search-mode)))

  (defun +sift/current-file-directory-regex (regex &optional args)
    "Run a Sift search with REGEX rooted at the directory of the current buffer (or the default directory).

ARGS provides Sift command line arguments."
    (interactive
     (list (read-from-minibuffer "Sift search for: " (thing-at-point 'symbol))))
    (+nx/sift-current-file-directory (shell-quote-argument regex) args)))

;; Programming
;;; Packages
(use-package prog-mode
  :init
  (setq font-lock-maximum-decoration 2)
  :bind
  (("C-c t p" . prettify-symbols-mode))) 

(use-package which-func
  :commands which-function-mode
  :init
  (setq mode-line-misc-info (assq-delete-all 'which-func-mode mode-line-misc-info)
	which-func-unknown "n/a")
  (add-hook 'prog-mode-hook #'which-function-mode))

(use-package rainbow-delimiters
  :ensure t
  :commands rainbow-delimiters-mode
  :init
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(use-package symbol-overlay
  :ensure t
  :commands (symbol-overlay-mode symbol-overlay-jump-next symbol-overlay-jump-prev)
  :init
  (add-hook 'prog-mode-hook #'symbol-overlay-mode)
  :bind
  (:map symbol-overlay-mode-map
	("M-n" . symbol-overlay-jump-next)
	("M-p" . symbol-overlay-jump-prev))
  :diminish symbol-overlay-mode)

(use-package highlight-escape-sequences
  :ensure t
  :commands hes-mode
  :init
  (add-hook 'prog-mode-hook #'hes-mode)
  (add-hook 'text-mode-hook #'hes-mode))

(use-package flycheck
  :ensure t
  :commands (flycheck-mode flycheck-pos-tip-mode)
  :init
  (add-hook #'prog-mode-hook #'flycheck-mode)
  (add-hook #'flycheck-mode #'flycheck-pos-tip-mode)
  :diminish flycheck-mode)

(use-package electric
  ;; built-in
  :init
  (add-hook #'prog-mode-hook #'electric-indent-mode)
  :diminish electric-mode)

;; C/C++
;;; Functions
(defun nx|c++-mode-hook ()
  "Personal C++ style."
  (c-set-style "stroustrup")
  (c-set-offset 'innamespace '0))

(defun nx|irony-mode-hook ()
  "Remap completion keys in `irony-mode'."
  (define-key irony-mode-map [remap completion-at-point] #'irony-completion-at-point-async)
  (define-key irony-mode-map [remap complete-symbol] #'irony-completion-at-point-async))

;;; Packages
(use-package cc-mode
  ;; built-in
  :after (company projectile flycheck)
  :init
  (setq c-default-style "stroustrup"
		c-basic-offset 4
		c-toggle-hungry-state 1)
  (add-to-list 'auto-mode-alist '("\\.n\\'" . c++-mode))
  (add-hook 'c-mode-common-hook #'hs-minor-mode)
  (add-hook 'c++-mode-hook #'nx|c++-mode-hook)
  :bind
  (:map c-mode-map
   ("C-c m a" . projectile-find-other-file)
   ("C-c m A" . projectile-find-other-file-other-window)
   :map c++-mode-map
   ("C-c m a" . projectile-find-other-file)
   ("C-c m A" . projectile-find-other-file-other-window)))

(use-package company-c-headers
  :ensure t
  :init
  (add-hook 'c-mode-common-hook (lambda ()
                                  (sanityinc--local-push-company-backend 'company-c-headers))))

(use-package irony
  :ensure t
  :commands irony-mode
  :init
  (add-hook 'c-mode-common-hook #'irony-mode)
  :config
  (irony-cdb-autosetup-compile-options))

(use-package company-irony
  :ensure t
  :after (company-irony-c-headers)
  :init
  (when (boundp 'w32-pipe-read-delay)
    (setq w32-pipe-read-delay 0))
  (when (boundp 'w32-pipe-buffer-size)
    (setq irony-server-w32-pipe-buffer-size (* 64 1024)))
  (add-hook #'irony-mode-hook #'nx|irony-mode-hook)
  (add-hook #'irony-mode-hook #'company-irony-setup-begin-commands)
  :config
  (add-to-list 'company-backends '(company-irony-c-headers company-irony))
  :diminish irony-mode)

(use-package company-irony-c-headers
  :ensure t
  :after (company irony)
  ;; see company-irony for :config
  )

(use-package flycheck-irony
  :ensure t
  :after (flycheck irony)
  :commands flycheck-irony-setup
  :init
  (add-hook #'irony-mode-hook #'flycheck-irony-setup))

(use-package modern-cpp-font-lock
  :ensure t
  :commands modern-c++-font-lock-mode
  :init
  (add-hook 'c++-mode-hook #'modern-c++-font-lock-mode)
  :diminish
  modern-c++-font-lock-mode)

;; Version Control
;;; Functions
(defun +git|git-gutter-maybe ()
  "Enable `git-gutter-mode' in non-remote buffers."
  (when (and (buffer-file-name)
             (not (file-remote-p (buffer-file-name))))
    (git-gutter-mode)))

;;; Packages
(use-package vc-git
  ;; built-in
  :bind
  (:map vc-prefix-map
	("f" . vc-git-grep))
  )

(use-package vc-hooks
  ;; built-in
  :init
  (setq vc-follow-symlinks t))

(use-package magit
  :ensure t
  :init
  (setq magit-save-repository-buffers 'dontask
        magit-refs-show-commit-count 'all
        magit-log-buffer-file-locked t
        magit-revision-show-gravatars nil)
  (setq-default magit-diff-refine-hunk t
                magit-stage-all-confirm nil)
  (add-hook 'magit-mode-hook 'magit-load-config-extensions)
  ;;(add-hook 'after-save-hook 'magit-after-save-refresh-status)
  :bind
  (("C-c g l" . magit-log)
   ("C-c g f" . magit-file-log)
   ("C-c g b" . magit-blame-mode)
   ("C-c g B" . magit-branch)
   ("C-c g c" . magit-checkout)
   ("C-c g d" . magit-ediff-show-working-tree)
   ("C-c g s" . magit-status)
   ("C-c g S" . magit-stage-file)
   ("C-c g r" . magit-rebase)
   ("C-c g U" . magit-unstage-file)))

(use-package git-timemachine
  :ensure t
  :after magit
  :commands (git-timemachine git-timemachine-toggle)
  :config
  (require 'magit-blame)
  :bind
  (("C-c g t" . git-timemachine)))

(use-package git-messenger
  :ensure t
  :init
  (setq git-messenger:show-detail t)
  :bind
  (("C-c g p" . git-messenger:popup-message)
   :map git-messenger-map
   ("C-g" . git-messenger:popup-close)))

(use-package gitignore-mode
  :ensure t
  :mode "/\\.gitignore$")

(use-package gitconfig-mode
  :ensure t
  :mode "/\\.?git/?config$"
  :mode "/\\.gitmodules$")


(use-package git-commit
  :ensure t
  :init
  (remove-hook 'git-commit-finish-query-functions #'git-commit-check-style-conventions))

(use-package gitattributes-mode
  :ensure t)

(use-package git-gutter-fringe
  :ensure t
  :commands git-gutter-mode
  :init
  (dolist (mode '(text-mode prog-mode conf-mode))
    (add-hook mode #'+git|git-gutter-maybe)))

(use-package browse-at-remote
  :ensure t)

;; Section
;;; Base configuration
;;; Define variables/constants
;;; Define functions/macros
;;; Add hooks/advice
;;; Packages
;;;; Built-ins
;;;; Melpa

(provide '.emacs)
;;; .emacs ends here

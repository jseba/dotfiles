;;; config-editor.el -- Editor config

;;; Commentary:

;;; Code:

(defvar nx-large-file-size 1
  "Size (in MB) above which we will prompt to open a file literally.")

(defvar nx-large-file-modes-list '(archive-mode tar-mode jka-compr
                                                doc-view-mode doc-view-mode-maybe
                                                image-mode git-commit-mode pdf-view-mode
                                                ebrowse-tree-mode)
  "Major modes that `nx|check-large-file' will ignore.")

(defvar nx-auto-minor-mode-alist '()
  "Alist of filename patterns to corresponding minor mode functions.")

(defvar nx-yank-indent-threshold 1000
  "If pasting a number of lines past this threshold, disable auto-indention.")

(defvar nx-indent-sensitive-modes '(conf-mode coffee-mode haml-mode python-mode slim-mode yaml-mode)
  "List of modes to limit auto-indention.")

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
  "Kill whole line and move back to indentation.

With ARG."
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
  "Move forward to the last non-blank character in the line.

Ignores comments and trailing whitespace. If already there, move
to the real end of the line. If already there, do nothing."
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

(defun nx/smart-backward-sexp (arg)
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

(defun nx/isearch-exit-other-end (&optional _rbeg _rend)
  "Exit isearch, but at the other end of the search string.
This is useful when followed by an immediate kill.

RBEG and REND denote the `isearch' region."
  (interactive "r")
  (isearch-exit)
  (goto-char isearch-other-end))

(add-hook 'kill-buffer-query-functions #'nx|dont-kill-scratch-buffer)
(add-hook 'find-file-hook #'nx|check-large-file)
(add-hook 'find-file-hook #'nx|enable-minor-mode-maybe)
(advice-add #'make-indirect-buffer :around #'nx*set-indirect-buffer-filename)
(advice-add #'delete-trailing-whitespace :around #'nx*delete-trailing-whitespace)

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

(use-package align)

(use-package expand-region
  :ensure t)

(use-package editorconfig
  :ensure t
  :demand t
  :config
  (add-hook 'nx-post-init-hook #'editorconfig-mode)
  :diminish editorconfig-mode)

(use-package smartparens
  :ensure t
  :demand t
  :config
  (setq sp-autowrap-region t
        sp-cancel-autoskip-on-backward-movement nil
        sp-highlight-pair-overlay nil
        sp-show-pair-delay 0
        sp-max-pair-length 6
        sp-autoskip-closing-pair 'always
        sp-hybrid-kill-entire-symbol nil)
  (add-hook 'nx-init-hook #'smartparens-global-mode)
  :diminish smartparens-mode)

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
  (setq browse-kill-ring-separator "\f"))

(use-package zop-to-char
  :ensure t)

(use-package avy
  :ensure t
  :init
  (setq avy-background t
	    avy-style 'at-full))

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

(use-package anzu
  :ensure t
  :diminish anzu-mode
  :init
  (add-hook 'nx-post-init-hook #'global-anzu-mode)
  (setq anzu-cons-mode-line-p nil
	    anzu-mode-lighter ""))

(provide 'config-editor)
;;; config-editor.el ends here

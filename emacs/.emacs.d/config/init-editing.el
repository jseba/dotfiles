(use-package unfill
  :ensure t)

(use-package electric
  ;; built-in
  :init
  (electric-pair-mode))

(use-package editing-prefs
  :init
  (defun sanityinc/newline-at-end-of-line ()
    "Move to end of line, enter a newline and reindent."
    (interactive)
    (move-end-of-line 1)
    (newline-and-indent))

  (defun smart-open-line ()
    "Insert empty line after the current line."
    (interactive)
    (move-end-of-line nil)
    (newline-and-indent))

  (defun smart-kill-whole-line (&optional arg)
    "Kill whole line and move back to indentation."
    (interactive "p")
    (kill-whole-line arg)
    (back-to-indentation))

  (defun smart-backward-kill-line ()
    "Kill line backwards from point and re-indent."
    (interactive)
    (kill-line 0)
    (indent-according-to-mode))

  (defun smart-move-beginning-of-line (arg)
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

  (defun smart-back-to-indentation (arg)
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

  (defun kill-back-to-indentation ()
    "Kill from point back to the first non-whitespace character on the current line."
    (interactive)
    (let ((prev-pos (point)))
      (back-to-indentation)
      (kill-region (point) prev-pos)))

  (defun smart-backward-up-sexp (arg)
    "Jump up to the start of the ARG'th enclosing sexp."
    (interactive "p")
    (let ((ppss (syntax-ppss)))
      (cond
       ((elt ppss 3)
	(goto-char (elt ppss 8))
	(backward-up-sexp (1- arg)))
       (backward-up-list arg))))

  (setq bookmark-default-file "~/.emacs.d/bookmarks.el"
	buffers-menu-max-size 30
	case-fold-search t
	column-number-mode t
	delete-selection-mode t
	fill-column 160
	indent-tabs-mode nil
	indicate-empty-lines t
	make-backup-files nil
	mouse-wheel-scroll-amount '(3)
	mouse-wheel-progressive-speed nil
	mouse-yank-at-point t
	require-final-newline t
	ring-bell-function #'ignore
	save-interprogram-paste-before-kill t
	scroll-preserve-screen-position 'always
	set-mark-command-repeat-pop t
	tab-width 4
	tooltip-delay 1.5
	truncate-lins nil
	trncate-partial-width-windows nil)
  (setq-default show-trailing-whitespace t)

  ;; Bring back region casing
  (put 'upcase-region 'disabled nil)
  (put 'downcase-region 'disabled nil)

  ;; Bring back narrowing
  (put 'narrow-to-region 'disabled nil)
  (put 'narrow-to-page 'disabled nil)
  (put 'narrow-to-defun 'disabled nil)

  ;; Set encoding defaults
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (set-language-environment "UTF-8")
  (prefer-coding-system 'utf-8)

  ;; Set editing modes
  (blink-cursor-mode -1)
  (delete-selection-mode)
  (transient-mark-mode)
  (line-number-mode t)
  (column-number-mode t)
  (size-indication-mode t)

  (add-hook 'text-mode-hook #'auto-fill-mode)

  :bind
  (("C-c x i" . indent-region)
   ([remap just-one-space] . cycle-spacing)
   ("RET" . newline-and-indent)
   ("S-RET" . sanityinc/newline-at-end-of-line)
   ([remap move-beginning-of-line] . smart-move-beginning-of-line)
   ([remap kill-whole-line] . smart-kill-whole-line)
   ([remap open-line] . smart-open-line)
   ([remap backward-up-sexp] . smart-backward-up-sexp)
   ("C-S-DEL" . smart-backward-kill-line)
   ([remap back-to-indentation] . smart-back-to-indentation))

  :diminish
  auto-fill-function)

(use-package beacon
  :ensure t
  :init
  (setq-default beacon-lighter ""
		beacon-size 5)
  (beacon-mode)
  :diminish beacon-mode)

(use-package subword
  :init (subword-mode)
  :diminish subword-mode)

(use-package nlinum
  :ensure t)

(use-package undo-tree
  :ensure t
  :init (global-undo-tree-mode)
  :diminish undo-tree-mode)

(use-package browse-kill-ring
  :ensure t
  :init
  (setq browse-kill-ring-separator "\f")
  :bind
  (("M-Y" . browse-kill-ring)
   :map browse-kill-ring-map
   ("C-g" . browse-kill-ring-quit)
   ("M-n" . browse-kill-ring-forward)
   ("M-p" . browse-kill-ring-previous)))

(use-package zop-to-char
  :ensure t
  :bind
  (("M-z" . zop-to-char)
   ("M-Z" . zop-up-to-char)))

(use-package align
  ;; built-in
  :bind
  (("C-c x a a" . align)
   ("C-c x a c" . align-current)))

(use-package expand-region
  :ensure t
  :bind
  (("C-=" . er/expand-region)))

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
  (setq which-key-idle-delay 0.4
	which-key-sort-order 'which-key-prefix-then-key-order
	which-key-popup-type 'side-window
	which-key-popup-window-location 'bottom
	which-key-side-windows-max-height 0.15
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
	  ("\\`sanityinc-"  . "")
	  ("projectile-"    . "proj-")
	  ("magit-"         . "ma-")))
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

  (which-key-mode)
  :diminish which-key-mode)

(provide 'init-editing)

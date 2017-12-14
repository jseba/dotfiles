;;; lisp/init-editor.el -*- lexical-binding: t; -*-

(defvar nx-large-file-size 1
  "Size (in MB) above which we will prompt to open a file literally.")
(defvar nx-large-file-modes-list '(archive-mode tar-mode jka-compr
                                   doc-view-mode doc-view-mode-maybe
                                   image-mode git-commit-mode pdf-view-mode
                                   ebrowse-tree-mode)
  "Major modes that `nx|check-large-file' will ignore.")

(defun nx|dont-kill-scratch-buffer ()
  "Don't kill the scratch buffer."
  (or (not (string= (buffer-file-name) "*scratch*"))
      (ignore (bury-buffer))))
(add-hook 'kill-buffer-query-functions #'nx|dont-kill-scratch-buffer)

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
(add-hook 'find-file-hook #'nx|check-large-file)

(defvar nx-auto-minor-mode-alist '()
  "")
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
(add-hook 'find-file-hook #'nx|enable-minor-mode-maybe)

(defun nx*set-indirect-buffer-filename (orig-fn base-buffer name &optional clone)
  "In indirect buffers, `buffer-file-name' is nil, which can cause problems
with functions that require it (like modeline segments)."
  (let ((file-name (buffer-file-name base-buffer))
        (buffer (funcall orig-fn base-buffer name clone)))
    (when (and file-name buffer)
      (with-current-buffer buffer
        (unless buffer-file-name
          (setq buffer-file-name file-name
                buffer-file-truename (file-truename file-name)))))
    buffer))
(advice-add #'make-indirect-buffer :around #'nx*set-indirect-buffer-filename)

(defun nx*delete-trailing-whitespace (orig-fn &rest args)
  "Don't affect trailing whitespace on current line."
  (let ((linestr (buffer-substring-no-properties
                  (line-beginning-position)
                  (line-end-position))))
    (apply orig-fn args)
    (when (string-match-p "^[\s\t]*$" linestr)
      (insert linestr))))
(advice-add #'delete-trailing-whitespace :around #'nx*delete-trailing-whitespace)

(setq-default
 auto-save-list-file-name (concat nx-cache-dir "autosave")
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
 show-trailing-whitespace t
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

;;;
;;  Built-in packages
;;;

(use-package autorevert
  :demand t
  :config
  (setq auto-revert-verbose nil)
  (global-auto-revert-mode 1)
  :diminish auto-revert-mode)

(use-package electric
  :demand t
  :config
  (electric-indent-mode 1)
  :diminish electric-indent-mode)

(use-package paren
  :demand t
  :config
  (show-paren-mode))

(use-package savehist
  :demand t
  :config
  (setq savehist-file (concat nx-cache-dir "savehist")
        savehist-save-minibuffer-history t
        savehist-autosave-interval nil
        savehist-additional-variables '(kill-ring search-ring regexp-search-ring))
  (add-hook 'nx-init-hook #'savehist-mode)
  :diminish savehist-mode)

(use-package saveplace
  :demand t
  :config
  (setq save-place-file (concat nx-cache-dir "saveplace"))
  (add-hook 'nx-init-hook #'save-place-mode)
  :diminish save-place-mode)

(use-package recentf
  :demand t
  :config
  (setq recentf-save-file (concat nx-etc-dir "recentf")
        recentf-max-menu-items 0
        recentf-max-saved-items 300
        recentf-exclude (list  "^/tmp/" "^/ssh:" "\\.?ido\\.last$" "\\.revive$" "/TAGS$"
                               "^/var/folders/.+$"))
  (add-hook 'nx-init-hook #'recentf-mode)
  :diminish recentf-mode)

(use-package editorconfig
  :ensure t
  :demand t
  :config
  (add-hook 'nx-init-hook #'editorconfig-mode)
  :diminish editorconfig-mode)

(use-package smartparens
  :ensure t
  :demand t
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
  (dolist (hook '(emacs-lisp-mode-hook lisp-mode-hook))
    (add-hook hook #'smartparens-strict-mode))
  :diminish smartparens-mode)

(use-package unfill
  :ensure t)

(use-package electric
  ;; built-in
  :init
  (add-hook 'prog-mode-hook #'electric-indent-mode))

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

(use-package subword
  :init (subword-mode)
  :diminish subword-mode)

(use-package nlinum
  :ensure t)

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
	  ("\\`lunaryorn/"  . "")
	  ("\\`sanityinc/"  . "")
      ("\\`doom/"       . "")
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


(provide 'init-editor)

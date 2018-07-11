;;; private/jseba/config.el  -*- lexical-binding: t; -*-

(pcase system-type
  ('windows-nt)
  ('darwin
   (setq ns-alternate-modifier 'meta
         ns-command-modifier 'super)))

(map!
 ;; --- Global keybindings ---------------------------
 [remap kill-whole-line]            #'nx/smart-kill-whole-line
 [remap move-beginning-of-line]     #'doom/backward-to-bol-or-indent
 [remap move-end-of-line]           #'doom/forward-to-last-non-comment-or-eol
 [remap delete-backward-char]       #'doom/delete-backward-char
 [remap back-to-indentation]        #'nx/smart-back-to-indentation
 [remap split-window-vertically]    #'nx/split-window-vertically-with-other-buffer
 [remap split-window-horizontally]  #'nx/split-window-horizontally-with-other-buffer
 [remap delete-other-windows]       #'nx/toggle-delete-other-windows

 (:map company-mode-map
   [remap dabbrev-expand] #'company-dabbrev)

 (:map counsel-mode-map
   [remap apropos]                  #'counsel-apropos
   [remap bookmark-jump]            #'counsel-bookmark
   [remap describe-face]            #'counsel-describe-face
   [remap describe-function]        #'counsel-describe-function
   [remap describe-variable]        #'counsel-describe-variable
   [remap execute-extended-command] #'counsel-M-x
   [remap find-file]                #'counsel-find-file
   [remap find-library]             #'counsel-find-library
   [remap info-lookup-symbol]       #'counsel-info-lookup-symbol
   [remap imenu]                    #'counsel-imenu
   [remap recentf-open-files]       #'counsel-recentf
   [remap org-capture]              #'counsel-org-capture
   [remap swiper]                   #'counsel-grep-or-swiper)

 (:map ivy-mode-map
   [remap switch-to-buffer] #'ivy-switch-buffer
   [remap imenu-anywhere]   #'ivy-imenu-anywhere)

 (:map helpful-mode-map
   [remap describe-function] #'helpful-callable
   [remap describe-variable] #'helpful-variable
   [remap describe-command]  #'helpful-command
   [remap describe-key]      #'helpful-key)

 (:map ace-window-mode-map
   [remap other-window] #'ace-window)

 (:map magit-status-mode-map
   [remap magit-mode-bury-buffer] #'+magit/quit)

 (:map org-mode-map
   [remap beginning-of-line] #'org-beginning-of-line
   [remap end-of-line]       #'org-end-of-line))

(load! (if (featurep! :feature evil) "+evil-bindings" "+bindings"))

(cond (IS-MAC
       (define-key input-decode-map [S-iso-lefttab] [backtab]))
      (IS-WINDOWS
       (setq vc-handled-backends (delq 'Git vc-handled-backends))))
(add-hook! 'tty-setup-hook (define-key input-decode-map (kbd "TAB") [tab]))

(when (featurep! :feature evil)
  (setq evil-cross-lines t
        evil-escape-key-sequence "kj"
        evil-want-fine-undo nil
        expand-region-contract-fast-key "V"))

(setq doom-font (font-spec :family "Hack" :size 12)
      doom-big-font (font-spec :family "Hack" :size 16)
      tab-always-indent 'complete)

(def-package! ivy-xref)

;; TODO: move to LSP module
(def-package! lsp-mode)
(def-package! lsp-imenu)
(def-package! company-lsp)

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
 [remap completion-at-point]        #'company-complete

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

(def-package! cc-mode
  :commands (c-mode c++-mode)
  :init
  (setq-default c-basic-offset tab-width
                c-backspace-function #'delete-backward-char
                c-default-style "nx"
                c-toggle-hungry-state 1)
  :config
  (set! :electric '(c-mode c++-mode)
        :chars '(?\n ?\}))

  (c-toggle-electric-state -1)
  (c-toggle-auto-newline -1)

  ;; personal style (based off Stroustrup)
  (map-put c-style-alist "nx"
           `((c-basic-offset . ,tab-width)
             (c-comment-only-line-offset . 0)
             (c-hanging-braces-alist (brace-list-open)
                                     (brace-entry-open)
                                     (substatement-open . after)
                                     (block-close . c-snug-do-while)
                                     (arglist-cont-nonempty))
             (c-cleanup-list brace-else-brace)
             (c-offsets-alist (statement-block-intro . +)
                              (knr-argdecl-intro . +)
                              (substatement . 0)
                              (substatement-open . 0)
                              (substatement-label . 0)
                              (statement-cont . +)
                              (case-label . 0)
                              (defun-open . 0)
                              (defun-close . 0)
                              (arglist-intro . +)
                              (arglist-cont +cc-lineup-arglist-close 0)
                              (arglist-cont-nonempty +cc-lineup-arglist-close 0)
                              (arglist-close +cc-lineup-arglist-close 0)
                              (template-args-cont +cc-lineup-arglist-close 0)
                              (inline-open . 0)
                              (inlambda . 0)
                              (innamespace . -)
                              (inexpr-class . 0)
                              (access-label . -)
                              (inclass +cc-c++-lineup-inclass +)
                              (inher-cont . c-lineup-multi-inher)
                              (label . 0))))

  (add-hook! prog-mode 'electric-indent-mode)
  (add-hook! c-mode-common-hook #'rainbow-delimiters-mode)
  (add-hook! (c-mode c++-mode) #'highlight-numbers-mode)
  (add-hook! (c-mode c++-mode) #'+cc|fontify-constants)
  (add-hook! (c-mode c++-mode) (lambda! (c-set-style "nx")))

  ;; Disable electric keys because it interferes with smartparens and
  ;; custom bindings. Do it manually instead.
  (setq c-tab-always-indent nil
        c-electric-flag nil)
  (dolist (key '("#" "{" "}" "/" "*" ";" "," ":" "(" ")" "\177"))
    (define-key c-mode-base-map key nil))

  ;; Smartparens and cc-mode both try to autoclose angle-brackets intelligently.
  ;; The result is...less intelligent than they think (redundant characters), so
  ;; do it manually.
  (map! :map c++-mode-map "<" nil ">" nil)

  ;; Employ smartparens for the job
  (sp-with-modes '(c++-mode)
    (sp-local-pair "<" ">"
                   :when '(+cc-sp-point-is-template-p +cc-sp-point-after-include-p)
                   :post-handlers '(("| " "SPC"))))
  (sp-with-modes '(c-mode c++-mode)
    (sp-local-pair "/*" "*/" :post-handlers '(("||\n[i]" "RET") ("| " "SPC")))
    ;; Doxygen blocks
    (sp-local-pair "/**" "*/" :post-handlers '(("||\n[i]" "RET") ("||\n[i]" "SPC")))
    (sp-local-pair "/*!" "*/" :post-handlers '(("||\n[i]" "RET") ("[d-1]< | " "SPC")))))

(def-package! modern-cpp-font-lock
  :hook (c++-mode . modern-c++-font-lock-mode))

(def-package! lsp-mode)
(def-package! lsp-imenu
  )
(def-package! company-lsp)
(def-package! cquery)
(def-package! cmake-mode)
(def-package! demangle-mode)
(def-package! disaster)
(def-package! ivy-xref)

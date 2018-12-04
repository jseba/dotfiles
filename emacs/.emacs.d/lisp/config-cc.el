;;; config-cc.el

(use-package cc-mode
  :ensure nil
  :commands (c-mode c++-mode)
  :preface
  ;; The plusses in `c++-mode' are annoying to type in helm
  (defalias 'cpp-mode 'c++-mode)
  (defvaralias 'cpp-mode-map 'c++-mode-map)

  :init
  (setq-default c-basic-offset tab-width
                c-backspace-function #'delete-backward-char
                c-default-style "llvm"
                c-toggle-hungry-state 1
                c-tab-always-indent nil
                c-electric-flag nil)

  (defun +cc-sp-point-is-template-p (id action context)
    "Return t if point is in the right place for C++ angle brackets."
    (and (sp-in-code-p id action context)
         (cond ((eq action 'insert)
                (sp-point-after-word-p id action context))
               ((eq action 'autoskip)
                (/= (char-before) 32)))))

  (defun +cc-sp-point-after-include-p (id action context)
    "Returns t if point is in an #include."
    (and (sp-in-code-p id action context)
         (save-excursion
           (goto-char (line-beginning-position))
           (looking-at-p "[ 	]*#include[^<]+"))))

  (defun +cc-c++-lineup-inclass (langelem)
    "Indent `inclass' lines one level further than access modifier keywords."
    (when (and (eq major-mode c++-mode)
               (or (assoc 'access-label c-syntactic-context)
                   (save-excursion
                     (save-match-data
                       (re-search-backward
                        "\\(?:p\\(?:ublic\\|r\\(?:otected\\|ivate\\)\\)\\)"
                        (c-langelem-pos langelem) t)))))
      '++))

  (defun +cc-lineup-arglist-close (langelem)
    "Line up the closing brace in an arglist with the opening brace IF the cursor is
preceeded by the opening brace or a comma (disregarding whitespace in between)."
    (when (save-excursion
            (save-match-data
              (skip-chars-backward " \t\n" (c-langelem-pos langelem))
              (memq (char-before) (list ?, ?\( ?\;))))
      (c-lineup-arglist langelem)))

  (defun +cc-fontify-constants ()
    "Better fontification for preprocessor constants."
    (font-lock-add-keywords
     nil '(("\\<[A-Z]*_[A-Z_]+\\>" . font-lock-constant-face)
           ("\\<A-Z]\\{3,\\}\\>"   . font-lock-constant-face))
     t))

  :config
  (c-toggle-electric-state -1)
  (c-toggle-auto-newline -1)

  (unless (assoc "llvm" c-style-alist)
    (push '("llvm"
            (fill-column . 80)
            (indent-tabs-mode . nil)
            (c++-indent-level . 2)
            (c-basic-offset . 2)
            (c-offsets-alist
             (arglist-close . c-lineup-arglist)
             (arglist-intro . ++)
             (brace-list-intro . c-lineup-arglist-intro-after-paren)
             (inline-open . 0)
             (innamespace . 0)
             (knr-argdecl-intro . 5)
             (label . 0)
             (member-init-intro . ++)
             (statement-block-intro . +)
             (statement-case-open . +)
             (statement-cont . +)
             (substatement-label . 0)
             (substatement-open . +)
             (topmost-intro-cont first
                                 c-lineup-topmost-intro-cont
                                 c-lineup-gnu-DEFUN-intro-cont))
            (c-special-indent-hook . c-gnu-impose-minimum)
            (c-block-comment-prefix . ""))
          c-style-alist))

  ;; TODO: create a Microsoft style

  (add-hook! (c-mode c++-mode) #'+cc-fontify-constants)

  ;; Disable electric keys as it interferes with smartparens;
  ;; do it manually instead
  (dolist (key '("#" "{" "}" "/" "*" ";" "," ":" "(" ")" "\177"))
    (define-key c-mode-base-map key nil))

  ;; Smartparens and cc-mode both try to autoclose angle-brackets
  ;; intelligently. The result is...less intelligent than they
  ;; think (redundant characters), so do it manually
  (define-key! c++-mode-map
    "<" nil
    ">" nil)

  ;; employ smartparens to do it properly
  (sp-with-modes '(c++-mode)
    (sp-local-pair "<" ">"
                   :when '(+cc-sp-point-is-template-p +cc-sp-point-after-include-p)
                   :post-handlers '(("| " "SPC")))))

(use-package modern-cpp-font-lock
  :hook (c++-mode . modern-c++-font-lock-mode))

(use-package ccls
  :when (executable-find "ccls")
  :init
  (require 'lsp)

  (setq ccls-executable (executable-find "ccls")
        ccls-cache-dir (concat %var-dir "ccls/")
        ccls-extra-args `(,(concat "--log-file="
                                     (expand-file-name
                                      (if %IS-WIN32
                                          (getenv "TEMP")
                                        "/tmp"))
                                     "/ccls.log"))
        ccls-extra-init-params '(:index
                                 (:comments 3)
                                 :completion
                                 (:detailedLabel t)))

  (after! projectile
    (push ".ccls" projectile-project-root-files-bottom-up))

  (defun +cc-enable-ccls-maybe ()
    "Enable ccls if compile_commands.json or .ccls files found in project root."
    (let ((default-directory (+projectile-project-root)))
      (when (or (file-exists-p ".ccls")
                (file-exists-p "compile_commands.json"))
        (require 'ccls)
        (lsp-ccls-enable))))
  (add-hook! '(c-mode-hook c++-mode-hook) #'+cc-enable-cquery-maybe)

  (defun +ccls-callee ()
    (interactive)
    (lsp-ui-peek-find-custom "$ccls/call" '(:callee t)))
  (defun +ccls-caller ()
    (interactive)
    (lsp-ui-peek-find-custom "$ccls/call"))
  (defun +ccls-vars (kind)
    (lsp-ui-peek-find-custom "$ccls/vars" `(:kind ,kind)))
  (defun +ccls-base (levels)
    (lsp-ui-peek-find-custom "$ccls/inheritance" `(:levels ,levels)))
  (defun +ccls-derived (levels)
    (lsp-ui-peek-find-custom "$ccls/inheritance" `(:levels ,levels :derived t)))
  (defun +ccls-member (kind)
    (lsp-ui-peek-find-custom "$ccls/member" `(:kind ,kind)))
  (defun +ccls-references-address ()
    (interactive)
    (lsp-ui-peek-find-custom "textDocument/references"
                             (plist-put (lsp--text-document-position-params) :role 128)))
  (defun +ccls-references-macro ()
    (interactive)
    (lsp-ui-peek-find-custom "textDocument/references"
                             (plust-put (lsp--text-document-position-params) :role 64)))
  (defun +ccls-references-not-call ()
    (interactive)
    (lsp-ui-peek-find-custom "textDocument/references"
                             (plist-put (lsp--text-document-position-params) :excludeRole 32)))
  (defun +ccls-references-read ()
    (interactive)
    (lsp-ui-peek-find-custom "textDocument/references"
                             (plist-put (lsp--text-document-position-params) :role 8)))
  (defun +ccls-references-write ()
    (interactive)
    (lsp-ui-peek-find-custom "textDocument/references"
                             (plist-put (lsp--text-document-position-params) :role 16)))

  (general-def
    :keymaps '(c-mode-map c++-mode-map)
    :prefix "C-c"
    "p" '(ccls-preprocess-file
          :wk "Preprocess file")
    "R" '(ccls-reload
          :wk "Reload ccls"))
  (general-def
    :keymaps '(c-mode-map c++-mode-map)
    :prefix "C-c x"
    ""  '(nil :wk "xref")
    "a" '(+ccls-references-address
          :wk "Variable Addresses")
    "F" '(+ccls-references-not-call
          :wk "Function Addresses")
    "P" '(+ccls-references-macro
          :wk "Macro References")
    "r" '(+ccls-references-read
          :wk "Read References")
    "w" '(+ccls-references-write
          :wk "Write References")
    "b" `(,(lambda! (+ccls-base 1))
          :wk "Direct Bases")
    "B" `(,(lambda! (+ccls-base 3))
          :wk "Bases")
    "d" `(,(lambda! (+ccls-derived 1))
          :wk "Direct Derived")
    "D" `(,(lambda! (+ccls-derived 3))
          :wk "Derived")
    "i" '(ccls-inheritance-hierarchy
          :wk "Base Hierarchy")
    "I" `(,(lambda! (ccls-inheritance-hierarchy t))
          :wk "Derived Hierarchy")
    "c" '(+ccls-caller
          :wk "Callers")
    "C" '(+ccls-callee
          :wk "Callees")
    "e" '(ccls-call-hierarchy
          :wk "Caller Hierarchy")
    "E" `(,(lambda! (ccls-call-hierarchy t))
          :wk "Callee Hierarchy")
    "s" `(,(lambda! (+ccls-member 2))
          :wk "Nested Classes")
    "f" `(,(lambda! (+ccls-member 3))
          :wk "Member Functions")
    "m" `(,(lambda! (+ccls-member 0))
          :wk "Member Variables")
    "M" '(ccls-member-hierarchy
          :wk "Member Hierarchy")
    "v" `(,(lambda! (+ccls-vars 3))
          :wk "Local Variables")
    "V" `(,(lambda! (+ccls-vars 1))
          :wk "Fields")
    "C-v" `(,(lambda! (+ccls-vars 7))
            :wk "All Variables")
    "t" '(lsp-goto-type-definition
          :wk "Go To Type Definition")
    "L" '(ccls-code-lens-mode
          :wk "Code Lens Mode")))

(use-package cquery
  :when (and (executable-find "cquery") (not (executable-find "ccls")))
  :commands lsp-cquery-enable
  :init
  (setq cquery-executable (executable-find "cquery")
        cquery-cache-dir (concat %var-dir "cquery/")
        cquery-extra-args `(,(concat "--log-file="
                                     (expand-file-name
                                      (if %IS-WIN32
                                          (getenv "TEMP")
                                        "/tmp"))
                                     "/cquery.log"))
        cquery-extra-init-params '(:index
                                   (:comments 3)
                                   :completion
                                   (:detailedLabel t)))

  ;; add ".cquery" configuration file as a project root
  (after! projectile
    (push ".cquery" projectile-project-root-files-bottom-up))

  (defun +cc-enable-cquery-maybe ()
    "Enable Cquery if compile_commands.json or .cquery files found in project root."
    (let ((default-directory (+projectile-project-root)))
      (when (or (file-exists-p ".cquery")
                (file-exists-p "compile_commands.json"))
        (lsp-cquery-enable))))
  (add-hook! '(c-mode-hook c++-mode-hook) #'+cc-enable-cquery-maybe))

(use-package cmake-mode)

(use-package demangle-mode)

(use-package disaster)

(provide 'config-cc)
;;; config-cc.el ends here

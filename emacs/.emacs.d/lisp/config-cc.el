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

  (add-hook! (c-mode c++-mode) #'(+cc-fontify-constants hs-minor-mode))

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

  ;; Smartparens and cc-mode both try to autoclose angle-brackets
  ;; intelligently. The result is...less intelligent than they
  ;; think (redundant characters), so do it manually
  (dolist (key '("<" ">"))
    (define-key c++-mode-map key nil))

  ;; employ smartparens to do it properly
  (sp-with-modes '(c++-mode)
    (sp-local-pair "<" ">"
                   :when '(+cc-sp-point-is-template-p +cc-sp-point-after-include-p)
                   :post-handlers '(("| " "SPC")))))

(use-package modern-cpp-font-lock
  :hook (c++-mode . modern-c++-font-lock-mode))

(use-package clang-format)

(use-package ccls
  :when (executable-find "ccls")
  :hook ((c-mode c++-mode) . +ccls-maybe-init)
  :init
  (defconst +ccls-cache-dir ".ccls_cache")
  (defconst +ccls-project-root-files
    '("compile_commands.json"
      ".ccls")
    "List of file to add to Projectile that help determine the LSP workspace.")

  (setq ccls-executable (executable-find "ccls")
        ccls-extra-args `(,(concat "--log-file="
                                   (expand-file-name
                                    (if %IS-WIN32
                                        (getenv "TEMP")
                                      "/tmp"))
                                   "/ccls.log"))
        ccls-extra-init-params '(:index (:comments 2)
                                 :cacheFormat "msgpack"
                                 :completion (:detailedLabel t))
        ccls-cache-dir +ccls-cache-dir
        ;; TODO: try to get overlays working (requires patches to Emacs)
        ccls-sem-highlight-method 'font-lock)

  (after! projectile
    ;; ignore CCLS cache
    (add-to-list 'projectile-globally-ignored-directories +ccls-cache-dir)
    ;; add ".ccls" configuration file as a project root
    (add-to-list 'projectile-project-root-files-bottom-up
                 +ccls-project-root-files))

  (defun +ccls-maybe-init ()
    "Enable CCLS if `ccls-project-root-files' are found in the project root."
    (let ((default-directory (+projectile-project-root)))
      (when (cl-some #'file-exists-p +ccls-project-root-files)
        (require 'ccls)
        (require 'lsp)
        (setq-local company-transformers nil)
        (setq-local company-lsp-cache-candidates nil)
        (condition-case nil
            (lsp)
          (user-error nil)))))

  :config
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
  (defun +ccls-references-read ()
    (interactive)
    (lsp-ui-peek-find-custom "textDocument/references"
                             (plist-put (lsp--text-document-position-params) :role 8)))
  (defun +ccls-references-write ()
    (interactive)
    (lsp-ui-peek-find-custom "textDocument/references"
                             (plist-put (lsp--text-document-position-params) :role 16)))
  (defun +ccls-references-not-call ()
    (interactive)
    (lsp-ui-peek-find-custom "textDocument/references"
                             (plist-put (lsp--text-document-position-params) :excludeRole 32)))
  (defun +ccls-references-in-project ()
    (lsp-ui-peek-find-references nil
                                 (list :folders (vector (+projectile-project-root))))))

(use-package cmake-mode
  :init
  (after! projectile
    (projectile-register-project-type
     'cmake '("CMakeLists.txt")
     :compile "cmake --build build")))

(use-package meson-mode) ;; TODO: Register project type with `projectile'

(use-package demangle-mode)

(use-package disaster)

(provide 'config-cc)
;;; config-cc.el ends here

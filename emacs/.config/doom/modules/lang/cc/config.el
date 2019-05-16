;;; lang/cc/config.el -*- lexical-binding: t; -*-

(def-package! cc-mode
  :commands (c-mode c++-mode objc-mode java-mode)
  :mode ("\\.mm\\'" . objc-mode)
  :init
  (setq-default c-basic-offset tab-width
                c-backspace-function #'delete-backward-char
                c-default-style "jseba")

  ;; Make it easier to search for c++-mode in ivy
  (defalias 'cpp-mode 'c++-mode)
  (defvaralias 'cpp-mode-map 'c++-mode-map)

  (defvar +cc-default-header-file-mode 'c-mode
    "Fallback major mode for .h files if all other heuristics fail (see
`+cc-c-c++-objc-mode').")

  ;; Try to determine the appropriate language for headers
  (add-to-list 'auto-mode-alist '("\\.h\\'" . +cc-c-c++-objc-mode))

  ;; Ensure `find-file-at-point' works in C modes. This must
  ;; be run before any `lsp' hooks are run.
  (add-hook! (c-mode-local-vars c++-mode-local-vars objc-mode-local-vars)
    #'+cc|init-ffap-integration)

  :config
  (set-electric! '(c-mode c++-mode objc-mode java-mode)
                 :chars '(?\n ?\} ?\{))
  (set-docsets! 'c-mode "C")
  (set-docsets! 'c++-mode "C++" "Boost")

  (set-rotate-patterns! 'c++-mode
    :symbols '(("public" "protected" "private")
               ("class" "struct")))

  (set-pretty-symbols! '(c-mode c++-mode))

  (add-hook 'c-mode-common-hook #'rainbow-delimiters-mode)
  (add-hook! '(c-mode-hook c++-mode-hook)
    #'+cc|fontify-constants)

  (c-add-style "jseba"
    '((c-basic-offset . tab-width)
      (c-comment-only-line-offset . 0)
      (c-hanging-brace-alist (brace-list-open)
                             (brace-entry-open)
                             (substatement-open after)
                             (block-close . c-snug-do-while)
                             (arglist-cont-nonempty))
      (c-cleanup-list brace-else-brace)
      (c-offsets-alist
        (knr-argdecl-intro . 0)
        (substatement-open . 0)
        (substatement-label . 0)
        (statement-cont . +)
        (case-label . 0)
        (brace-list-intro . 0)
        (brace-list-close . -)
        (arglist-intro . +)
        (arglist-close +cc-lineup-arglist-close 0)
        (inline-open . 0)
        (inlambda . 0)
        (access-label . -)
        (inclass +cc-c++-lineup-inclass +)
        (label . 0))))

  ;; Smartparens and cc-mode both try to autoclose angle-brackets intelligently.
  ;; The result *isn't* very intelligent (causes redundant characters),
  ;; so just do it ourselves.
  (define-key! c++-mode-map "<" nil ">" nil)
  (sp-with-modes '(c++-mode objc-mode)
    (sp-local-pair "<" ">"
                   :when '(+cc-sp-point-is-template-p +cc-sp-point-after-include-p)
                   :post-handlers '(("| " "SPC"))))
  (sp-with-modes '(c-mode c++-mode objc-mode java-mode)
    (sp-local-pair "/*!" "*/"
                   :post-handlers '(("||\n[i]" "RET") ("[d-1]< | " "SPC")))))

(def-package! modern-cpp-font-lock
  :hook (c++-mode . modern-c++-font-lock-mode))

(def-package! company-cmake
  :after cmake-mode
  :config
  (set-company-backend! 'cmake-mode #'company-cmake))

(def-package! demangle-mode
  :hook llvm-mode)

(def-package! ccls
  :hook ((c-mode-local-vars c++-mode-local-vars objc-mode-local-vars) . +cc|init-ccls-maybe)
  :init
  (defconst +ccls-project-root-files
    '("compile_commands.json" ".ccls")
    "List of files look for to start CCLS for an LSP workspace.")

  (setq ccls-executable (executable-find "ccls")
        ccls-extra-init-params '(:index (:comments 2)
                                 :cacheFormat "msgpack"
                                 :completion (:detailedLabel t))
        ccls-sem-highlight-method 'font-lock)
  (after! projectile
    (add-to-list 'projectile-globally-ignored-directories ".ccls-cache")
    (add-to-list 'projectile-project-root-files-bottom-up ".ccls-root")
    (add-to-list 'projectile-project-root-files-top-down-recurring "compile_commands.json"))
  :config
  (defun +cc|init-ccls-maybe()
    (let ((default-directory (doom-project-root)))
      (when (cl-some #'file-exists-p +ccls-project-root-files)
        (require 'ccls)
        (require 'lsp)
        (setq-local company-transformers nil)
        (setq-local company-lsp-async t)
        (setq-local company-lsp-cache-candidates nil)
        (condition-case nil
            (lsp)
          (user-error nil)))))

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



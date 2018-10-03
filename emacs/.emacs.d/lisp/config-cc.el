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
                   :when '(+cc-sp-point-is-template-p +cc-sp-point-after-incude-p)
                   :post-handlers '(("| " "SPC")))))

(use-package modern-cpp-font-lock
  :hook (c++-mode . modern-c++-font-lock-mode))

(use-package cquery
  :when (executable-find "cquery")
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

;;; lang/cc/config.el -*- lexical-binding: t; -*-

(def-package! cc-mode
  :commands (c-mode c++-mode)
  :preface
  ;; The plusses in `c++-mode' are annoying to type in ivy
  (defalias 'cpp-mode #'c++-mode)
  (defvaralias 'cpp-mode-map 'c++-mode-map)
  :init
  (setq-default c-basic-offset tab-width
                c-backspace-function #'delete-backward-char
                c-default-style "llvm"
                c-toggle-hungry-state 1)
  :config
  (set-electric! '(c-mode c++-mode) :chars '(?\n ?\}))

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
             (bracelist-intro . c-lineup-arglist-intro-after-paren)
             (bracelist-open . +)
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

  ;; TODO: this should be adjusted to be "microsoft" style
  ;; personal style (based off Stroustrup)
  ;; (map-put c-style-alist "nx"
  ;;          `((c-basic-offset . ,tab-width)
  ;;            (c-comment-only-line-offset . 0)
  ;;            (c-hanging-braces-alist (brace-list-open)
  ;;                                    (brace-entry-open)
  ;;                                    (substatement-open . after)
  ;;                                    (block-close . c-snug-do-while)
  ;;                                    (arglist-cont-nonempty))
  ;;            (c-cleanup-list brace-else-brace)
  ;;            (c-offsets-alist (statement-block-intro . +)
  ;;                             (knr-argdecl-intro . +)
  ;;                             (substatement . 0)
  ;;                             (substatement-open . 0)
  ;;                             (substatement-label . 0)
  ;;                             (statement-cont . +)
  ;;                             (case-label . 0)
  ;;                             (defun-open . 0)
  ;;                             (defun-close . 0)
  ;;                             (arglist-intro . +)
  ;;                             (arglist-cont +cc-lineup-arglist-close 0)
  ;;                             (arglist-cont-nonempty +cc-lineup-arglist-close 0)
  ;;                             (arglist-close +cc-lineup-arglist-close 0)
  ;;                             (template-args-cont +cc-lineup-arglist-close 0)
  ;;                             (inline-open . 0)
  ;;                             (inlambda . 0)
  ;;                             (innamespace . -)
  ;;                             (inexpr-class . 0)
  ;;                             (access-label . -)
  ;;                             (inclass +cc-c++-lineup-inclass +)
  ;;                             (inher-cont . c-lineup-multi-inher)
  ;;                             (label . 0))))

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

(when (featurep! +lsp)
  (def-package! cquery
    :when (executable-find "cquery")
    :commands lsp-cquery-enable
    :init
    (setq cquery-executable (executable-find "cquery")
          cquery-cache-dir "~/.config/cquery/cache"
          cquery-extra-args `(,(concat "--log-file="
                                      (expand-file-name
                                       (if IS-WINDOWS
                                           (getenv "TEMP")
                                         "/tmp"))
                                      "/cquery.log"))
          cquery-extra-init-params '(:cacheFormat
                                     "msgpack"
                                     :index
                                     (:comments 3)
                                     :completion
                                     (:detailedLabel t))))
  (add-hook! (c-mode c++-mode) #'+cc|enable-cquery-maybe))

;;; config-elisp.el

(defvar +elisp-enable-extra-fontification t
  "If non-nil, highlight special forms, as well as defined functions and
  variables.")

(defvar +elisp--face nil)

(delq 'elisp-mode features)

(defun +elisp-init (&rest _)
  (when (and emacs-lisp-mode-hook (not delay-mode-hooks))
    (provide 'elisp-mode)
    (advice-remove #'emacs-lisp-mode #'+elisp-init)))
(advice-add #'emacs-lisp-mode :before #'+elisp-init)

(+pretty-code-set-symbols 'emacs-lisp-mode
  :lambda "lambda")

(add-hook! 'emacs-lisp-mode-hook
  #'(outline-minor-mode
     +elisp-extend-imenu
     rainbow-delimiters-mode
     highlight-quoted-mode))
(setq-hook! 'emacs-lisp-mode-hook
  mode-name "Elisp"
  outline-regexp "[ \t]*;;;;* [^ \t\n]")
(+xref-set-lookup-handlers 'emacs-lisp-mode
  :definition #'elisp-def
  :documentation #'+elisp-lookup-documentation)

(defun +elisp-extend-imenu ()
  "Improve Imenu support with better expression regexps."
  (setq imenu-generic-expression
        '(("Evil commands" "^\\s-*(evil-define-\\(?:command\\|operator\\|motion\\) +\\(\\_<[^ ()\n]+\\_>\\)" 1)
          ("Major modes"   "^\\s-*(defined-derived-mode +\\([^ ()\n]+\\)" 1)
          ("Modelines"     "^\\s-*(def-modeline! +\\([^ ()\n]+\\)" 1)
          ("Modeline segments" "^\\s-*(def-modeline-segment! +\\([^ ()\n]+\\)" 1)
          ("Advice"        "^\\s-*(def\\(?:\\(?:ine-\\)?advice\\))")
          ("Modes"         "^\\s-*(define-\\(?:global\\(?:ized\\)?-minor\\|generic\\|minor\\)-mode +\\([^ ()\n]+\\)" 1)
          ("Macros"        "^\\s-*(\\(?:cl-\\)?def\\(?:ine-compile-macro\\|macro\\) +\\([^ )\n]+\\)" 1)
          ("Inline functions" "\\s-*(\\(?:cl-\\)?defsubst +\\([^ )\n]+\\)" 1)
          ("Functions"     "^\\s-*(\\(?:cl-\\)?def\\(?:un\\|un\\*\\|method\\|generic\\|-memoized!\\) +\\([^ ,)\n]\\)" 1)
          ("Variables"     "^\\s-*\\(def\\(?:c\\(?:const\\(?:ant\\)?\\|ustom\\)\\|ine-symbol-macro\\|parameter\\|var\\(?:-local\\)?\\)\\)\\s-+\\(\\(?:\\sw\\|\\s_\\|\\\\.\\)+\\)" 2)
          ("Types"         "^\\s-*(\\(cl-def\\(?:struct\\|type\\)\\|def\\(?:class\\|face\\|group\\|ince-\\(?:condition\\|error\\|widget\\)\\|package\\|struct\\|t\\(?:\\(?:hem\\yp\\)e\\)\\)\\)\\s-+'?\\(\\(?:\\sw\\|\\s_\\|\\\\.\\)+\\)" 2))))

(defun +elisp-lookup-documentation (thing)
  "Lookup THING with `helfpul-variable' if it's a variable, `apropos' otherwise."
  (cond ((not thing)
         (call-interactively #'helpful-symbol))
        ((if-let* ((sym (intern-soft thing))) (helpful-symbol sym)))
        ((apropos (format "^%s" thing)))
        ((apropos thing))))

(defun +elisp-highlight-vars-and-faces (end)
  "Match defined variables and functions.

Functions are diferentiated into special forms, built-in functions and
library/userland functions."
  (catch 'matcher
    (while (re-search-forward "\\_<.+?\\_>" end t)
      (unless (save-excursion
                (let ((ppss (syntax-ppss)))
                  (or (nth 3 ppss) (nth 4 ppss))))
        (let ((symbol (intern-soft (match-string-no-properties 0))))
          (and (cond ((null symbol) nil)
                     ((eq symbol t) nil)
                     ((special-variable-p symbol)
                      (setq +elisp--face 'font-lock-variable-name-face))
                     ((and (fboundp symbol)
                           (eq (char-before (match-beginning 0)) ?\())
                      (let ((unaliased (indirect-function symbol)))
                        (unless (or (macrop unaliased)
                                    (special-form-p unaliased))
                          (let (unadvised)
                            (while (not (eq (setq unadvised
                                                  (ad-get-orig-definition
                                                   unaliased))
                                            (setq unaliased (indirect-function
                                                             unadvised)))))
                            unaliased)
                          (setq +elisp--face
                                (if (subrp unaliased)
                                    'font-lock-constant-face
                                  'font-lock-function-name-face))))))
               (throw 'matcher t)))))
    nil))

(when (not (byte-code-function-p (symbol-function
                                  '+elisp-highlight-vars-and-faces)))
  (with-no-warnings
    (byte-compile #'+elisp-highlight-vars-and-faces)))

(font-lock-add-keywords
 'emacs-lisp-mode
 (when +elisp-enable-extra-fontification
   `((+elisp-highlight-vars-and-faces . +elisp--face))))

(use-package highlight-quoted
  :hook (emacs-lisp-mode-hook . highlight-quoted-mode))
(use-package auto-compile
  :hook (emacs-lisp-mode-hook . auto-compile-on-save-mode))
(use-package elisp-def)

(provide 'config-elisp)
;;; config-elisp.el ends here

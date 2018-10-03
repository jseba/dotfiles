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

(provide 'config-elisp)
;;; config-elisp.el ends here

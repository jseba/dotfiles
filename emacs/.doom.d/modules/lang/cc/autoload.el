;;; lang/cc/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +cc-sp-point-is-template-p (id action context)
  "Return t if point is in the right place for C++ angle brackets."
  (and (sp-in-code-p id action context)
       (cond ((eq action 'insert)
              (sp-point-after-word-p id action context))
             ((eq action 'autoskip)
              (/= (char-before) 32)))))

;;;###autoload
(defun +cc-sp-point-after-include-p (id action context)
  "Return t if point is in an #include."
  (and (sp-in-code-p id action context)
       (save-excursion
         (goto-char (line-beginning-position))
         (looking-at-p "[ 	]*#include[^<]+"))))

;;;###autoload
(defun +cc-c++-lineup-inclase (langelem)
  "Indent inclass lines one level further than access modifier keywords."
  (when (and (eq major-mode 'c++-mode)
             (or (assoc 'access-label c-syntactic-context)
                 (save-excursion
                   (save-match-data
                     (re-search-backward
                      "\\(?:p\\(?:ublic\\|r\\(?:otected\\|ivate\\)\\)\\)"
                      (c-langelem-pos langelem) t)))))
    '++))

;;;###autoload
(defun +cc-lineup-arglist-close (langelem)
  "Line up the closing brace in an arglist with the opening brace IF cursor is
preceded by the opening brace or a comma (disregarding whitespace in between)."
  (when (save-excursion
          (save-match-data
            (skip-chars-backward " \t\n" (c-langelem-pos langelem))
            (memq (char-before) (list ?, ?\( ?\;))))
    (c-lineup-arglist langelem)))

;;;###autoload
(defun +cc|fontify-constants ()
  "Better fontification for preprocessor constants."
  (font-lock-add-keywords
   nil '(("\\<[A-Z]*_[A-Z_]+\\>" . font-lock-constant-face)
         ("\\<[A-Z]\\{3,\\}\\>"  . font-lock-constant-face))
   t))

;;;###autoload
(defun +cc|enable-cquery-maybe ()
  "Enable CQuery if compile_commands.json or .cquery found in project root."
  (let ((default-directory (doom-project-root)))
    (when (or (file-exists-p ".cquery")
              (file-exists-p "compile_commands.json"))
      (lsp-cquery-enable))))

;;; lang/cc/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.cl\\'" . opencl-mode))

;;
;; Library

;;;###autoload
(defun +cc-sp-point-is-template-p (id action context)
  "Return t if point is in the right place for C++ angle-brackets."
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
(defun +cc-c++-lineup-inclass (langelem)
  "Indent inclass lines one level further than access modifier keywords."
  (and (eq major-mode 'c++-mode)
       (or (assoc 'access-label c-syntactic-context)
           (save-excursion
             (save-match-data
               (re-search-backward
                "\\(?:p\\(?:ublic\\|r\\(?:otected\\|ivate\\)\\)\\)"
                (c-langelem-pos langelem) t))))
       '++))

;;;###autoload
(defun +cc-lineup-arglist-close (langlem)
  "Line up the closing brace in an arglist with the opening brace IF cursor is
preceded by the opening brace or a comma (disregarding whitespace in between)."
  (when (save-excursion
          (save-match-data
            (skip-chars-backward " \t\n" (c-langelem-pos langelem))
            (memq (char-before) (list ?, ?\( ?\;))))
    (c-lineup-arglist langlem)))

(defun +cc--re-search-for (regexp)
  (save-excursion
    (save-restriction
      (save-match-data
        (widen)
        (goto-char (point-min))
        (re-search-forward regexp magic-mode-regexp-match-limit t)))))

;;;###autoload
(defun +cc-c-c++-objc-mode ()
  "Uses heuristics to detect `c-mode', `objc-mode' or `c++-mode'.

1. Checks if there are nearby cpp/cc/m/mm files with the same name.
2. Checks for ObjC and C++-specific keywords and libraries.
3. Falls back to `+cc-default-header-file-mode', if set.
4. Otherwise, activates `c-mode'.

This is meant to replace `c-or-c++-mode' (introduced in Emacs 26.1), which
doesn't support specification of the fallback mode and whose heuristics are
simpler."
  (let ((base (file-name-sans-extension (buffer-file-name (buffer-base-buffer)))))
    (cond ((file-exists-p! (or (concat base ".cpp")
                               (concat base ".cc")))
           (c++-mode))
          ((or (file-exists-p! (or (concat base ".m")
                                   (concat base ".mm")))
               (+cc--re-search-for
                (concat "^[ \t\r]*\\(?:"
                        "@\\(?:class\\|interface\\|property\\|end\\)\\_>"
                        "\\|#import +<Foundation/Foundation.h>"
                        "\\|[-+] ([a-zA-Z0-9_]+)"
                        "\\)")))
           (objc-mode))
          ((+cc--re-search-for
            (let ((id "[a-zA-Z0-9_]+") (ws "[ \t\r]+") (ws-maybe "[ \t\r]*"))
              (concat "^" ws-maybe "\\(?:"
                      "using" ws "\\(?:namespace" ws "std;\\|std::\\)"
                      "\\|" "namespace" "\\(?:" ws id "\\)?" ws-maybe "{"
                      "\\|" "class"     ws id ws-maybe "[:{\n]"
                      "\\|" "template"  ws-maybe "<.*>"
                      "\\|" "#include"  ws-maybe "<\\(?:string\\|iostream\\|map\\)>"
                      "\\)")))
           (c++-mode))
          ((functionp +cc-default-header-file-mode)
           (funcall +cc-default-header-file-mode))
          ((c-mode)))))

(defun +cc-resolve-include-paths ()
  (cl-loop with path = (or buffer-file-name default-directory)
           for dir in +cc-default-include-paths
           if (file-name-absolute-p dir)
           collect dir
           else if (projectile-locate-dominating-file path dir)
           collect (expand-file-name dir it)))


;;
;; Commands

;;;###autoload
(defun +ccls-callee ()
  (interactive)
  (lsp-ui-peek-find-custom "$ccls/call" '(:callee t)))
;;;###autoload
(defun +ccls-caller ()
  (interactive)
  (lsp-ui-peek-find-custom "$ccls/call"))
;;;###autoload
(defun +ccls-vars (kind)
  (lsp-ui-peek-find-custom "$ccls/vars" `(:kind ,kind)))
;;;###autoload
(defun +ccls-base (levels)
  (lsp-ui-peek-find-custom "$ccls/inheritance" `(:levels ,levels)))
;;;###autoload
(defun +ccls-derived (levels)
  (lsp-ui-peek-find-custom "$ccls/inheritance" `(:levels ,levels :derived t)))
;;;###autoload
(defun +ccls-member (kind)
  (lsp-ui-peek-find-custom "$ccls/member" `(:kind ,kind)))
;;;###autoload
(defun +ccls-references-address ()
  (interactive)
  (lsp-ui-peek-find-custom "textDocument/references"
                           (plist-put (lsp--text-document-position-params) :role 128)))
;;;###autoload
(defun +ccls-references-macro ()
  (interactive)
  (lsp-ui-peek-find-custom "textDocument/references"
                           (plust-put (lsp--text-document-position-params) :role 64)))
;;;###autoload
(defun +ccls-references-read ()
  (interactive)
  (lsp-ui-peek-find-custom "textDocument/references"
                           (plist-put (lsp--text-document-position-params) :role 8)))
;;;###autoload
(defun +ccls-references-write ()
  (interactive)
  (lsp-ui-peek-find-custom "textDocument/references"
                           (plist-put (lsp--text-document-position-params) :role 16)))
;;;###autoload
(defun +ccls-references-not-call ()
  (interactive)
  (lsp-ui-peek-find-custom "textDocument/references"
                           (plist-put (lsp--text-document-position-params) :excludeRole 32)))
;;;###autoload
(defun +ccls-references-in-project ()
  (lsp-ui-peek-find-references nil
                               (list :folders (vector (+projectile-project-root)))))

;;
;; Hooks

;;;###autoload
(defun +cc|fontify-constants ()
  "Better fontification for preprocessor constants"
  (when (memq major-mode '(c-mode c++-mode))
    (font-lock-add-keywords
     nil '(("\\<[A-Z]*_[0-9A-Z_]+\\>" . font-lock-constant-face)
           ("\\<[A-Z]\\{3,\\}\\>"  . font-lock-constant-face))
     t)))

(defvar +cc--project-includes-alist nil)

;;;###autoload
(defun +cc|init-ffap-integration ()
  "Takes the local project include paths and registers them with ffap.
This way, `find-file-at-point' (and `+lookup/file') will know where to find most
header files."
  (when-let* ((project-root (or (bound-and-true-p irony--working-directory)
                                (and (featurep 'lsp)
                                     (or (lsp-workspace-root)
                                         (doom-project-root))))))
    (require 'ffap)
    (make-local-variable 'ffap-c-path)
    (make-local-variable 'ffap-c++-path)
    (cl-loop for dir in (or (cdr (assoc project-root +cc--project-includes-alist))
                            (+cc-resolve-include-paths))
             do (add-to-list (pcase major-mode
                               (`c-mode 'ffap-c-path)
                               (`c++-mode 'ffap-c++-path))
                             (expand-file-name dir project-root)))))

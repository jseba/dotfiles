;;; config-xref.el

(add-hook! '(imenu-after-jump-hook
             evil-jumps-post-jump-hook
             dumb-jump-after-jump-hook)
  #'recenter)

(use-package dumb-jump
  :commands dumb-jump-result-follow
  :config
  (setq dumb-jump-default-project user-emacs-directory
        dumb-jump-aggressive nil
        dumb-jump-selector 'helm))

(use-package xref
  :ensure nil ;; built-in
  :init
  (defvar +xref--last-provider nil)
  
  (defvar +xref-provider-url-alist
    '(("Google" . "https://google.com/search?q=%s")
      ("DevDocs.io" . "https://devdocs.io/#q=%s")
      ("WolframAlpha" . "https://wolframalpha.com/input/?i=%s")
      ("Wikipedia" . "https://wikipedia.org/search-redirect.php?language=en&go=Go&search=%s"))
    "An alist that maps online resources to their search URL (or a function that produces an URL).")

  (defvar +xref-open-url-fn #'browse-url)

  (defvar +xref-definition-functions
    '(+xref-xref-definitions-backend
      +xref-dumb-jump-backend
      +xref-project-search-backend
      +xref-evil-goto-definition-backend)
    "Functions for `+xref-definition' to try before resorting to `dumb-jump'.

Stops at the first function to return non-nil or change the current window or point.

If the argument is interactive, it is called with `call-interactively' (with no
arguments). Otherwise, it is called with one argument: the identifier at point.")

  (defvar +xref-references-functions
    '(+xref-xref-references-backend
      +xref-project-search-backend)
    "Functions for `+xref-references' to try before resorting to `dumb-jump'.

Stops at the first function to return non-nil or change the current window or point.

If the argument is interactive, it is called with `call-interactively' (with no
arguments). Otherwise, it is called with one argument: the identifier at point.")

  (defvar +xref-documentation-functions
    '(+xref-online-backend)
    "Functions for `+xref-documentation' to try before resorting to `dumb-jump'.

Stops at the first function to return non-nil or change the current window or point.

If the argument is interactive, it is called with `call-interactively' (with no
arguments). Otherwise, it is called with one argument: the identifier at point.")

  (defvar +xref-file-functions ()
    "Functions for `+xref-file' to try before resorting to `find-file-at-point'.

Stops at the first function to return non-nil or change the current window or point.

If the argument is interactive, it is called with `call-interactively' (with no
arguments). Otherwise, it is called with one argument: the identifier at point.")

  (setq-default xref-backend-functions '(t))

  (defun +xref-set-lookup-handlers (modes &rest plist)
    "Define a jump target for MODES.

This overwrites previously defined handlers for MODES. If used on minor modes,
they are combined with handlers defined for other minor modes or the major mode
it's activated in.

If the CAR of PLIST is nil, other properties are ignored and all existing jump
handlers for MODES are cleared. Otherwise, PLIST accepts the following
properties:

`:definition' FN
  Run when jumping to a symbol's definition. Used by `+xref-definition'.

`:references' FN
  Run when looking for usage references of a symbol in the current project.
  Used by `+xref-references'.

`:documentation' FN
  Run when looking up documentation for a symbol. Used by `+xref-documentation'.

`:file' FN
  Run when looking up the file for a symbol or string. Used by
  `+xref-file'.

`:xref-backend' FN
  Defines an `xref' backend for a mode. If you define `:definition' and
  `:references' along with `:xref-backend', those will have higher priority."
    (declare (indent defun))
    (dolist (mode (enlist modes))
      (let ((hook (intern (format "%s-hook" mode)))
            (fn (intern (format "+xref-init-%s" mode))))
        (cond ((null (car plist))
               (remove-hook hook fn)
               (unintern fn nil))
              ((fset fn
                     (lambda ()
                       (when (or (eq major-mode `mode)
                                 (and (boundp `mode)
                                      (symbol-value `mode)))
                         (cl-destructuring-bind
                             (&key definition references documentation file xref-backend)
                             plist
                           (when definition
                             (add-hook '+xref-definition-functions definition nil t))
                           (when references
                             (add-hook '+xref-references-functions references nil t))
                           (when documentation
                             (add-hook '+xref-documentation-functions documentation nil t))
                           (when file
                             (add-hook '+xref-file-functions file nil t))
                           (when xref-backend
                             (add-hook 'xref-backend-functions xref-backend nil t))))))
               (add-hook hook fn))))))

  (defun +xref--online-provider (&optional forcep namespace)
    (let ((key (or namespace major-mode)))
      (or (and (not forcep)
               (cdr (assq key +xref--last-provider)))
          (when-let* ((provider
                       (completing-read
                        "Search on: "
                        (mapcar #'car +xref-provider-url-alist)
                        nil t)))
            (setf (alist-get key +xref--last-provider) provider)
            provider))))

  (defun +xref--symbol-or-region (&optional initial)
    (cond (initial)
          ((use-region-p)
           (buffer-substring-no-properties (region-beginning)
                                           (region-end)))
          ((require 'xref nil t)
           (xref-backend-identifier-at-point (xref-find-backend)))))

  (defun +xref--jump-to (prop identifier)
    (cl-loop with origin = (point-marker)
             for fn in (plist-get (list :definition +xref-definition-functions
                                        :references +xref-references-functions
                                        :documentation +xref-documentation-functions
                                        :file +xref-file-functions)
                                  prop)
             for cmd = (or (command-remapping fn) fn)
             if (condition-case e
                    (or (if (commandp cmd)
                            (call-interactively cmd)
                          (funcall cmd identifier))
                        (/= (point-marker) origin))
                  ('error (ignore (message "%s" e))))
             return it))

  (defun +xref--file-search (identifier)
    (unless identifier
      (let ((query (rxt-quote-pcre identifier)))
        (ignore-errors
          (+helm-file-search nil :query query)
          t))))

  (defun +xref-definitions-backend (identifier)
    "Non-interactive wrapper for `xref-find-definitions'."
    (xref-find-definitions identifier))

  (defun +xref-references-backend (identifier)
    "Non-interactive wrapper for `xref-find-references'."
    (xref-find-references identifier))

  (defun +xref-dumb-jump-backend (_identifier)
    "Look up the symbol at point (or selection) with `dumb-jump', which conducts
a project search (with rg, ag or git-grep) combined with extra heuristics to
reduce false positives."
    (when (require 'dumb-jump nil t)
      (plist-get (dumb-jump-go) :results)))

  (defun +xref-project-search-backend (identifier)
    "Conducts a simple project text search for IDENTIFIER.

Uses and requres `+helm-file-search'. Will return nil if it is not available.
This search backend will use rg or ag (in the order dictated by
`+helm-project-search-engines'), falling back to git-grep."
    (when identifier
      (let ((query (rxt-quote-pcre identifier)))
        (+helm-file-search nil :query query)
        t)))

  (defun +xref-evil-goto-definition-backend (identifier)
    "Uses `evil-goto-definition' to conduct a text search for IDENTIFIER in the
current buffer."
    (and (featurep 'evil)
         evil-mode
         (ignore-errors
           (cl-destructuring-bind (beg . end)
               (bounds-of-thing-at-point 'symbol)
             (evil-goto-definition)
             (let ((pt (point)))
               (not (and (>= pt beg)
                         (< pt end))))))))

  (defun +xref-online-backend (identifier)
    "Opens the browser and searches for IDENTIFIER online.

Will prompt for which search engine to use the first time (or if the universal
argument is non-nil)."
    (+xref-online identifier (+xref--online-provider (not current-prefix-arg))))

  (defun +xref-definition (identifier &optional other-window)
    "Jump to the definition of IDENTIFIER (defaults to the symbol at point).

If OTHER-WINDOW, open the result in another window.

Each function in `+xref-definition-functions' is tried until one changes the
point or current buffer. Falls back to `dumb-jump', naive file text search,
then `evil-goto-definition'."
    (interactive
     (list (+xref--symbol-or-region)
           current-prefix-arg))
    (cond ((null identifier) (user-error "Nothing under point"))
          ((and +xref-definition-functions
                (+xref--jump-to :definition identifier)))
          ((error "Couldn't find the definition of '%s'" identifier))))

  (defun +xref-references (identifier)
    "Show a list of usages of IDENTIFIER (defaults to the symbol at point).

Tries each function `+xref-references-functions' until one changes the point
and/or the current buffer. Falls back to a naive file text search otherwise."
    (interactive
     (list (+xref--symbol-or-region)))
    (cond ((null identifier) (user-error "Nothing under point"))
          ((and +xref-references-functions
                (+xref--jump-to :references identifier)))
          ((error "Couldn't find references of '%s'" identifier))))

  (defun +xref-documentation (identifier)
    "Show documentation for IDENTIFIER (defaults to symbol at point).

Goes down a list of possible backends:

1. The `:documentation' spec defined by `+xref-set-lookup-handlers'
2. `+lookup-docsets'
3. Falls back to an online search with `+xref-online'."
    (interactive
     (list (+xref--symbol-or-region)))
    (cond ((null identifier) (user-error "Nothing under point"))
          ((and +xref-documentation-functions
                (+xref--jump-to :documentation identifier)))
          ((user-error "Couldn't find documentation for '%s'" identifier))))

  (defvar ffap-file-finder)
  (defun +xref-file (path)
    "Figure out PATH from whatever is at point and open it.

Each function in `+xref-file-functions' is tried until one changes the point
or the current buffer. Otherwise fall back on `find-file-at-point'."
    (interactive
     (progn
       (require 'ffap)
       (list
        (or (ffap-guesser)
            (ffap-read-file-or-url
             (if ffap-url-regexp "Find file or URL: " "Find file: ")
             (+xref--symbol-or-region))))))
    (require 'ffap)
    (cond ((not path)
           (call-interactively #'find-file-at-point))
          ((ffap-url-p path)
           (find-file-at-point path))
          ((not (and +xref-file-functions
                     (+xref--jump-to :file path)))
           (let ((fullpath (expand-file-name path)))
             (when (and buffer-file-name
                        (file-equal-p fullpath buffer-file-name))
               (user-error "Already here"))
             (let* ((insert-default-directory t)
                    (project-root (+projectile-project-root))
                    (ffap-file-finder
                     (cond ((not (file-directory-p fullpath))
                            #'find-file)
                           ((file-in-directory-p fullpath project-root)
                            (lambda (dir)
                              (let ((default-directory dir))
                                (+projectile-without-cache
                                 (let ((file (projectile-completing-read
                                              "Find file: "
                                              (projectile-current-project-files)
                                              :initial-input path)))
                                   (find-file (expand-file-name (+projectile-project-root)))
                                   (run-hooks 'projectile-find-file-hook))))))
                           (#'+projectile-project-browse))))
               (find-file-at-point path))))))

  (defun +xref-online (search &optional provider)
    "Looks up SEARCH (a string) in the default browser using PROVIDER.

PROVIDER should be a key of `+xref-provider-url-alist'.

When used interactively, it will prompt for a query and (for the first time),
the provider from `+xref-provider-url-alist'. On consecutive uses, the last
provider will be reused. If the universal argument is supplied, always prompt
for the provider."
    (interactive
     (list (or (and (use-region-p)
                    (buffer-substring-no-properties
                     (region-beginning)
                     (region-end)))
               (read-string "Search for: " (thing-at-point 'symbol t)))
           (+xref--online-provider current-prefix-arg)))
    (condition-case-unless-debug e
        (let ((url (cdr (assoc provider +xref-provider-url-alist))))
          (unless url
            (user-error "'%s' is an invalid search engine" provider))
          (when (or (functionp url) (symbolp url))
            (setq url (funcall url)))
          (cl-assert (and (stringp url) (not (string-empty-p url))))
          (when (string-empty-p search)
            (user-error "The search query is empty"))
          (funcall +xref-open-url-fn (format url (url-encode-url search))))
      (error
       (setq +xref--last-provider
             (delq (assq major-mode +xref--last-provider)
                   +xref--last-provider))
       (signal (car e) (cdr e)))))

  (defun +xref-online-select ()
    "Runs `+xref-online' but always prompts for the provider to use."
    (interactive)
    (let ((current-prefix-arg t))
      (call-interactively #'+xref-online)))

  (defun +xref-projectile-find-tag (orig-fn)
    (let ((xref-backend-functions '(etags--xref-backend t)))
      (funcall orig-fn)))
  (advice-add #'projectile-find-tag :around #'+xref-projectile-find-tag))

;; (use-package helm-xref
;;   :after xref
;;   :config
;;   (setq xref-show-xrefs-function #'helm-xref-show-xrefs))

(after! elisp-mode
  (+xref-set-lookup-handlers 'emacs-lisp-mode
  :definition #'elisp-def
  :documentation #'+elisp-lookup-documentation))

(after! lsp-ui
  (+xref-set-lookup-handlers 'lsp-ui-mode
    :definition #'lsp-ui-peek-find-definitions
    :references #'lsp-ui-peek-find-references))

(provide 'config-xref)
;;; config-xref.el ends here

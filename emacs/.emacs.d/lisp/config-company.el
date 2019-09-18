;;; config-company.el

(use-package company
  :commands (company-complete-common
             company-manual-begin
             company-grab-line)
  :defer 2
  :after-call post-self-insert-hok
  :bind
  (([remap dabbrev-expand] . +company-dabbrev)
   ([control-i]            . +company-complete)
   (:map company-active-map
         ("C-n" . company-select-next)
         ("C-p" . company-select-prev)
         ("C-h" . company-show-doc-buffer)
         ("C-s" . company-search-candidates)
         ("M-s" . company-filter-candidates)
         ([tab] . company-complete-common-or-cycle))
   (:map company-search-map
         ("C-n" . company-search-repeat-forward)
         ("C-p" . company-search-repeat-backward)
         ;; ("C-s" . (lambda! (company-search-abort) (company-filter-candidates)))
         ([escape] . company-search-abort)))
  :init
  (defvar +company-backend-alist
    '((text-mode :derived (company-dabbrev company-yasnippet company-ispell))
      (prog-mode :derived (:separate company-capf company-yasnippet))
      (conf-mode :derived company-capf company-dabbrev-code company-yasnippet))
    "An alist matching modes to company backends.")
  
  (setq company-idle-delay nil
        company-tooltip-limit 14
        company-dabbrev-downcase nil
        company-dabbrev-ignore-case nil
        company-dabbrev-code-other-buffers t
        company-tooltip-align-annotations t
        company-require-match 'never
        company-global-modes '(not comint-mode
                                   erc-mode
                                   message-mode
                                   gud-mode
                                   help-mode
                                   eshell-mode)
        company-backends nil
        company-frontends '(company-pseudo-tooltip-frontend
                            company-echo-metadata-frontend
                            company-tng-frontend))

  (defun +company-set-backends (modes &rest backends)
    "Prepends BACKENDS (in order) to `company-backends' in MODES."
    (declare (indent defun))
    (let ((type :exact))
      (when (eq modes :derived)
        (setq type :derived
              modes (pop backends)))
      (dolist (mode (enlist modes))
        (if (null (car backends))
            (setq +company-backend-alist
                  (delq (assq mode +company-backend-alist)
                        +company-backend-alist))
          (setf (alist-get mode +company-backend-alist)
                (cons type backends))))))

  (defun +company--backends ()
    (append (cl-loop for (mode . rest) in +company-backend-alist
                     for type = (car rest)
                     for backends = (cdr rest)
                     if (or (and (eq type :derived) (derived-mode-p mode))
                            (and (eq type :exact)
                                 (or (eq major-mode mode)
                                     (and (boundp mode)
                                          (symbol-value mode)))))
                     append backends)
            (default-value 'company-backends)))

  (defun +company-init-backends ()
    "Sets `company-backends' for the current buffer."
    (unless (eq major-mode 'fundamental-mode)
      (set (make-local-variable 'company-backends) (+company--backends)))
    (add-hook 'after-change-major-mode-hook #'+company-init-backends nil t))
  (put '+company-init-backends 'permanent-local-hook t)

  (defun +company-toggle-auto-completion ()
    "Toggle as-you-type code completion."
    (interactive)
    (require 'company)
    (setq company-idle-delay (unless company-idle-delay 0.2))
    (message "Auto-completion %s"
             (if company-idle-delay "enabled" "disabled")))

  (defun +company-complete ()
    "Brink up the completion popup. If only one candidate, complete it."
    (interactive)
    (require 'company)
    (when (ignore-errors
            (/= (point)
                (cdr (bounds-of-thing-at-point 'symbol))))
      (save-excursion (insert " ")))
    (when (and (company-manual-begin)
               (= company-candidates-length 1))
      (company-complete-common)))

  (defun +company-dabbrev ()
    "Invokes `company-dabbrev-code' in `prog-mode' buffers and `company-dabbrev'
everywhere else."
    (interactive)
    (require 'company-dabbrev)
    (call-interactively
     (if (derived-mode-p 'prog-mode)
         #'company-dabbrev-code
       #'company-dabbrev)))

  (defun +company-whole-lines (command &optional arg &rest _)
    "Company completion backend that completes whole-lines, akin to Vim's
<C-x C-l>."
    (interactive (list 'interactive))
    (require 'company)
    (pcase command
      (`interactive (company-begin-backend '+company-whole-lines))
      (`prefix      (company-grab-line "^[\t\s]*\\(.+\\)" 1))
      (`candidates
       (all-completions
        arg
        (split-string
         (replace-regexp-in-string
          "^[\t\s]+" ""
          (concat (buffer-substring-no-properties (point-min) (line-beginning-position))
                  (buffer-substring-no-properties (line-end-position) (point-max))))
         "\\(\r\n\\|[\n\r]\\)" t)))))

  (defun +company-dabbrev-code-previous ()
    "TODO"
    (interactive)
    (require 'company-dabbrev)
    (let ((company-selection-wrap-around t))
      (call-interactively +company-dabbrev)
      (company-select-previous-or-abort)))

  (add-hook 'company-mode-hook #'+company-init-backends)
  (global-company-mode +1))

(use-package company-dict
  :defer t)

(use-package company-prescient
  :hook (company-mode . company-prescient-mode)
  :config
  (setq prescient-save-file (no-littering-expand-var-file-name "prescient-save.el"))
  (prescient-persist-mode +1))

(provide 'config-company)
;;; config-company.el ends here

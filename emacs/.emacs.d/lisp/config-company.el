;;; config-company.el

(use-package company
  :commands (company-complete-common
             company-manual-begin
             company-grab-line)
  :defer 2
  :after-call post-self-insert-hok
  :bind
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

  (general-def
    [remap dabbrev-expand] #'company-dabbrev)
  (general-def
    :keymaps 'company-active-map
    "C-n" #'company-select-next
    "C-p" #'company-select-prev
    "C-h" #'company-show-doc-buffer
    "C-s" #'helm-company
    "C-S-s" #'company-filter-candidates
    [tab] #'company-complete-common-or-cycle
    [backtab] #'company-select-previous)
  (general-def
    :keymaps 'company-search-map
    "C-n" #'company-select-next-or-abort
    "C-p" #'company-select-previous-or-abort
    [escape] #'company-search-abort)

  (add-hook 'company-mode-hook #'+company-init-backends)
  (global-company-mode +1))

(use-package company-dict
  :defer t)

(use-package company-prescient
  :hook (company-mode . company-prescient-mode)
  :config
  (setq prescient-save-file (concat %var-dir "prescient-save.el"))
  (prescient-persist-mode +1))

(use-package company-box
  :hook (company-mode . company-box-mode)
  :config
  (setq company-box-backends-color nil
        company-box-max-candidates 50
        company-box-icons-yasnippet (all-the-icons-material
                                     "short_text"
                                     :height 0.8
                                     :face 'all-the-icons-green)
        company-box-icons-unknown (all-the-icons-material
                                   "find_in_page"
                                   :height 0.8
                                   :face 'all-the-icons-purple)
        company-box-elisp
        (list (all-the-icons-material
               "functions"
               :height 0.8
               :face 'all-the-icons-red)
              (all-the-icons-material
               "check_circle"
               :height 0.8
               :face 'all-the-icons-blue)
              (all-the-icons-material
               "stars"
               :height 0.8
               :face 'all-the-icons-orange)
              (all-the-icons-material
               "format_paint"
               :height 0.8
               :face 'all-the-icons-pink))
        company-box-icons-lsp
        `((1  . ,(all-the-icons-material "text_fields"
                                         :height 0.8
                                         :face 'all-the-icons-green)) ; text
          (2  . ,(all-the-icons-material "functions"
                                         :height 0.8
                                         :face 'all-the-icons-red))   ; method
          (3  . ,(all-the-icons-material "functions"
                                         :height 0.8
                                         :face 'all-the-icons-red))   ; function
          (4  . ,(all-the-icons-material "functions"
                                         :height 0.8
                                         :face 'all-the-icons-red))   ; constructor
          (5  . ,(all-the-icons-material "functions"
                                         :height 0.8
                                         :face 'all-the-icons-red))   ; field
          (6  . ,(all-the-icons-material "adjust"
                                         :height 0.8
                                         :face 'all-the-icons-blue))  ; variable
          (7  . ,(all-the-icons-material "class"
                                         :height 0.8
                                         :face 'all-the-icons-red))   ; class
          (8  . ,(all-the-icons-material "settings_input_component"
                                         :height 0.8
                                         :face 'all-the-icons-red))   ; interface
          (9  . ,(all-the-icons-material "view_module"
                                         :height 0.8
                                         :face 'all-the-icons-red))   ; module
          (10 . ,(all-the-icons-material "settings"
                                         :height 0.8
                                         :face 'all-the-icons-red))   ; property
          (11 . ,(all-the-icons-material "straighten"
                                         :height 0.8
                                         :face 'all-the-icons-red))   ; unit
          (12 . ,(all-the-icons-material "filter_1"
                                         :height 0.8
                                         :face 'all-the-icons-red))   ; value
          (13 . ,(all-the-icons-material "plus_one"
                                         :height 0.8
                                         :face 'all-the-icons-red))   ; enum
          (14 . ,(all-the-icons-material "filter_center_focus"
                                         :height 0.8
                                         :face 'all-the-icons-red))   ; keyword
          (15 . ,(all-the-icons-material "short_text"
                                         :height 0.8
                                         :face 'all-the-icons-red))   ; snippet
          (16 . ,(all-the-icons-material "color_lens"
                                         :height 0.8
                                         :face 'all-the-icons-red))   ; color
          (17 . ,(all-the-icons-material "insert_drive_file"
                                         :height 0.8
                                         :face 'all-the-icons-red))   ; file
          (18 . ,(all-the-icons-material "collections_bookmark"
                                         :height 0.8
                                         :face 'all-the-icons-red))   ; reference
          (19 . ,(all-the-icons-material "folder"
                                         :height 0.8
                                         :face 'all-the-icons-red))   ; folder
          (20 . ,(all-the-icons-material "people"
                                         :height 0.8
                                         :face 'all-the-icons-red))   ; enumMember
          (21 . ,(all-the-icons-material "pause_circle_filled"
                                         :height 0.8
                                         :face 'all-the-icons-red))   ; constant
          (22 . ,(all-the-icons-material "streetview"
                                         :height 0.8
                                         :face 'all-the-icons-red))   ; struct
          (23 . ,(all-the-icons-material "event"
                                         :height 0.8
                                         :face 'all-the-icons-red))   ; event
          (24 . ,(all-the-icons-material "control_point"
                                         :height 0.8
                                         :face 'all-the-icons-red))   ; operator
          (25 . ,(all-the-icons-material "class"
                                         :height 0.8
                                         :face 'all-the-icons-red))))

  (defun +company-box-frontend-even-if-single (command)
    (cond ((or (eq command 'hide)
               (equal company-candidates-length 0))
           (company-box-hide))
          ((eq command 'update)
           (company-box-show))
          ((eq command 'post-command)
           (company-box--post-command))))
  (advice-add #'company-box-fronted
              :override #'+company-box-frontend-even-if-single))

(provide 'config-company)
;;; config-company.el ends here

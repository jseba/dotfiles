;;; config-org.el

(use-package org-plus-contrib
  :defer-incrementally
  (calendar
   find-func
   format-spec
   org-macs
   org-compat
   org-faces
   org-entities
   org-list
   org-pcomplete
   org-src
   org-footnote
   org-macro
   ob
   org
   org-clock
   org-agenda
   org-capture)

  :init
  (setq org-directory "~/org/")

  (defun +org-setup-ui ()
    (setq-default
     org-adapt-indentation nil
     org-cycle-include-plain-lists t
     org-eldoc-breadcrumb-separator " › "
     org-entities-user '(("flat" "\\flat" nil "" "" "266D" "♭")
                         ("sharp" "\\sharp" nil "" "" "266F" "♯"))
     org-fontify-done-headline t
     org-fontify-quote-and-verse-blocks t
     org-fontify-whole-heading-line t
     org-footnote-auto-label 'plain
     org-hidden-keywords nil
     org-hide-emphasis-markers nil
     org-hide-leading-stars t
     org-hide-leading-stars-before-indent-mode t
     org-image-actual-width nil
     org-indent-indentation-per-level 2
     org-indent-mode-turns-on-hiding-stars t
     org-list-description-max-indent 4
     org-pretty-entities nil
     org-pretty-entities-include-sub-superscripts t
     org-priority-faces '((?a . error)
                          (?b . warning)
                          (?c . success))
     org-refile-targets '((nil :maxlevel . 3)
                          (org-agenda-files :maxlevel . 3))
     org-startup-folded t
     org-startup-indented t
     org-startup-with-inline-images nil
     org-tags-column 0
     org-todo-keywords
     '((sequence "[ ](t)" "[-](p)" "[?](m)" "|" "[X](d)")
       (sequence "TODO(T)" "|" "DONE(D)")
       (sequence "NEXT(n)" "ACTIVE(a)" "WAITING(w)" "LATER(l)" "|" "CANCELLED(c)"))
     org-use-sub-superscripts '{}
     org-preview-latex-image-directory (concat %var-dir "org-latex/")
     org-format-latex-options (plist-put org-format-latex-options :scale 1.5))

    (defun +org-update-latex-preview-background-color ()
      "Ensure that LaTeX previews match the current theme."
      (setq-default
       org-format-latex-options
       (plist-put org-format-latex-options
                  :background
                  (face-attribute (or (cadr (assq 'default face-remapping-alist))
                                      'default)
                                  :background nil t))))
    (add-hook 'load-theme-hook #'+org-update-latex-preview-background-color)

    (defun +org--relpath (path root)
      (if (and buffer-file-name
               (file-in-directory-p buffer-file-name root))
          (file-relative-name path)
        path))

    (org-link-set-parameters
     "file"
     :face (lambda (path)
             (if (or (file-remote-p path)
                     (file-exists-p path))
                 'org-link
               'error)))

    (eval-when-compile
      (defmacro +org-define-file-link! (key dir)
        `(org-link-set-parameters
          ,key
          :complete
          (lambda () (+org--relpath (+org-link-read-file ,key ,dir) ,dir))
          :follow
          (lambda (link) (find-file (expand-file-name link ,dir)))
          :face
          (lambda (link) (if (file-exists-p (expand-file-name link ,dir))
                        'org-link
                      'error)))))

    (+org-define-file-link! "org" org-directory)
    (+org-define-file-link! "dotemacs" %emacs-dir))

  (defun +org-setup-popup-rules ()
    (+popup-set-rules
     '(("^\\*Org Links" :slot -1 :vslot -1 :size 2 :ttl 0)
       ("^\\*\\(?:Agenda Com\\|Calendar\\|Org \\(?:Export Dispatcher\\|Select\\)\\)"
        :slot -1 :vslot -1 :size #'+popup-shrink-to-fit :ttl 0)
       ("^\\*Org Agenda" :size 0.35 :select t :ttl nil)
       ("^\\*Org Src" :size 0.3 :quit nil :select t :autosave t :ttl nil)
       ("^CAPTURE.*\\.org$" :size 0.2 :quit nil :select t :autosave t))))

  (defun +org-setup-agenda ()
    (setq-default
     org-agenda-dim-blocked-tasks nil
     org-agenda-inhibit-startup t
     org-agenda-skip-unavailable-files t
     org-agenda-span 10
     org-agenda-start-on-weekday nil
     org-agenda-start-day "-3d"))

  (defun +org-setup-keybinds ()
    (add-hook 'global-escape-hook #'+org-remove-occur-highlights)

    (setq org-special-ctrl-a/e t
          org-M-RET-may-split-line nil
          org-insert-heading-respect-content t)

    (add-hook! 'org-tab-first-hook #'(+org-indent-maybe
                                      +org-yas-expand-maybe))
    (add-hook 'delete-backward-functions
              #'+org-delete-backward-char-and-realign-table-maybe)

    (define-key! org-mode-map
      (kbd "C-c C-S-l") #'+org-remove-link
      (kbd "C-c C-i")   #'org-toggle-inline-images
      [remap backward-to-bol-or-indent] #'org-beginning-of-line
      [remap forward-to-last-non-comment-or-eol] #'org-end-of-line))

  (defun +org-setup-hacks ()
    (setf (alist-get 'file org-link-frame-setup) #'find-file)

    (defun +org-delayed-recenter ()
      "Same as `recenter' but after a small delay.

Needed to prevent race conditions when a window's buffer hasn't changed at the
time this hook is run."
      (run-at-time 0.1 nil #'recenter))
    (add-hook 'org-follow-link-hook #'+org-delayed-recenter)

    (defun +org-fix-font-size-variation-in-eldoc (orig-fn)
      "Fix variable height `org-level-N' faces in the `eldoc' string."
      (cl-letf (((symbol-function 'org-format-outline-path)
                 (lambda (path &optional _width _prefix separator)
                   (string-join
                    (cl-loop with i = 1
                             for seg in (delq nil path)
                             for face = (nth (% (cl-incf i) org-level-n-faces)
                                             org-level-faces)
                             collect
                             (propertize
                              (replace-regexp-in-string "[ \t]+\\'" "" seg)
                              'face (if face
                                        `(:foreground
                                          ,(face-foreground face nil t)))))
                    separator))))
        (funcall orig-fn)))
    (advice-add #'org-eldoc-get-breadcrumb
                :around #'+org-fix-font-size-variation-in-eldoc)

    (setq org-file-apps
          `(("pdf" . default)
            ("\\.x?html?\\'" . default)
            ("/docs/" . emacs)
            (auto-mode . emacs)
            (directory . emacs)
            (t . ,(cond (%IS-MACOS "open -R \"%s\"")
                        (%IS-LINUX "xdg-open \"%s\"")
                        (%IS-WIN32 "start \"%s\"")))))

    (defun +org-exclude-agenda-buffers-from-recentf (orig-fn file)
      (let ((recentf-exclude (list (lambda (_file) t))))
        (funcall orig-fn file)))
    (advice-add #'org-get-agenda-file-buffer
                :around #'+org-exclude-agenda-from-recentf))

  (defun +org-setup-pretty-code ()
    (+pretty-code-set-symbols 'org-mode
      :name "#+NAME:"
      :src_block "#+BEGIN_SRC"
      :src_block_end "#+END_SRC"))

  (defun +org-disable-line-numbers ())

  (defun +org-disable-show-paren-mode ())

  (defun +org-enable-auto-reformat-tables ()
    "Realign tables and update formulas when exiting insert mode."
    (when (featurep 'evil)
      (add-hook 'evil-insert-state-exit-hook #'+org-realign-table-maybe nil t)
      (add-hook 'evil-replace-state-exit-hook #'+org-realign-table-maybe nil t)
      (advice-add #'evil-replace :after #'+org-realign-table-maybe)))

  (defun +org-enable-auto-update-cookies ()
    "Update statistics cookies when saving or exiting insert mode."
    (when (featurep 'evil)
      (add-hook 'evil-insert-state-exit-hook #'+org-update-cookies nil t))
    (add-hook 'before-save-hook #'+org-update-cookies nil t))

  (defun +org-config-smartparens-compatibility ()
    "Make `smartparens' not impose itself in `org-mode'."
    (after! smartparens
      (defun +org-sp-in-point-checkbox-p (_id action _context)
        (and (eq action 'insert)
             (sp--looking-at-p "\\s-*]")))
      (defun +org-sp-point-at-bol-p (_id action _context)
        (and (eq action 'insert)
             (eq (char-before) ?*)
             (sp--looking-back-p "^\\**" (line-beginning-position))))

      (sp-with-modes 'org-mode
        (sp-local-pair "*" nil :unless '(:add sp-point-before-word-p
                                              +org-sp-point-at-bol-p))
        (sp-local-pair "_" nil :unless '(:add sp-point-before-word-p))
        (sp-local-pair "/" nil :unless '(:add sp-point-before-word-p
                                              +org-sp-point-in-checkbox-p))
        (sp-local-pair "~" nil :unless '(:add sp-point-before-word-p))
        (sp-local-pair "=" nil :unless '(:add sp-point-before-word-p)))))

  (defun +org-auto-unfold-to-second-level-or-point ()
    "Expands the first level, but no further. If point was left somewhere
deeper, unfold to point on startup."
    (unless org-agenda-inhibit-startup
      (when (eq org-startup-folded t)
        (outline-hide-sublevels 2))
      (when (outline-invisible-p)
        (ignore-errors
          (save-excursion
            (outline-previous-visible-heading 1)
            (org-show-subtree))))))

  (add-hook! 'org-load-hook #'(+org-setup-ui
                               +org-setup-popup-rules
                               +org-setup-agenda
                               +org-setup-keybinds
                               +org-setup-hacks
                               +org-setup-pretty-code))

  (add-hook! 'org-mode-hook #'(+org-disable-line-numbers
                               org-indent-mode
                               auto-fill-mode
                               +org-disable-show-paren-mode
                               +org-enable-auto-reformat-tables
                               +org-enable-auto-update-cookies
                               +org-config-smartparens-compatibility
                               +org-auto-unfold-to-second-level-or-point))

  (run-hooks 'org-load-hook)

  :config
  (defun +org-dwim-at-point ()
    "Do-what-I-mean-at-point.

If on a:
  - checkbox, list item or TODO heading: toggle it.
  - clock: update its time.
  - headline: toggle LaTeX fragments and inline images underneath.
  - footnote reference: jump to the footnote's definition.
  - footnote definition: jump to the first reference to of this footnote.
  - table-row or a TBLFM: recalculate the table's formulas.
  - table-cell: clear it and go into insert mode. If this is a formula cell,
    recalculate it instead.
  - babel-call: execute the source block.
  - statistics-cookie: update it.
  - LaTeX fragment: toggle it.
  - link: follow it.

otherwise, refresh all inline images in the current tree."
    (interactive)
    (let* ((context (org-element-context))
           (type (org-element-type context)))
      (while (and context
                  (memq type '(verbatim
                               code
                               bold
                               italic
                               underline
                               strike-through
                               subscript
                               superscript)))
        (setq context (org-element-property :parent context)
              type (org-element-type context)))
      (pcase type
        ((guard
          (org-element-property
           :checkbox
           (org-element-lineage context '(item) t)))
         (let ((match (and (org-at-item-checkbox-p)
                           (match-string 1))))
           (org-toggle-checkbox (if (equal match "[ ]") '(16)))))

        (`headline
         (cond ((org-element-property :todo-type context)
                (org-todo
                 (if (eq (org-element-property :todo-type 'done))
                     'todo
                   'done)))
               ((string= "ARCHIVE" (car-safe (org-get-tags)))
                (org-force-cycle-archived))
               (t
                (+org-refresh-inline-images)
                (org-remove-latex-fragment-image-overlays)
                (org-toggle-latex-fragment '(4)))))

        (`clock (org-clock-update-time-maybe))

        (`footnote-reference
         (org-footnote-goto-definition (org-element-propert :label context)))

        (`footnote-definition
         (org-footnote-goto-previous-reference
          (org-element-property :label context)))

        ((or `planning `timestamp)
         (org-follow-timestamp-link))

        ((or `table `table-row)
         (if (org-at-TBLFM-p)
             (org-table-calc-current-TBLFM)
           (ignore-errors
             (save-excursion
               (goto-char (org-element-property :contents-begin context))
               (org-call-with-arg 'org-table-recalculate (or arg t))))))

        (`table-cell
         (org-table-blank-field)
         (org-table-recalculate)
         (when (and (string-empty-p (string-trim (org-table-get-field)))
                    (bound-and-true-p evil-mode))
           (evil-change-state 'insert)))

        (`babel-call
         (org-babel-lob-execute-maybe))

        (`statistics-cookie
         (save-excursion (org-update-statistics-cookies nil)))

        ((or `src-block `inline-src-block)
         (org-babel-execute-src-block))

        ((or `latex-fragment `latex-environment)
         (org-toggle-latex-fragment))

        (`link
         (let ((path (org-element-property
                      :path
                      (org-element-lineage context '(link) t))))
           (if (and path (image-type-from-file-name path))
               (+org-refresh-inline-images)
             (org-open-at-point))))

        (_ (+org-refresh-inline-images)))))

  (defun +org-insert-item (direction)
    "Inserts a new heading, table cell or item (depending on the context).

DIRECTION can be `\'above' or `\'below'."
    (interactive)
    (let* ((context (save-excursion
                      (when (bolp)
                        (back-to-indentation)
                        (forward-char))
                      (org-element-lineage
                       (org-element-context)
                       '(table table-row headline inlinetask item plain-list)
                       t)))
           (type (org-element-type context)))
      (cond ((memq type '(item plain-list))
             (let ((marker (org-element-property :bullet context))
                   (pad (save-excursion
                          (org-beginning-of-item)
                          (back-to-indentation)
                          (- (point) (line-beginning-position)))))
               (save-match-data
                 (pcase direction
                   (`below
                    (org-end-of-line)
                    (backward-char)
                    (end-of-line)
                    (if (and marker
                             (string-match "\\([0-9]+\\)\\([).] *\\)"
                                           marker))
                        (let ((l (line-number-at-pos)))
                          (org-insert-item)
                          (when (= l (line-number-at-pos))
                            (org-next-item)
                            (org-end-of-line)))
                      (insert "\n" (make-string pad 32) (or marker ""))))
                   (`above
                    (org-beginning-of-line)
                    (if (and marker (string-match-p "[0-9]+[).]" marker))
                        (org-insert-item)
                      (insert (make-string pad 32) (or marker ""))
                      (save-excursion (insert "\n")))))))
             (when (org-element-property :checkbox context)
               (insert "[ ] ")))

            ((memq type '(table table-row))
             (pcase direction
               ('below (save-excursion (org-table-insert-row t))
                       (org-table-next-row))
               ('above (save-excursion (org-shiftmetadown))
                       (+org-table-previous-row))))

            ((memq type '(headline inlinetask))
             (let ((level (if (eq (org-element-type context) 'headline)
                              (org-element-property :level context)
                            1)))
               (pcase direction
                 (`below
                  (let ((at-eol (>= (point) (1- (line-end-position))))
                        org-insert-heading-respect-content)
                    (goto-char (line-end-position))
                    (org-end-of-subtree)
                    (insert (concat "\n"
                                    (when (= level 1)
                                      (if at-eol
                                          (ignore (cl-incf level))
                                        "\n"))
                                    (make-string level ?*)
                                    " "))))
                 (`above
                  (org-back-to-heading)
                  (insert (make-string level ?*) " ")
                  (save-excursion
                    (insert "\n")
                    (if (= level 1) (insert "\n")))))
               (when (org-element-property :todo-type context)
                 (org-todo 'todo))))

            (t (user-error "Not a valid list, heading or table")))
      (when (org-invisible-p)
        (org-show-hidden-entry))
      (when (bound-and-true-p evil-mode)
        (evil-insert 1)))))
;;
;; Ignore org from ELPA
(use-package org :disabled t)

(use-package org-crypt
  :ensure nil ;; built-in
  :hook (org-load-hook . org-crypt-use-before-save-magic)
  :config
  (setq org-tags-exclude-from-inheritance '("crypt")
        org-crypt-key user-mail-address))

(use-package org-clock
  :ensure nil ;; built-in
  :commands org-clock-save
  :hook (org-mode . org-clock-load)
  :init
  (setq org-clock-persist 'history
        org-clock-persist-file (concat %etc-dir "org-clock-save.el"))
  :config
  (add-hook 'kill-emacs-hook #'org-clock-save))

(use-package toc-org)

(use-package evil-org
  :hook (org-mode . evil-org-mode)
  :init
  (defvar evil-org-key-theme '(navigation insert textobjects))
  (defvar evil-org-special-o/O '(table-row))

  (defun +org-setup-evil (&rest args)
    (unless args (require 'evil-org))

    (add-hook 'org-tab-first-hook #'+org-cycle-only-current-subtrees t)
    (advice-add #'org-return-indent
                :after #'+org-fix-newline-and-indent-in-src-blocks)

    (advice-add #'evil-org-open-below :around #'+org-evil-org-open-below)

    (evil-define-key* 'normal outline-mode-map
      "^" nil
      [backtab] nil
      "\M-j" nil
      "\M-k" nil
      "\C-j" nil
      "\C-k" nil
      "]" nil
      "[" nil)
    (evil-define-key* 'insert evil-org-mode-map
      [backtab] #'+org-dedent
      "\C-l" #'+org-table-next-field
      "\C-h" #'+org-table-previous-field
      "\C-j" #'+org-table-next-row
      "\C-k" #'+org-table-previous-row)
    (evil-define-key* '(insert normal) evil-org-mode-map
      (kbd "C-S-l") #'+org-table-append-field-or-shift-right
      (kbd "C-S-h") #'+org-table-prepend-field-or-shift-left
      (kbd "C-S-j") #'org-metadown
      (kbd "C-S-k") #'org-metaup)
    (evil-define-key* 'insert evil-org-mode-map
      [return] #'org-return-indent)
    (evil-define-key* 'normal evil-org-mode-map
      [return] #'+org-dwim-at-point)
    (evil-define-key* '(insert normal) evil-org-mode-map
      [M-return] (lambda! (+org-insert-item 'below))
      [S-M-return] (lambda! (+org-insert-item 'above)))
    (evil-define-key* 'motion evil-org-mode-map
      "[[" (lambda! (org-backward-heading-same-level nil) (org-beginning-of-line))
      "]]" (lambda! (org-forward-heading-same-level nil) (org-beginning-of-line))
      "[h" #'org-next-visible-heading
      "]h" #'org-previous-visible-heading
      "[l" #'org-next-link
      "]l" #'org-previous-link
      "[s" #'org-babel-next-src-block
      "]s" #'org-bable-previous-src-block
      "^" #'evil-org-beginning-of-line
      "0" (lambda! (let (visual-line-mode) (org-beginning-of-line))))
    (evil-define-key* 'normal evil-org-mode-map
      "gQ" #'org-fill-paragraph
      "za" #'+org-toggle-fold
      "zA" #'+org-shifttab
      "zc" #'+org-close-fold
      "zC" #'outline-hide-subtree
      "zm" #'+org-hide-next-fold-level
      "zo" #'+org-open-fold
      "zO" #'outline-show-subtree
      "zr" #'+org-show-next-fold-level
      "zR" #'outline-show-all)
    (define-key! org-read-date-minibuffer-local-map
      "\C-l" (lambda! (org-eval-in-calendar '(calendar-forward-day 1)))
      "\C-h" (lambda! (org-eval-in-calendar '(calendar-backward-day 1)))
      "\C-j" (lambda! (org-eval-in-calendar '(calendar-forward-week 1)))
      "\C-k" (lambda! (org-eval-in-calendar '(calendar-backward-week 1)))
      (kbd "C-S-l") (lambda! (org-eval-in-calendar '(calendar-forward-month 1)))
      (kbd "C-S-h") (lambda! (org-eval-in-calendar '(calendar-backward-month 1)))
      (kbd "C-S-j") (lambda! (org-eval-in-calendar '(calendar-forward-year 1)))
      (kbd "C-S-k") (lambda! (org-eval-in-calendar '(calendar-backward-year 1)))))
  (add-hook 'org-load-hook #'+org-setup-evil)
  (add-hook 'evil-org-mode-hook #'evil-normalize-keymaps)

  :config
  (define-minor-mode +org-pretty-mode
    "Hides emphasis markers and toggles pretty entities."
    :init-value nil
    :lighter " *"
    :group 'evil-org
    (setq org-hide-emphasis-markers +org-pretty-mode)
    (org-toggle-pretty-entities)
    (org-with-silent-modifications
     (org-table-map-tables 'org-table-align t)))

  (advice-add #'evil-org-set-key-theme :override #'ignore))

(use-package evil-org-agenda
  :after (evil-org org-agenda)
  :config
  (evil-org-agenda-set-keys))

(use-package ob-sql-mode)

(use-package ob-translate)

(use-package ox-pandoc)

(use-package htmlize)

(provide 'config-org)
;;; config-org.el ends here

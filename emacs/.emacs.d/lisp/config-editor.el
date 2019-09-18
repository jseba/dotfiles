;;; config-editor.el

(setq-default
 auto-save-list-file-name (no-littering-expand-var-file-name "autosave")
 auto-save-default nil
 bookmark-default-file (no-littering-expand-etc-file-name "bookmarks")
 bookmark-save-flag t
 create-lockfiles nil
 make-backup-files nil
 ring-bell-function #'ignore
 save-interprogram-paste-before-kill t
 tooltip-delay 1.5
 buffers-menu-max-size 30
 case-fold-search t
 column-number-mode t
 delete-selection-mode t
 delete-trailing-lines nil
 fill-column 140
 indicate-empty-lines t
 sentence-end-double-space nil
 word-wrap t
 hscroll-margin 1
 hscroll-step 1
 scroll-conservatively 1001
 scroll-margin 0
 scroll-preserve-screen-position t
 mouse-wheel-scroll-amount '(1)
 mouse-wheel-progressive-speed nil
 mouse-yank-at-point t
 indent-tabs-mode nil
 require-final-newline nil
 tab-always-indent 'complete
 tab-width 4
 tabify-regex "^\t [ \t]+"
 truncate-lines t
 truncate-partial-width-windows nil
 show-trailing-whitespace nil
 whitespace-line-column fill-column
 whitespace-style '(face indentation tab-mark spaces space-mark
                         newline newline-mark trailing lines-tail)
 whitespace-display-mappings '((tab-mark ?\t [?� ?\t])
                               (newline-mark ?\n [?� ?\n])
                               (space-mark ?\  [?�] [?.]))
 word-wrap t)

;; Bring back region casing
(put 'upcase-region   'disabled nil)
(put 'downcase-region 'disabled nil)

;; Bring back narrowing
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page   'disabled nil)
(put 'narrow-to-defun  'disabled nil)

;; Set system encoding to UTF-8 everywhere
(when (fboundp 'set-charset-priority)
  (set-charset-priority 'unicode))
(prefer-coding-system        'utf-8)
(set-terminal-coding-system  'utf-8)
(set-keyboard-coding-system  'utf-8)
(set-selection-coding-system 'utf-8)
(setq locale-coding-system   'utf-8)
(setq-default buffer-file-coding-system 'utf-8)

(defun backward-to-bol-or-indent ()
  (interactive)
  (let ((pos (point))
        (indent (save-excursion
                  (beginning-of-visual-line)
                  (skip-chars-forward " \t\r")
                  (point))))
    (cond ((or (> pos indent) (= pos (line-beginning-position)))
           (goto-char indent))
          ((<= pos indent)
           (beginning-of-visual-line)))))
(global-set-key [remap move-beginning-of-line] #'backward-to-bol-or-indent)

(defun forward-to-last-non-comment-or-eol ()
  (interactive)
  (let ((eol (save-excursion (if visual-line-mode
                                 (end-of-visual-line)
                               (end-of-line))
                             (point))))
    (if (and (sp-point-in-comment) (not (= (point) eol)))
        (goto-char eol)
      (let* ((bol (save-excursion (beginning-of-visual-line) (point)))
             (boc (or (save-excursion
                        (if (not comment-use-syntax)
                            (progn
                              (goto-char bol)
                              (when (re-search-forward comment-start-skip eol t)
                                (or (match-end 1) (match-beginning 0))))
                          (goto-char eol)
                          (while (and (sp-point-in-comment)
                                      (> (point) bol))
                            (backward-char))
                          (skip-chars-backward " " bol)
                          (point)))
                      eol)))
        (cond ((= boc (point))
               (goto-char eol))
              ((/= bol boc)
               (goto-char boc)))))))
(global-set-key [remap move-end-of-line] #'forward-to-last-non-comment-or-eol)

(defun backward-delete-whitespace-to-column ()
  (interactive)
  (let* ((context (sp-get-thing))
         (op (plist-get context :op))
         (cl (plist-get context :cl))
         open-len close-len)
    (cond
     ((and op cl
           (string= op cl)
           (and (string= (char-to-string (or (char-before) 0)) op)
                (setq open-len (length op)))
           (and (string= (char-to-string (or (char-after) 0)) cl)
                (setq close-len (length cl))))
      (delete-char (- open-len))
      (delete-char close-len))
     ((and (not indent-tabs-mode)
           (not (bolp))
           (not (sp-point-in-string))
           (save-excursion (>= (- (skip-chars-backward " \t")) tab-width)))
      (let ((movement (% (current-column) tab-width)))
        (when (= movement 0)
          (setq movement tab-width))
        (delete-char (- movement)))
      (unless (memq (char-before) '(?\n ?\ ))
        (insert " ")))
     (t (delete-char -1)))))

(defun surrounded-p (&optional pair inline balanced)
  (when-let* ((pair (or pair (sp-get-thing))))
    (let ((beg (plist-get pair :beg))
          (end (plist-get pair :end))
          (pt (point)))
      (when (and (> pt beg) (< pt end))
        (when-let* ((cl (plist-get pair :cl))
                    (op (plist-get pair :op)))
          (and (not (string= op ""))
               (not (string= cl ""))
               (let ((nbeg (+ (length op) beg))
                     (nend (- end (length cl))))
                 (let ((content (buffer-substring-no-properties nbeg nend)))
                   (and (string-match-p (format "[ %s]*" (if inline "" "\n")) content)
                        (or (not balanced)
                            (= (- pt nbeg) (- nend pt))))))))))))

(defun delete-backward-char-extended (n &optional killflag)
  (interactive "p\nP")
  (or (integerp n)
      (signal 'wrong-type-argument (list 'integerp n)))
  (cond ((and (use-region-p)
              delete-active-region
              (= n 1))
         (if (eq delete-active-region 'kill)
             (kill-region (region-beginning) (region-end) 'region)
           (funcall region-extract-function 'delete-only)))
        ((null (or (null overwrite-mode)
                   (<= n 0)
                   (memq (char-before) '(?\t ?\n))
                   (eobp)
                   (eq (char-after) ?\n)))
         (let ((ocol (current-column)))
           (delete-char (- n) killflag)
           (save-excursion
             (insert-char ?\s (- ocol (current-column)) nil))))
        ((and (= n 1) (bound-and-true-p smartparens-mode))
         (cond ((and (memq (char-before) '(?\  ?\t))
                     (save-excursion
                       (and (> (- (skip-chars-backward " \t" (line-beginning-position))) 0)
                            (bolp))))
                (backward-delete-whitespace-to-column))
               ((let* ((pair (sp-get-thing))
                       (op   (plist-get pair :op))
                       (cl   (plist-get pair :cl))
                       (beg  (plist-get pair :beg))
                       (end  (plist-get pair :end)))
                  (cond ((and end beg (= end (+ beg (length op) (length cl))))
                         (sp-backward-delete-char 1))
                        ((surrounded-p pair 'inline 'balanced)
                         (delete-char -1 killflag)
                         (delete-char 1)
                         (when (= (point) (+ (length cl) beg))
                           (sp-backward-delete-char 1)
                           (sp-insert-pair op)))
                        ((and (bolp) (surrounded-p) pair nil 'balanced)
                         (delete-region beg end)
                         (sp-insert-pair op)
                         t)
                        ((run-hook-with-args-until-success 'delete-backward-functions))
                        ((backward-delete-whitespace-to-column)))))))
        ((delete-char (- n) killflag))))
(advice-add #'delete-backward-char :override #'delete-backward-char-extended)

(defun newline-and-indent-maybe-continue-comment (_orig-fn)
  (interactive)
  (cond ((sp-point-in-string)
         (newline))
        ((and (sp-point-in-comment)
              comment-line-break-function)
         (funcall comment-line-break-function))
        (t
         (newline nil t)
         (indent-according-to-mode))))
(advice-add #'newline-and-indent :around #'newline-and-indent-maybe-continue-comment)

(defun retab (&optional beg end)
  "Change all tabs to spaces, or vice-versa, depding on `indent-tabs-mode'."
  (interactive "r")
  (unless (and beg end)
    (setq beg (point-min)
          end (point-max)))
  (if indent-tabs-mode
      (tabify beg end)
    (untabify beg end)))
;;(global-set-key "" #'retab)

(defun set-indirect-buffer-file-name  (orig-fn base-buffer name &optional clone)
  (let ((file-name (buffer-file-name base-buffer))
        (buffer (funcall orig-fn base-buffer name clone)))
    (when (and file-name buffer)
      (with-current-buffer buffer
        (unless buffer-file-name
          (setq buffer-file-name file-name
                buffer-file-truename (file-truename file-name)))))
    buffer))
(advice-add #'make-indirect-buffer :around #'set-indirect-buffer-file-name)

(defun newline-above-and-indent ()
  "Insert an indented newline before the current one."
  (interactive)
  (beginning-of-line)
  (save-excursion (newline))
  (indent-according-to-mode))
(defun newline-below-and-indent ()
  "Insert an indented newline after the current one."
  (interactive)
  (end-of-line)
  (newline-and-indent))
(bind-keys
 ("<C-return>" . newline-below-and-indent)
 ("<M-return>" . newline-above-and-indent))

(defalias '+newline #'newline)

(defalias 'join-line-above #'join-line)
(defalias 'join-line-below (lambda! (join-line t)))

(defun new-buffer ()
  "Creates a new empty buffer."
  (interactive)
  (if (featurep 'evil)
      (evil-buffer-new)
    (let ((buffer (generate-new-buffer "*new*")))
      (set-window-buffer nil)
      (with-current-buffer buffer
        (funcall (default-value 'major-mode))))))
(global-set-key (kbd "C-x C-n") #'new-buffer) ;; more useful than `set-goal-column'

(use-package autorevert
  :after-call after-find-file
  :config
  (setq auto-revert-verbose nil)
  (global-auto-revert-mode +1))

(use-package savehist
  :defer 1
  :after-call post-command-hook
  :config
  (setq savehist-file (concat %var-dir "savehist")
        savehist-save-minibuffer-history t
        savehist-autosave-interval nil
        savehist-additional-variables '(kill-ring search-ring regexp-search-ring))
  (savehist-mode +1))

(use-package saveplace
  :after-call (after-find-file dired-initial-position-hook)
  :config
  (setq save-place-file (concat %var-dir "saveplace"))
  (defun recenter-on-load-saveplace (&rest _)
    (if buffer-file-name (ignore-errors (recenter))))
  (advice-add #'save-place-find-file-hook
              :after-while #'recenter-on-load-saveplace)
  (save-place-mode +1))

(use-package recentf
  :defer 1
  :after-call after-find-file
  :commands recentf-open-files
  :config
  (setq recentf-save-file (concat %var-dir "recentf")
        recentf-auto-cleanup 'never
        recentf-max-menu-items 0
        recentf-max-saved-items 300
        recentf-filename-handlers '(file-truename)
        recentf-exclued (list #'file-remote-p
                              "\\.\\(?:gz\\|gif\\|svg\\|png\\|jpe?g\\)$"
                              "^/tmp"
                              "^/ssh:"
                              "\\.?ido\\.last$"
                              "\\.revive$"
                              "/TAGS$"
                              "^/var/folders/.+$"))
  (add-hook 'kill-emacs-hook #'recentf-cleanup)
  (quiet! (recentf-mode +1)))

(use-package smartparens
  :after-call (after-find-file pre-command-hook)
  :commands (sp-pair sp-local-pair sp-with-modes)
  :bind
  (([remap beginning-of-sexp]     . sp-beginning-of-sexp)
   ([remap end-of-sexp]           . sp-end-of-sexp)
   ([remap forward-sexp]          . sp-forward-sexp)
   ([remap backward-sexp]         . sp-backward-sexp)
   ("C-M-a" . sp-beginning-of-sexp)
   ("C-M-e" . sp-end-of-sexp)
   ("C-M-f" . sp-forward-sexp)
   ("C-M-b" . sp-backward-sexp)
   ("C-M-k" . sp-kill-sexp)
   ("C-M-t" . sp-transpose-sexp)
   ("C-<right>" . sp-forward-slurp-sexp)
   ("C-<left>"  . sp-backward-slurp-sexp)
   ("M-<right>" . sp-forward-barf-sexp)
   ("M-<left>"  . sp-backward-barf-sexp))
  :config
  (require 'smartparens-config)
  (setq sp-highlight-pair-overlay nil
        sp-highlight-wrap-overlay nil
        sp-highlight-wrap-tag-overlay nil
        sp-show-pair-from-inside t
        sp-cancel-autoskip-on-backward-movement nil
        sp-show-pair-delay 0.1
        sp-max-pair-length 4
        sp-max-prefix-length 50
        sp-escape-quotes-after-insert nil)

  (dolist (key '(:unmatched-expression :no-matching-tag))
    (setf (cdr (assq key sp-message-alist)) nil))

  (defun +smartparens-disable-navigate-skip-match ()
    (setq sp-navigate-skip-match nil
          sp-navigate-consider-sgml-tags nil))
  (add-hook 'after-change-major-mode-hook #'+smartparens-disable-navigate-skip-match)

  (defun +smartparens-enable-in-eval-expression ()
    (when (memq this-command '(eval-expression evil-ex))
      (smartparens-mode)))
  (add-hook 'minibuffer-setup-hook #'+smartparens-enable-in-eval-expression)
  (sp-local-pair 'minibuffer-inactive-mode "'" nil :actions nil)
  (sp-local-pair 'minibuffer-inactive-mode "`" nil :actions nil)

  ;; autopair quotes more conservatively
  (let ((unless-list '(sp-point-before-word-p
                       sp-point-after-word-p
                       sp-point-before-same-p)))
    (sp-pair "'" nil :unless unless-list)
    (sp-pair "\"" nil :unless unless-list))

  ;; auto expand braces
  (dolist (brace '("(" "{" "["))
    (sp-pair brace nil
             :post-handlers '(("||\n[i]" "RET") ("| " "SPC"))
             ;; except near a word or another opening brace
             :unless '(sp-point-before-word-p sp-point-before-same-p)))

  ;; don't do square-brace expansion when it doesn't make sense
  (sp-local-pair '(emacs-lisp-mode org-mode)
                 "[" nil :post-handlers '(:rem ("| " "SPC")))

  ;; reasonable defaults for comments
  (sp-local-pair '(c-mode c++-mode)
                 "/*" "*/"
                 :actions '(insert)
                 :post-handlers '(("| " "SPC")
                                  ("\n*/[i][d-2]" "RET")
                                  ("\n* ||\n*/[i][d-2]" "*")))

  ;; highjacks backspace to:
  ;; a) balance spaces inside braces: ( | ) -> (|)
  ;; b) delete space-indented `tab-width' steps at a time
  ;; c) close empty multiline brace blocks in one step:
  ;;   {
  ;;   |
  ;;   }
  ;;   becomes {|}
  ;; d) refresh `smartparens' `:post-handlers' so SPC and RET expansions work even after a backspace
  ;; e) properly delete `smartparens' pairs when they are encountered without the need for strict mode
  ;; f) do none of this when in a string
  (advice-add #'delete-backward-char :override #'delete-backward-char-extended)

  ;; make `newline-and-indent' smarter when comments are involved
  (advice-add #'newline-and-indent :around #'newline-and-indent-maybe-continue-comment)

  (smartparens-global-mode +1))

(use-package undo-tree
  :after-call after-find-file
  :config
  (setq undo-tree-auto-save-history nil
        undo-tree-enable-undo-in-region nil
        undo-tree-history-directory-alist `(("." . ,(concat %var-dir "undo-tree-hist/"))))
  (global-undo-tree-mode +1))

(use-package helpful
  :bind
  (([remap describe-function] . helpful-callable)
   ([remap describe-command]  . helpful-command)
   ([remap describe-variable] . helpful-variable)
   ([remap describe-key]      . helpful-key))
  :init
  (after! apropos
    (dolist (fun-bt '(apropos-func apropos-macro apropos-command))
      (button-type-put
       fun-bt 'action
       (lambda (button)
	 (helpful-callable (button-get button 'apropos-symbol)))))
    (dolist (var-bt '(apropos-variable apropos-user-option))
      (button-type-put
       var-bt 'action
       (lambda (button)
	 (helpful-variable (button-get button 'apropos-symbol)))))))

(use-package ws-butler
  :after-call (after-find-file)
  :config
  (appendq! ws-butler-global-exempt-modes
            '(special-mode comint-mode term-mode eshell-mode))
  (ws-butler-global-mode +1))

(provide 'config-editor.el)
;;; config-editor.el ends here

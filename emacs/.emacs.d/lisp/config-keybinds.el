;;; config-keybinds.el

(defun global-escape ()
  (interactive)
  (cond ((minibuffer-window-active-p (minibuffer-window))
         (abort-recursive-edit))
        ((cl-find-if #'funcall global-escape-hook))
        ((or defining-kbd-macro executing-kbd-macro) nil)
        ((keyboard-quit))))
(global-set-key [remap keyboard-quit] #'global-escape)

(use-package which-key
  :defer 1
  :after-call pre-command-hook
  :init
  (setq which-key-sort-order #'which-key-prefix-then-key-order
        which-key-sort-uppercase-first nil
        which-key-add-column-padding 1
        which-key-max-display-columns nil
        which-key-min-display-lines 6
        which-key-side-window-slot -10
        which-key-replacement-alist
        '(((nil . "Prefix Command")        . (nil . "prefix"))
          ((nil . "\\`\\?\\?\\'")          . (nil . "λ")) ;; Lambdas
          ((nil . "/body\\'")              . (nil . "|="));; Prettify hydra entry points
          ((nil . "<\\([[:alnum:]-]+\\)>") . (nil . "\\1"))
          (("up")         . ("↑"))
          (("right")      . ("→"))
          (("down")       . ("↓"))
          (("left")       . ("←"))
          (("DEL")        . ("⌫"))
          (("deletechar") . ("⌦"))
          (("RET")        . ("⏎"))
          (("TAB")        . ("⭾"))
          (("SPC")        . ("␣"))))
  :config
  (set-face-attribute 'which-key-local-map-description-face nil :weight 'bold)
  (which-key-setup-side-window-bottom)
  (setq-hook! 'which-key-init-buffer-hook line-spacing 3)

  (defun +which-key-no-fringes-in-buffer (&rest _)
    (set-window-fringes (get-buffer-window which-key--buffer) 0 0 nil))
  (advice-add 'which-key--show-buffer-side-window
              :after #'+which-key-no-fringes-in-buffer)

  (which-key-mode +1))

(use-package hydra)

(use-package general
  :demand t)

(defvar +map-leader-key "C-c"
  "The leader prefix key.")
(defvar +map-localleader-key (concat +map-leader-key " m")
  "The local-leader prefix key (for major-mode specific commands).")
(defvar +map-leader-map (make-sparse-keymap)
  "The overriding keymap for `leader' keys.")

(defvar +map-leader-alist `((t . ,+map-leader-map)))
(add-to-list 'emulation-mode-map-alists '+map-leader-alist)

(defalias 'define-key! #'general-def)
(defalias 'unmap! #'general-unbind)

(defmacro define-leader-key! (&rest args)
  `(general-define-key
    :states nil
    :keymaps '+map-leader-map
    :prefix +map-leader-key
    ,@args))
(general-create-definer define-localleader-key!
  :major-modes t
  :wk-full-keys nil
  :prefix +map-localleader-key)

(dolist (key '(:after
               :desc
               :leader
               :localleader
               :keymap
               :mode
               :prefix
               :unless
               :when))
  (put key 'lisp-indent-function 'defun))

(defvar +map--forms nil)
(defvar +map--fn nil)
(defvar +map--batch-forms nil)
(defvar +map--state '(:dummy t))
(defvar +map--parent-state nil)

(defun +map--process (rest)
  (let ((+map--fn +map--fn)
        +map--forms
        +map--state
        desc)
    (while rest
      (let ((key (pop rest)))
        (cond ((listp key)
               (+map--nested nil key))

              ((keywordp key)
               (pcase key
                 (:leader
                  (+map--commit)
                  (setq +map--fn 'define-leader-key!))
                 (:localleader
                  (+map--commit)
                  (setq +map--fn 'define-localleader-key!))
                 (:after
                  (+map--nested (list 'after! (pop rest)) rest)
                  (setq rest nil))
                 (:desc
                  (setq desc (pop rest)))
                 (:keymap
                  (+map--set :keymaps `(quote ,(enlist (pop rest)))))
                 (:mode
                  (push (cl-loop for m in (enlist (pop rest))
                                 collect (intern (concat (symbol-name m) "-map")))
                        rest)
                  (push :keymap rest))
                 (:when
                  (+map--nested (list 'when (pop rest)) rest)
                  (setq rest nil))
                 (:unless
                  (+map--nested (list 'unless (pop rest)) rest)
                  (setq rest nil))
                 (:prefix
                  (cl-destructuring-bind (prefix . desc) (enlist (pop rest))
                    (+map--set (if +map--fn :infix :prefix)
                               prefix)
                    (when (stringp desc)
                      (setq rest (append (list :desc desc "" nil) rest)))))
                 (_
                  (error "Not a valid `map!' property: %s" key)
                  (setq desc nil))))

              ((+map--def key (pop rest) nil desc)
               (setq desc nil)))))
    (+map--commit)
    (macroexp-progn (nreverse (delq nil +map--forms)))))

(defun +map--append-keys (prop)
  (let ((a (plist-get +map--parent-state prop))
        (b (plist-get +map--state prop)))
    (if (and a b)
        `(general--concat nil ,a ,b)
      (or a b))))

(defun +map--nested (wrapper rest)
  (+map--commit)
  (let ((+map--parent-state (+map--state)))
    (push (if wrapper
              (append wrapper (list (+map--process rest)))
            (+map--process rest))
          +map--forms)))

(defun +map--set (prop &optional value)
  (unless (equal (plist-get +map--state prop) value)
    (+map--commit))
  (setq +map--state (plist-put +map--state prop value)))

(defun +map--def (key def &optional states desc)
  (when (or (memq 'global states)
            (null states))
    (setq states (cons 'nil (delq 'global states))))
  (when desc
    (let (unquoted)
      (cond ((and (listp def)
                  (keywordp (car-safe (setq unquoted (unquote def)))))
             (setq def (list 'quote (plist-put unquoted :which=key desc))))
            ((setq def (cons 'list
                             (if (and (equal key "")
                                      (null def))
                                 `(:ignore t :which-key ,desc)
                               (plist-put (general--normalize-extended-def def)
                                          :which-key desc))))))))
  (dolist (state states)
    (push (list key def)
          (alist-get state +map--batch-forms)))
  t)

(defun +map--commit ()
  (when +map--batch-forms
    (cl-loop with attrs = (+map--state)
             for (state . defs) in +map--batch-forms
             if (not state)
             collect `(,(or +map--fn 'general-define-key)
                       ,@(if state `(:states ',state))
                       ,@attrs
                       ,@(mapcan #'identity (nreverse defs)))
             into forms
             finally do (push (macroexp-progn forms) +map--forms))
    (setq +map--batch-forms nil)))

(defun +map--state ()
  (let ((plist (append (list :prefix  (+map--append-keys :prefix)
                             :infix   (+map--append-keys :infix)
                             :keymaps (append (plist-get +map--parent-state :keymaps)
                                              (plist-get +map--state :keymaps)))
                       +map--state
                       nil))
        newplist)
    (while plist
      (let ((key (pop plist))
            (val (pop plist)))
        (when (and val (not (plist-member newplist key)))
          (push val newplist)
          (push key newplist))))
    newplist))

(defmacro map! (&rest rest)
  "A convenience macro for defining keybinds, powered by `general'.

Shamelessly lifted from Doom Emacs."
  (+map--process rest))

(add-hook! tty-setup-hook (define-key input-decode-map
                            (kbd "TAB") [tab]))

(map! "C-a"   #'backward-to-bol-or-indent
      "C-e"   #'forward-to-last-non-comment-or-eol
      "C-j"   #'newline-below-and-indent
      "C-S-j" #'newline-above-and-indent

      [remap newline] #'newline-and-indent
      [C-return]      #'+newline

      [M-return]    #'join-line-below
      [M-S-return]  #'join-line-above

      "C-S-f" #'toggle-frame-fullscreen

      "C-x p" #'+popup-other
      "C-`"   #'+popup-toggle
      "C-~"   #'+popup-raise
      "C-;"   #'+smartparens-hydra/body)

(map! :leader                           ; "C-c"
      "["       #'previous-buffer
      "]"       #'next-buffer
      "SPC"     #'projectile-find-file
      "RET"     #'bookmark-jump
      "/"       #'+helm-project-search
      "'"       #'helm-resume
      "e"       #'+eshell-open
      "E"       #'+eshell-open-popup
      "k"       #'kill-this-buffer
      "K"       #'kill-other-buffers
      "r"       #'projectile-recentf
      "R"       #'recentf-open-files
      "x"       #'flycheck-list-errors)

(after! lsp
  (map! :leader (:prefix ("l" . "lsp")
                  :desc "Format Buffer"       "=" #'lsp-format-buffer
                  :desc "Code Action"         "a" #'lsp-execute-code-action
                  :desc "Sideline Mode"       "l" #'lsp-ui-sideline-mode
                  :desc "Doc Mode"            "d" #'lsp-ui-doc-mode
                  :desc "Diagnostics"         "e" #'lsp-ui-flycheck-list
                  :desc "Imenu"               "i" #'lsp-ui-imenu
                  :desc "Rename"              "r" #'lsp-rename
                  :desc "Restart Workspace"   "R" #'lsp-restart-workspace
                  :desc "Find Symbol"         "?" #'lsp-ui-peek-find-workspace-symbol)))

(map! :localleader
      (:keymap (c-mode-map c++-mode-map)
        (:after ccls
          :prefix ("x" . "ccls")
          :desc "Variable Addresses"    "A" #'+ccls-references-address
          :desc "Function Addresses"    "F" #'+ccls-references-not-call
          :desc "Macro References"      "P" #'+ccls-references-macro
          :desc "Read References"       "R" #'+ccls-references-read
          :desc "Write References"      "W" #'+ccls-references-write
          :desc "Direct Bases"          "b" (lambda! (+ccls-base 1))
          :desc "All Bases"             "B" (lambda! (+ccls-base 3))
          :desc "Directed Derived"      "d" (lambda! (+ccls-derived 1))
          :desc "All Derived"           "D" (lambda! (+ccls-derived 3))
          :desc "Base Hierarchy"        "i" #'ccls-inheritance-hierarchy
          :desc "Derived Hierarchy"     "I" (lambda! (ccls-inheritance-hierarchy t))
          :desc "Callers"               "c" #'+ccls-caller
          :desc "Callee"                "C" #'+ccls-callee
          :desc "Caller Hierarchy"      "e" #'ccls-call-hierarchy
          :desc "Callee Hierarchy"      "E" (lambda! (ccls-call-hierarchy t))
          :desc "Nested Classes"        "s" (lambda! (+ccls-member 2))
          :desc "Member Functions"      "f" (lambda! (+ccls-member 3))
          :desc "Member Variables"      "m" (lambda! (+ccls-member 0))
          :desc "Member Hierarchy"      "M" #'ccls-member-hierarchy
          :desc "Local Variables"       "v" (lambda! (+ccls-vars 3))
          :desc "All Variables"         "V" (lambda! (+ccls-vars 7))
          :desc "Fields"                "k" (lambda! (+ccls-vars 1))
          :desc "Go To Type Definition" "t" #'lsp-goto-type-definition
          :desc "Code Lens Mode"        "l" #'ccls-code-lens-mode)))

(when %IS-MACOS
  (map! :keymap input-decode-map
        [S-iso-lefttab] [backtab]))        ;; Fix Shift+Tab in macOS

;; Fix legacy control key translations
(when (display-graphic-p)
  (map! :keymap input-decode-map
        [(control ?i)] [control-i]
        [(control ?I)] [(shift control-i)]))

(after! eshell
  (map! :keymap eshell-mode-map
        "C-s"   #'+eshell-search-history
        "C-c s" #'+eshell-split-below
        "C-c v" #'+eshell-split-right
        "C-c x" #'+eshell-kill-and-close
        [remap split-window-below] #'+eshell-split-below
        [remap split-window-right] #'+eshell-split-right
        [remap backward-to-bol-or-indent] #'eshell-bol
        [remap backward-kill-to-bol-or-indent] #'eshell-kill-input))

(after! smartparens
  (map!
    "C-M-a"     #'sp-beginning-of-sexp
    "C-M-e"     #'sp-end-of-sexp
    "C-M-f"     #'sp-forward-sexp
    "C-M-b"     #'sp-backward-sexp
    "C-M-d"     #'sp-splice-sexp
    "C-M-k"     #'sp-kill-sexp
    "C-M-t"     #'sp-transpose-sexp
    "C-<right>" #'sp-forward-slurp-sexp
    "M-<right>" #'sp-forward-barf-sexp
    "C-<left>"  #'sp-backward-slurp-sexp
    "M-<left>"  #'sp-backward-barf-sexp))

(after! company
  (map!
   [remap dabbrev-expand] #'+company-dabbrev
   [control-i]            #'+company-complete

   (:keymap company-active-map
     "C-n"       #'company-select-next
     "C-p"       #'company-select-prev
     "C-h"       #'company-show-doc-buffer
     "C-S-h"     #'company-show-doc-buffer
     "C-s"       #'company-search-candidates
     "M-s"       #'company-filter-candidates
     [tab]       #'company-complete-common-or-cycle
     [backtab]   #'company-select-previous
     [C-Return]  #'helm-company)
   (:keymap company-search-map
     "C-n"      #'company-search-repeat-forward
     "C-p"      #'company-search-repeat-backward
     "C-s"      (lambda! (company-search-abort) (company-filter-candidates))
     [escape]   #'company-search-abort)
   (:keymap comint-mode-map
     [tab]      #'company-complete)))

(map! :keymap (minibuffer-local-map
               minibuffer-local-ns-map
               minibuffer-local-completion-map
               minibuffer-local-must-match-map
               minibuffer-local-isearch-map
               read-expression-map)
      "\C-s" #'helm-minibuffer-history
      "C-w" #'backward-kill-word
      "C-u" #'backward-kill-sentence
      "C-z" (lambda! (ignore-errors
                       (call-interactively #'undo)))
      "C-S-d" #'scroll-up-command
      "C-S-n" #'scroll-down-command)

(provide 'config-keybinds)
;;; config-keybinds.el ends here

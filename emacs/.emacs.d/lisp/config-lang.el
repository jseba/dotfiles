;;; config-lang.el

;;
;; Prettify Symbols

(defconst +pretty-code-symbols
  '(;; Org
    :name          "»"
    :src_block     "»"
    :src_block_end " "
    ;; Functional
    :lambda        "λ"
    :def           "ƒ"
    :composition   "∘"
    :map           "↦"
    ;; Types
    :null          "∅"
    :true          "𝕋"
    :false         "𝔽"
    :int           "ℤ"
    :float         "ℝ"
    :str           "𝕊"
    :bool          "𝔹"
    ;; Flow
    :not           "￢"
    :in            "∈"
    :not-in        "∉"
    :and           "∧"
    :or            "∨"
    :for           "∀"
    :some          "∃"
    :return        "⟼"
    :yield         "⟻"
    ;; Other
    :tuple         "⨂"
    :pipe          "" ;; FIXME: find a non-private char
    :dot           "•"))

(defvar +pretty-code-symbols-alist '((t)))

(defconst +pretty-code-fira-font-ligatures
  '(("www"         . #Xe100)
    ("**"          . #Xe101)
    ("***"         . #Xe102)
    ("**/"         . #Xe103)
    ("*>"          . #Xe104)
    ("*/"          . #Xe105)
    ("\\\\"        . #Xe106)
    ("\\\\\\"      . #Xe107)
    ("{-"          . #Xe108)
    ("[]"          . #Xe109)
    ("::"          . #Xe10a)
    (":::"         . #Xe10b)
    (":="          . #Xe10c)
    ("!!"          . #Xe10d)
    ("!="          . #Xe10e)
    ("!=="         . #Xe10f)
    ("-}"          . #Xe110)
    ("--"          . #Xe111)
    ("---"         . #Xe112)
    ("-->"         . #Xe113)
    ("->"          . #Xe114)
    ("->>"         . #Xe115)
    ("-<"          . #Xe116)
    ("-<<"         . #Xe117)
    ("-~"          . #Xe118)
    ("#{"          . #Xe119)
    ("#["          . #Xe11a)
    ("##"          . #Xe11b)
    ("###"         . #Xe11c)
    ("####"        . #Xe11d)
    ("#("          . #Xe11e)
    ("#?"          . #Xe11f)
    ("#_"          . #Xe120)
    ("#_("         . #Xe121)
    (".-"          . #Xe122)
    (".="          . #Xe123)
    (".."          . #Xe124)
    ("..<"         . #Xe125)
    ("..."         . #Xe126)
    ("?="          . #Xe127)
    ("??"          . #Xe128)
    (";;"          . #Xe129)
    ("/*"          . #Xe12a)
    ("/**"         . #Xe12b)
    ("/="          . #Xe12c)
    ("/=="         . #Xe12d)
    ("/>"          . #Xe12e)
    ("//"          . #Xe12f)
    ("///"         . #Xe130)
    ("&&"          . #Xe131)
    ("||"          . #Xe132)
    ("||="         . #Xe133)
    ("|="          . #Xe134)
    ("|>"          . #Xe135)
    ("^="          . #Xe136)
    ("$>"          . #Xe137)
    ("++"          . #Xe138)
    ("+++"         . #Xe139)
    ("+>"          . #Xe13a)
    ("=:="         . #Xe13b)
    ("=="          . #Xe13c)
    ("==="         . #Xe13d)
    ("==>"         . #Xe13e)
    ("=>"          . #Xe13f)
    ("=>>"         . #Xe140)
    ("<="          . #Xe141)
    ("=<<"         . #Xe142)
    ("=/="         . #Xe143)
    (">-"          . #Xe144)
    (">="          . #Xe145)
    (">=>"         . #Xe146)
    (">>"          . #Xe147)
    (">>-"         . #Xe148)
    (">>="         . #Xe149)
    (">>>"         . #Xe14a)
    ("<*"          . #Xe14b)
    ("<*>"         . #Xe14c)
    ("<|"          . #Xe14d)
    ("<|>"         . #Xe14e)
    ("<$"          . #Xe14f)
    ("<$>"         . #Xe150)
    ("<!--"        . #Xe151)
    ("<-"          . #Xe152)
    ("<--"         . #Xe153)
    ("<->"         . #Xe154)
    ("<+"          . #Xe155)
    ("<+>"         . #Xe156)
    ("<="          . #Xe157)
    ("<=="         . #Xe158)
    ("<=>"         . #Xe159)
    ("<=<"         . #Xe15a)
    ("<>"          . #Xe15b)
    ("<<"          . #Xe15c)
    ("<<-"         . #Xe15d)
    ("<<="         . #Xe15e)
    ("<<<"         . #Xe15f)
    ("<~"          . #Xe160)
    ("<~~"         . #Xe161)
    ("</"          . #Xe162)
    ("</>"         . #Xe163)
    ("~@"          . #Xe164)
    ("~-"          . #Xe165)
    ("~="          . #Xe166)
    ("~>"          . #Xe167)
    ("~~"          . #Xe168)
    ("~~>"         . #Xe169)
    ("%%"          . #Xe16a)))

(defun +pretty-code--correct-symbol-bounds (ligature-alist)
  (let ((len (length (car ligature-alist)))
        (acc (list   (cdr ligature-alist))))
    (while (> len 1)
      (setq acc (cons #X00a0 (cons '(Br . Bl) acc))
            len (1- len)))
    (cons (car ligature-alist) acc)))

(defun +pretty-code-setup-fira-ligatures ()
  (set-fontset-font t '(#Xe100 . #Xe16f) "Fira Code Symbol")
  (setq-default prettify-symbols-alist
                (append prettify-symbols-alist
                        (mapcar #'+pretty-code--correct-symbol-bounds
                                +pretty-code-fira-font-ligatures))))
(add-hook 'init-ui-hook #'+pretty-code-setup-fira-ligatures)

(defun +pretty-code-setup ()
  (unless (or (eq major-mode 'fundamental-mode)
              (eq (get major-mode 'mode-class) 'special)
              (derived-mode-p 'comint-mode 'eshell-mode 'term-mode))
    (setq prettify-symbols-alist
          (append (cdr (assq major-mode +pretty-code-symbols-alist))
                  (default-value 'prettify-symbols-alist)))
    (when prettify-symbols-mode
      (prettify-symbols-mode -1))
    (prettify-symbols-mode +1)))
(add-hook 'after-change-major-mode-hook #'+pretty-code-setup)

(defun +pretty-code-set-symbols (modes &rest rest)
  (declare (indent defun))
  (dolist (mode (enlist modes))
    (if (null (car-safe rest))
        (delq (assq mode +pretty-code-symbols-alist)
              +pretty-code-symbols-alist)
      (let (results key)
        (while rest
          (setq key (pop rest))
          (unless (plist-member +pretty-code-symbols key)
            (user-error "Invalid keyword in +pretty-code-set-symbols: %s"
                        key))
          (let* ((sym (pop rest))
                 (char (plist-get +pretty-code-symbols key)))
            (push (cons sym char) results)))
        (delq (assq mode +pretty-code-symbols-alist)
            +pretty-code-symbols-alist)
        (push (cons mode results) +pretty-code-symbols-alist)))))

(setq prettify-symbols-unprettify-at-point 'right-edge)

;;
;; Electric Indent

(defvar-local +electric-indent-words '()
  "A list of 'electric' words. Typing these will trigger reindentation of the
current line.")

(after! electric
  (setq-default electric-indent-chars '(?\n ?\^?))

  (defun +electric-char (_c)
    (when (and (eolp) +electric-indent-words)
      (save-excursion
        (backward-word)
        (looking-at-p (concat "\\<" (regexp-opt +electric-indent-words))))))
  (add-to-list 'electric-indent-functions #'+electric-char nil #'eq)

  (defun +electric-set (modes &rest plist)
    "Declare :words (list of strings) or :chars (lists of chars) in MODES that
will trigger electric reindentation."
    (declare (indent defun))
    (dolist (mode (enlist modes))
      (let ((hook (intern (format "%s-hook" mode)))
            (fn   (intern (format "+electric-init-%s" mode))))
        (cond ((null (car-safe plist))
               (remove-hook hook fn)
               (unintern fn nil))
              ((fset fn
                     (lambda!
                      (cl-destructuring-bind (&key chars words) plist
                        (electric-indent-local-mode +1)
                        (if chars (setq electric-indent-chars chars))
                        (if words (setq +electric-indent-words words)))))
               (add-hook hook fn)))))))

(use-package highlight-numbers
  :hook (prog-mode . highlight-numbers-mode)
  :config
  (setq highlight-numbers-generic-regexp "\\_<[[:digit:]]+.*\\_>"))

(use-package highlight-escape-sequences
  :hook ((prog-mode conf-mode) . hes-mode))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode)
  :config
  (setq rainbow-delimiters-max-face-count 3))

(use-package flycheck
  :commands (flycheck-list-errors flycheck-buffer)
  :after-call (enter-buffer-hook after-find-file)
  :config
  (setq flycheck-check-syntax-automatically '(save idle-change mode-enabled))

  (after! evil
    (defun +flycheck-check-buffer ()
      "Flycheck buffer on ESC in normal mode."
      (when flycheck-mode
        (ignore-errors (flycheck-buffer))
        nil))
    (add-hook 'global-escape-hook #'+flycheck-check-buffer t)
    (add-hook 'evil-insert-state-exit-hook #'+flycheck-check-buffer)))

(use-package flycheck-popup-tip
  :commands (flycheck-popup-tip-show-popup flycheck-popup-tip-delete-popup)
  :init
  (defun +flycheck-show-popup (errors)
    "TODO"
    (flycheck-popup-tip-show-popup errors))
  
  (defun +flycheck-cleanup-popup ()
    "TODO"
    (when (display-graphic-p)
      (flycheck-popup-tip-delete-popup)))

  (define-minor-mode +flycheck-popup-mode
    "TODO"
    :lighter nil
    :group 'flycheck
    (require 'flycheck-popup-tip)
    (let ((hooks '(post-command-hook focus-out-hook)))
      (cond
       ((and +flycheck-popup-mode
             (not (eq flycheck-display-errors-function
                      #'+flycheck-show-popup)))
        (setq flycheck-popup-tip-old-display-function
              flycheck-display-errors-function
              flycheck-display-errors-function
              #'+flycheck-show-popup)
        (dolist (hook hooks)
          (add-hook hook #'+flycheck-cleanup-popup nil t)))
       ((and (not +flycheck-popup-mode)
             (eq flycheck-display-errors-function
                 #'+flycheck-show-popup))
        (setq flycheck-display-errors-function
              flycheck-popup-tip-old-display-function
              flycheck-popup-tip-old-display-function nil)
        (dolist (hook hooks)
          (remove-hook hook #'+flycheck-cleanup-popup t))))))
  (add-hook 'flycheck-mode-hook #'+flycheck-popup-mode)

  :config
  (setq flycheck-popup-tip-error-prefix "x "))

(use-package lsp-mode)

(use-package lsp-ui
  :init
  (add-hook 'lsp-mode-hook #'lsp-ui-mode)
  :config
  (general-def
    :keymaps '(c-mode-map c++-mode-map)
    :prefix "C-c l"
    "" '(nil :wk "LSP")
    "=" '(lsp-format-buffer
          :wk "Format Buffer")
    "a" '(lsp-execute-code-action
          :wk "Code Action")
    "l" '(lsp-ui-sideline-mode
          :wk "Sideline Mode")
    "d" '(lsp-ui-doc-mode
          :wk "Doc Mode")
    "e" '(lsp-ui-flycheck-list
          :wk "Diagnostics")
    "i" '(lsp-ui-imenu
          :wk "Imenu")
    "r" '(lsp-rename
          :wk "Rename")
    "R" '(lsp-restart-workspace
          :wk "Restart Workspace")
    "w" '(lsp-ui-peek-find-workspace-symbol
          :wk "Find Symbol in Workspace")))

(use-package company-lsp
  :after company)

(provide 'config-lang)
;;; config-lang.el ends here

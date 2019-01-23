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

(define-key! 'global
  (kbd "C-[")   #'previous-buffer
  (kbd "C-]")   #'next-buffer
  (kbd "C-x p") #'+popup-other
  (kbd "C-x `") #'+popup-toggle
  (kbd "C-x ~") #'+popup-raise)

(after! company
  (define-key! 'global
    (kbd "C-;") #'+company-complete)
  (define-key! company-active-map
    (kbd "C-o")       #'company-search-kill-others
    (kbd "C-n")       #'company-select-next
    (kbd "C-p")       #'company-select-previous
    (kbd "C-s")       #'company-search-candidates
    (kbd "M-s")       #'company-filter-candidates
    (kbd "<C-Tab>")   #'company-complete-common-or-cycle
    [tab]             #'company-complete-common-or-cycle
    [backtab]         #'company-select-previous
    (kbd "C-RET")     #'helm-company)
  (define-key! company-search-map
    (kbd "C-n")       #'company-search-repeat-forward
    (kbd "C-p")       #'company-search-repeat-backward
    (kbd "C-s")       (lambda! (company-search-abort) (company-filter-candidates))))

(after! smartparens
  (define-key! 'global
    (kbd "C-M-a")     #'sp-beginning-of-sexp
    (kbd "C-M-e")     #'sp-end-of-sexp
    (kbd "C-M-f")     #'sp-forward-sexp
    (kbd "C-M-b")     #'sp-backward-sexp
    (kbd "C-M-d")     #'sp-splice-sexp
    (kbd "C-M-k")     #'sp-kill-sexp
    (kbd "C-M-t")     #'sp-transpose-sexp
    (kbd "C-<right>") #'sp-forward-slurp-sexp
    (kbd "C-<left>")  #'sp-backward-slurp-sexp
    (kbd "M-<right>") #'sp-forward-barf-sexp
    (kbd "M-<left>")  #'sp-backward-barf-sexp))

;; (use-package general
;;   :defer 1
;;   :after-call pre-command-hook
;;   :init
;;   (general-define-key
;;    "C-["   #'previous-buffer
;;    "C-]"   #'next-buffer
;;    "C-x p" #'+popup-other
;;    "C-`"   #'+popup-toggle
;;    "C-~"   #'+popup-raise)

;;   (after! company
;;     (general-define-key
;;      "C-;"  #'+company-complete)
;;     (general-define-key
;;      company-active-map
;;      "C-o"        #'company-search-kill-others
;;      "C-n"        #'company-select-next
;;      "C-p"        #'company-select-previous
;;      "C-s"        #'company-search-candidates
;;      "M-s"        #'company-filter-candidates
;;      "<C-Tab>"    #'company-complete-common-or-cycle
;;      [tab]        #'company-complete-common-or-cycle
;;      [backtab]    #'company-select-previous
;;      "C-RET"      #'helm-company)
;;     (general-define-key
;;      company-search-map
;;      "C-n"        #'company-search-repeat-forward
;;      "C-p"        #'company-search-repeat-backward
;;      "C-s"        (lambda! (company-search-abort) (company-filter-candidates))))

;;   (after! smartparens
;;     (general-define-key
;;      "C-M-a"      #'sp-beginning-of-sexp
;;      "C-M-e"      #'sp-end-of-sexp
;;      "C-M-f"      #'sp-forward-sexp
;;      "C-M-b"      #'sp-backward-sexp
;;      "C-M-d"      #'sp-splice-sexp
;;      "C-M-k"      #'sp-kill-sexp
;;      "C-M-t"      #'sp-transpose-sexp
;;      "C-<right>"  #'sp-forward-slurp-sexp
;;      "C-<left>"   #'sp-backward-slurp-sexp
;;      "M-<right>"  #'sp-forward-barf-sexp
;;      "M-<left>"   #'sp-forward-barf-sexp)))

(add-hook! tty-setup-hook (define-key input-decode-map
                            (kbd "TAB") [tab]))

(define-key! (minibuffer-local-map
              minibuffer-local-ns-map
              minibuffer-local-completion-map
              minibuffer-local-must-match-map
              minibuffer-local-isearch-map
              read-expression-map)
  "\C-s" #'helm-minibuffer-history
  "\C-w" #'backward-kill-word
  "\C-u" #'backward-kill-sentence
  "\C-z" (lambda! (ignore-errors
                    (call-interactively #'undo)))

  (kbd "C-S-d") #'scroll-up-command
  (kbd "C-S-n") #'scroll-down-command)

(provide 'config-keybinds)
;;; config-keybinds.el ends here

(provide 'setup-convenience)

;; Replace dabbrev-expand with hippie-expand
(global-set-key (kbd "M-/") 'hippie-expand)
(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev                      ; expand from current buffer
        try-expand-dabbrev-from-all-buffers     ; expand from all other buffers
        try-expand-dabbrev-from-kill            ; expand from the kill ring
        try-complete-file-name-partially        ; complete text as file name
        try-complete-file-name
        try-expand-all-abbrevs                  ; expand word before point from all abbrev tables
        try-expand-list                         ; expand current line to entire line in buffer
        try-expand-line
        try-complete-lisp-symbol-partially      ; complete LISP symbol
        try-complete-lisp-symbol))

;; Highlight current-line
(global-hl-line-mode)

;; Always display ibuffer in different window
(setq ibuffer-use-other-window t)

;; Show line numbers if in a programming mode
(add-hook 'prog-mode-hook 'linum-mode)

;; Show useless whitespace
(add-hook 'prog-mode-hook '(lambda ()
                             (interactive)
                             (setq show-trailing-whitespace 1)))

;; Toggle whitespace
(global-set-key (kbd "C-c w") 'whitespace-mode)

;; Easier window navigation: Shift-{left,right,up,down}
(windmove-default-keybindings)

;;;;;;;;;;;;;;;;;;;;;
;;
;; Convenience Packages
;;
;;;;;;;;;;;;;;;;;;;;;

;; Company-mode
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)

(require 'expand-region)
(global-set-key (kbd "M-m") 'er/expand-region)

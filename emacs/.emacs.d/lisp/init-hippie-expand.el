;; Replace dabbrev with hippie-expand
(use-package hippie-exp
  ;; built-in
  :bind
  (([remap dabbrev-expand] . hippie-expand))
  :init
  (setq hippie-expand-try-functions-list
            '(try-expand-dabbrev                      ; expand from current
              try-expand-dabbrev-all-buffers          ; expand from all other buffers
              try-expand-dabbrev-from-kill            ; expand from the kill ring
              try-complete-file-name-partially        ; complete text as file name
              try-complete-file-name
              try-expand-all-abbrevs                  ; expand word before point from all abbrev tables
              try-expand-list                         ; expand current line to entire line in buffer
              try-expand-line
              try-complete-lisp-symbol-partially      ; complete LISP symbol
              try-complete-lisp-symbol)))

(provide 'init-hippie-expand)

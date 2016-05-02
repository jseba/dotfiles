;;; setup-editing -- Setup editing
;;; Commentary:
;;; Code:

(provide 'setup-editing)

(defun smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between first non-whitespace character and
the beginning of the line."

  (interactive "^p")
  (setq arg (or arg 1))

  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))
  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

;; Setup basic editing
(setq mode-require-final-newline t)              ; make sure the file ends in '\n'
(setq-default indent-tabs-mode nil)              ; use spaces instead of tabs
(setq-default tab-width 4)                       ; use 4 spaces for a tab character
(setq-default tab-always-indent 'complete)       ; make tab indent then complete
(set-terminal-coding-system 'utf-8)              ; use UTF-8 by default
(set-keyboard-coding-system 'utf-8)
(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)
(setq-default indent-tabs-mode nil)              ; don't replace spaces with tabs when formatting
(delete-selection-mode)                          ; when typing over a selected region, delete then insert
(global-set-key (kbd "RET") 'newline-and-indent) ; automatically indent on newline

;; Setup keymappings
(global-set-key [remap move-beginning-of-line]
                'smarter-move-beginning-of-line)
(global-set-key (kbd "C-a") 'smarter-move-beginning-of-line)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Editing Packages
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Volatile highlights
(require 'volatile-highlights)
(volatile-highlights-mode 1)

;; Smartparens
(require 'smartparens-config)
(setq sp-base-key-bindings 'paredit
      sp-autoskip-closing-pair 'always
      sp-hybrid-kill-entire-symbol nil)
(sp-use-paredit-bindings)

;; Undo-tree
(require 'undo-tree)
(global-undo-tree-mode)

(provide 'setup-editing)
;;; setup-editing.el ends here


(use-package editing-prefs
  :init
  (defun sanityinc/newline-at-end-of-line ()
    "Move to end of line, enter a newline and reindent."
    (interactive)
    (move-end-of-line 1)
    (newline-and-indent))

  (defun smart-open-line ()
    "Insert empty line after the current line."
    (interactive)
    (move-end-of-line nil)
    (newline-and-indent))

  (defun smart-kill-whole-line (&optional arg)
    "Kill whole line and move back to indentation."
    (interactive "p")
    (kill-whole-line arg)
    (back-to-indentation))

  (defun smart-backward-kill-line ()
    "Kill line backwards from point and re-indent."
    (interactive)
    (kill-line 0)
    (indent-according-to-mode))

  (defun smart-move-beginning-of-line (arg)
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

  (defun smart-back-to-indentation (arg)
    "Move point back to indentation of beginning of line.
  Move point to the first non-whitespace character on this line.
  If point is already there, move to the beginning of the line.
  Effectively toggle between the first non-whitespace character and
  the beginning of the line.
  If ARG is not nil or 1, move forward ARG - 1 lines first.  If
  point reaches the beginning or end of the buffer, stop there."
    (interactive "^p")
      (setq arg (or arg 1))

        ;; Move lines first
          (when (/= arg 1)
                (let ((line-move-visual nil))
                        (forward-line (1- arg))))

            (let ((orig-point (point)))
                  (back-to-indentation)
                      (when (= orig-point (point))
                              (move-beginning-of-line 1))))

  (defun kill-back-to-indentation ()
    "Kill from point back to the first non-whitespace character on the current line."
    (interactive)
    (let ((prev-pos (point)))
      (back-to-indentation)
      (kill-region (point) prev-pos)))

  (defun smart-backward-up-sexp (arg)
    "Jump up to the start of the ARG'th enclosing sexp."
    (interactive "p")
    (let ((ppss (syntax-ppss)))
      (cond
       ((elt ppss 3)
	(goto-char (elt ppss 8))
	(backward-up-sexp (1- arg)))
       (backward-up-list arg))))

  ;; Set editing modes
  (blink-cursor-mode -1)
  (delete-selection-mode)
  (transient-mark-mode)
  (line-number-mode t)
  (column-number-mode t)
  (size-indication-mode t)

  (add-hook 'text-mode-hook #'auto-fill-mode)

  :bind
  (("C-c x i" . indent-region)
   ([remap just-one-space] . cycle-spacing)
   ("RET" . newline-and-indent)
   ("S-RET" . sanityinc/newline-at-end-of-line)
   ([remap move-beginning-of-line] . smart-move-beginning-of-line)
   ([remap kill-whole-line] . smart-kill-whole-line)
   ([remap open-line] . smart-open-line)
   ([remap backward-up-sexp] . smart-backward-up-sexp)
   ("C-S-DEL" . smart-backward-kill-line)
   ([remap back-to-indentation] . smart-back-to-indentation))

  :diminish
  auto-fill-function)

(use-package undo-tree
  :ensure t
  :init (global-undo-tree-mode)
  :diminish undo-tree-mode)


(provide 'init-editing)

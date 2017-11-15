(use-package winner
  ;; built-in
  :init
  (winner-mode))

(use-package switch-window
  :ensure t
  :init
  (setq-default switch-window-shortcut-style 'alphabet
		switch-window-timeout nil)
  :bind
  (([remap other-window] . switch-window)))

(use-package windmove
  ;; built-in
  :init
  (defun split-window-horizontally-with-other-buffer (&optional arg)
    "Split this window horizontally and switch to the new window unless ARG is provided."
    (interactive "P")
    (split-window-horizontally)
    (let ((target-window (next-window)))
      (set-window-buffer target-window (other-buffer))
      (unless arg
	(select-window target-window))))
  (defun split-window-vertically-with-other-buffer (&optional arg)
    "Split this window vertically and switch to the new window unless ARG is provided."
    (interactive "P")
    (split-window-vertically)
    (let ((target-window (next-window)))
      (set-window-buffer target-window (other-buffer))
      (unless arg
	(select-window target-window))))
  (defun sanityinc/toggle-delete-other-windows ()
    "Delete other windows in frame (if any) or restore previous window config."
    (interactive)
    (if (and winner-mode
	     (equal (selected-window) (next-window)))
	(winner-undo)
      (delete-other-windows)))
  (defun split-window-horizontally-instead ()
    (interactive)
    (save-excursion
      (delete-other-windows)
      (split-window-horizontally-with-other-buffer)))
  (defun split-window-vertically-instead ()
    (interactive)
    (save-excursion
      (delete-other-windows)
      (split-window-vertically-with-other-buffer)))
  (defun sanityinc/split-window ()
    "Split the window to see the most recent buffer in the other window.
Call a second time to restore the orignal window configuration."
    (interactive)
    (if (eq last-command 'sanityinc/split-window)
	(progn
	  (jump-to-register :sanityinc/split-window)
	  (setq this-command 'sanityinc/unsplit-window))
      (window-configuration-to-register :sanityinc/split-window)
      (switch-to-buffer-other-window nil)))
  (defun sanityinc/toggle-current-window-dedication ()
    "Toggle whether the current window is dedicated to its current buffer."
    (interactive)
    (let* ((window (selected-window))
	   (was-dedicated (window-dedicated-p window)))
      (set-window-dedicated-p window (not was-dedicated))
      (message "Window %sdedicated to %s"
	       (if was-dedicated "no longer " "")
	       (buffer-name))))
  (unless (memq window-system '(nt w32))
    (windmove-default-keybindings 'control))
  :bind
  (([remap split-window-vertically] . split-window-vertically-with-other-buffer)
   ([remap split-window-horizontally] . split-window-horizontally-with-other-buffer)
   ([remap delete-other-windows] . sanityinc/toggle-delete-other-windows)
   ("C-c w |" . split-window-horizontally-instead)
   ("C-c w _" . split-window-vertically-instead)
   ("C-c w S" . sanityinc/split-window)
   ("C-c w d" . sanityinc/toggle-current-window-dedication)))

(provide 'init-windows)

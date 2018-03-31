;;;  -*- lexical-binding: t; -*-

;;;###autoload
(defun nx/split-window-horizontally-with-other-buffer (&optional arg)
  "Split this window horizontally and switch to the new window unless ARG is provided."
  (interactive "P")
  (split-window-horizontally)
  (let ((target-window (next-window)))
    (set-window-buffer target-window (other-window 1))
    (unless arg
      (select-window target-window))))

;;;###autoload
(defun nx/split-window-vertically-with-other-buffer (&optional arg)
  "Split this window vertically and switch to the new window unless ARG is provided."
  (interactive "P")
  (split-window-vertically)
  (let ((target-window (next-window)))
    (set-window-buffer target-window (other-buffer 1))
    (unless arg
      (select-window target-window))))

;;;###autoload
(defun nx/toggle-delete-other-windows ()
  "Delete other windows in frame (if any) or restore previous window config."
  (interactive)
  (if (and winner-mode
	       (equal (selected-window) (next-window)))
      (winner-undo)
    (delete-other-windows)))

;;;###autoload
(defun nx/split-window-horizontally-instead ()
  "Change window split to horizontal."
  (interactive)
  (save-excursion
    (delete-other-windows)
    (nx/split-window-horizontally-with-other-buffer)))

;;;###autoload
(defun nx/split-window-vertically-instead ()
  "Change window split to vertical."
  (interactive)
  (save-excursion
    (delete-other-windows)
    (nx/split-window-vertically-with-other-buffer)))

;;;###autoload
(defun nx/split-window ()
  "Split the window to see the most recent buffer in the other window.
Call a second time to restore the orignal window configuration."
  (interactive)
  (if (eq last-command 'nx/split-window)
      (progn
	    (jump-to-register :nx/split-window)
	    (setq this-command 'nx/unsplit-window))
    (window-configuration-to-register :nx/split-window)
    (switch-to-buffer-other-window nil)))

;;;###autoload
(defun nx/toggle-current-window-dedication ()
  "Toggle whether the current window is dedicated to its current buffer."
  (interactive)
  (let* ((window (selected-window))
	     (was-dedicated (window-dedicated-p window)))
    (set-window-dedicated-p window (not was-dedicated))
    (message "Window %sdedicated to %s"
	         (if was-dedicated "no longer " "")
	         (buffer-name))))

;;;###autoload
(defun nx/kill-this-buffer ()
  "Kill the current buffer."
  (interactive)
  (kill-buffer (current-buffer)))


;;; lisp/autoloads-ui.el -*- lexical-binding: t; -*-

(defun nx/toggle-fullscreen ()
  "Toggle fullscreen Emacs."
  (interactive)
  (set-frame-parameter
   nil
   'fullscreen
   (unless (frame-parameter nil 'fullscreen)
     'fullboth)))

(defun nx/toggle-line-numbers (&optional arg)
  "Toggle `linum-mode'."
  (interactive "P")
  (cond ((boundp 'display-line-numbers)
	 (setq display-line-numbers
	       (pcase arg
		 ('(4) 'relative)
		 (1 t)
		 (-1 nil)
		 (_ (not display-line-numbers)))))
	((featurep 'nlinum)
	 (nlinum-mode (or arg (if nlinum-mode -1 +1))))
	(t
	 (error "No line number plugin detected."))))

(defun nx-resize-window (window new-size &optional horizontal force-p)
  "Resize a window to NEW-SIZE. If HORIZONTAL, do it width-wise."
  (with-selected-window (or window (selected-window))
    (let ((window-size-fixed (unless force-p window-size-fixed)))
      (enlarge-window (- new-size (if horizontal (window-width) (window-height)))
		      horizontal))))

(defun nx/window-zoom ()
  "Maximize and isolate the current buffer. Activate again to undo.

If the window changes before then, the undo expires."
  (interactive)
  (if (and (one-window-p)
	   (assoc ?_ register-alist))
      (jump-to-register ?_)
    (window-configuration-to-register ?_)
    (delete-other-windows)))

(defvar nx--window-enlargened-p nil)
(defun nx/window-enlargen ()
  "Enlargen the current window. Activate again to undo."
  (interactive)
  (setq nx--window-enlargened-p
	(if (and nx--window-enlargened-p
		 (assoc ?_ register-alist))
	    (ignore (jump-to-register ?_))
	  (window-configuration-to-register ?_)
	  (nx-resize-window nil (truncate (/ (frame-width) 1.2)) t)
	  (nx-resize-window nil (truncate (/ (frame-heigth) 1.2)))
	  t)))

(defun nx/delete-frame ()
  "Delete the current-frame, but confirm if it isn't empty."
  (interactive)
  (if (cdr (frame-alist))
      (when (nx-quit-p "Close frame?")
	(delete-frame))
    (save-buffers-kill-emacs)))

(defun nx/split-window-horizontally-with-other-buffer (&optional arg)
  "Split this window horizontally and switch to the new window unless ARG is provided."
  (interactive "P")
  (split-window-horizontally)
  (let ((target-window (next-window)))
    (set-window-buffer target-window (other-window))
    (unless arg
      (select-window target-window))))

(defun nx/split-window-vertically-with-other-buffer (&optional arg)
  "Split this window vertically and switch to the new window unless ARG is provided."
  (interactive "P")
  (split-window-vertically)
  (let ((target-window (next-window)))
    (set-window-buffer target-window (other-buffer))
    (unless arg
      (select-window target-window))))

(defun nx/toggle-delete-other-windows ()
  "Delete other windows in frame (if any) or restore previous window config."
  (interactive)
  (if (and winner-mode
	   (equal (selected-window) (next-window)))
      (winner-undo)
    (delete-other-windows)))

(defun nx/split-window-horizontally-instead ()
  (interactive)
  (save-excursion
    (delete-other-windows)
    (split-window-horizontally-with-other-buffer)))

(defun nx/split-window-vertically-instead ()
  (interactive)
  (save-excursion
    (delete-other-windows)
    (split-window-vertically-with-other-buffer)))

(defun nx/split-window ()
  "Split the window to see the most recent buffer in the other window.
Call a second time to restore the orignal window configuration."
  (interactive)
  (if (eq last-command 'sanityinc/split-window)
      (progn
	(jump-to-register :sanityinc/split-window)
	(setq this-command 'sanityinc/unsplit-window))
    (window-configuration-to-register :sanityinc/split-window)
    (switch-to-buffer-other-window nil)))

(defun nx/toggle-current-window-dedication ()
  "Toggle whether the current window is dedicated to its current buffer."
  (interactive)
  (let* ((window (selected-window))
	 (was-dedicated (window-dedicated-p window)))
    (set-window-dedicated-p window (not was-dedicated))
    (message "Window %sdedicated to %s"
	     (if was-dedicated "no longer " "")
	     (buffer-name))))

(define-minor-mode nx-big-font-mode
  "A global mdoe that resizes the font."
  :init-value nil
  :lighter " BIG"
  :global t
  (unless (fontp nx-big-font)
    (user-error "`nx-big-font is not set to a valid font"))
  (if nx-big-font-mode
      (set-frame-font nx-big-font t t)
    (set-frame-font nx-font t t)))

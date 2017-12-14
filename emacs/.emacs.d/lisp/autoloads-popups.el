;;; lisp/autoloads-popups.el -*- lexical-binding: t; -*-

(defun nx-popup-p (&optional target)
  "Returns t if TARGET (a window or buffer) is a popup."
  (-when-let (target (or target (selected-window)))
	    (cond
	     ((bufferp target)
	      (and (buffer-live-p target)
		   (buffer-local-value 'nx-popup-mode target)))
	     ((windowp target)
	      (and (window-live-p target)
		   (window-parameter target 'popup))))))

(defun nx-popup-buffer (buffer &optional plist extend-p)
  "Display BUFFER in a `shackle' with PLIST rules.

If EXTEND-P is non-nil, don't overwrite the original rules
for this popup, just the specified properties."
  (declare (indent defun))
  (unless (bufferp buffer)
    (error "%s is not a valid buffer" buffer))
  (shackle-display-buffer buffer
			  nil
			  (or (if extend-p
				  (append plist (shackle-match buffer))
				plist)
			      (shackle-match buffer))))

(defun nx-popup-switch-to-buffer (buffer)
  "Switch the current (or closest) pop-up window to BUFFER."
  (unless (nx-popup-p)
    (if-let (popups (nx-popup-windows))
	    (select-window (car popups))
	    (error "No popups to switch to"))
    (set-window-dedicated-p nil nil)
    (switch-to-buffer buffer nil t)
    (prog1 (selected-window)
      (set-window-dedicated-p nil t))))

(defun nx-popup-fit-to-buffer (&optional window max-size)
  "Fit WINDOW to the size of its content."
  (unless (string-empty-p (buffer-string))
    (let* ((window-size (nx-popup-size window))
           (max-size (or max-size (nx-popup-property :size window)))
           (size (+ 2 (if (floatp max-size) (truncate (* max-size window-size)) window-size))))
      (fit-window-to-buffer window size nil size))))

(defun nx-popup-move (direction)
  "Move a popup window to another side of the frame, in DIRECTION, which can be
one of the following: 'left 'right 'above 'below"
  (when (nx-popup-p)
    (let ((buffer (current-buffer))
          (nx-popup-inhibit-autokill t))
      (nx/popup-close)
      (nx-popup-buffer buffer `(:align ,direction) 'extend))))

(defun nx-popup-file (file &optional plist extend-p)
  "Display FILE in a shackle popup, with PLIST rules. See `shackle-rules' for
possible rules."
  (unless (file-exists-p file)
    (user-error "Can't display file in popup, it doesn't exist: %s" file))
  (nx-popup-buffer (find-file-noselect file t) plist extend-p))

(defun nx-popup-windows (&optional filter-static-p)
  "Get a list of open pop up windows."
  (cl-loop for window in nx-popup-windows
           if (and (nx-popup-p window)
                   (not (and filter-static-p
                             (nx-popup-property :static window))))
           collect window))

(defun nx-popup-properties (window-or-buffer)
  "Returns a window's popup property list, if possible. The buffer-local
`nx-popup-rules' always takes priority, but this will fall back to the popup
window parameter."
  (cond ((windowp window-or-buffer)
         (or (window-parameter window-or-buffer 'popup)
             (nx-popup-properties (window-buffer window-or-buffer))))
        ((bufferp window-or-buffer)
         (buffer-local-value 'nx-popup-rules window-or-buffer))))

(defun nx-popup-property (prop &optional window)
  "Returns a `nx-popup-rules' PROPerty from WINDOW."
  (or (plist-get (nx-popup-properties (or window (selected-window)))
                 prop)
      (pcase prop
        (:size  shackle-default-size)
        (:align shackle-default-alignment))))

(defun nx-popup-side (&optional window)
  "Return what side a popup WINDOW came from ('left 'right 'above or 'below)."
  (let ((align (nx-popup-property :align window)))
    (when (eq align t)
      (setq align shackle-default-alignment))
    (when (functionp align)
      (setq align (funcall align)))
    align))

(defun nx-popup-size (&optional window)
  "Return the size of a popup WINDOW."
  (pcase (nx-popup-side window)
    ((or 'left 'right)  (window-width window))
    ((or 'above 'below) (window-height window))))

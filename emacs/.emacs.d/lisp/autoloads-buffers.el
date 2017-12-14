;;; lisp/autoloads-buffers.el -*- lexical-binding: t; -*-

(defvar-local nx-buffer--narrowed-origin nil)

(defvar nx-real-buffer-functions '()
  "A list of functions to run in order to determine if a buffer is real.")
(defvar-local nx-real-buffer-p nil
  "If non-nil, this buffer should be considered real, regardless.")

(defvar nx-fallback-buffer "*scratch*"
  "The name of the buffer to fall back to if no other buffers exist.")

(defun nx-fallback-buffer ()
  "Returns the fallback buffer, creating it if necessary."
  (get-buffer-create nx-fallback-buffer))

(defun nx/narrow-buffer (beg end &optional clone-p)
  "Restrict editing in this buffer to the current region, indirectly.

With CLONE-P, clone the buffer and hard-narrow the region. If mark isn't
active, then widen the buffer if narrowed."
  (interactive "r")
  (cond ((region-active-p)
	 (deactivate-mark)
	 (when clone-p
	   (let ((old-buf (current-buffer)))
	     (switch-to-buffer (clone-indirect-buffer nil nil))
	     (setq nx-buffer--narrowed-origin old-buf)))
	 (narrow-to-region beg end))
	(nx-buffer--narrowed-origin
	 (kill-this-buffer)
	 (switch-to-buffer nx-buffer--narrowed-origin)
	 (setq nx-buffer--narrowed-origin nil))
	(t
	 (widen))))

(defalias 'nx-buffer-list #'buffer-list)

(defun nx-project-buffer-list ()
  "Return a list of buffers belonging to the current project.

If no project is active, return all buffers."
  (let ((buffers (doom-buffer-list)))
    (if-let (project-root (if (nx-project-p) (nx-project-root)))
        (cl-loop for buf in buffers
                 if (projectile-project-buffer-p buf project-root)
                 collect buf)
	buffers)))

(defun nx-real-buffer-list (&optional buffer-list)
  "Return a list of buffers that satify `doom-real-buffer-p'."
  (cl-loop for buf in (or buffer-list (nx-buffer-list))
           if (nx-real-buffer-p buf)
	   collect buf))

(defun nx-buffers-in-mode (modes &optional buffer-list derived-p)
  "Return a list of buffers whose `major-mode' is `eq' to MODE(S).
If DERIVED-P, test with `derived-mode-p', otherwise use `eq'."
  (let ((modes (nx-enlist modes)))
    (cl-remove-if-not (if derived-p
                          (lambda (buf)
                            (with-current-buffer buf
                              (apply #'derived-mode-p modes)))
                        (lambda (buf)
                          (memq (buffer-local-value 'major-mode buf) modes)))
                      (or buffer-list (nx-buffer-list)))))

(defun nx-visible-windows (&optional window-list)
  "Return a list of the visible, non-popup windows."
  (cl-loop for win in (or window-list (window-list))
           unless (nx-popup-p win)
           collect win))

(defun nx-visible-buffers (&optional buffer-list)
  "Return a list of visible buffers (i.e. not buried)."
  (cl-loop for buf in (or buffer-list (nx-buffer-list))
           when (get-buffer-window buf)
           collect buf))

(defun nx-buried-buffers (&optional buffer-list)
  "Get a list of buffers that are buried."
  (cl-loop for buf in (or buffer-list (nx-buffer-list))
           unless (get-buffer-window buf)
           collect buf))

(defun nx-matching-buffers (pattern &optional buffer-list)
  "Get a list of all buffers that match the regex PATTERN."
  (cl-loop for buf in (or buffer-list (nx-buffer-list))
           when (string-match-p pattern (buffer-name buf))
	   collect buf))

(defun nx--cycle-real-buffers (&optional n)
  "Switch to the next buffer N times (previous, if N < 0), skipping over unreal
buffers. If there's nothing left, switch to `nx-fallback-buffer'. See
`nx-real-buffer-p' for what 'real' means."
  (let ((buffers (delq (current-buffer) (nx-real-buffer-list)))
        (project-dir (nx-project-root)))
    (cond ((or (not buffers)
               (zerop (% n (1+ (length buffers)))))
           (switch-to-buffer (nx-fallback-buffer) nil t))
          ((= (length buffers) 1)
           (switch-to-buffer (car buffers) nil t))
          (t
           ;; Why this instead of switching straight to the Nth buffer in
           ;; BUFFERS? Because `switch-to-next-buffer' and
           ;; `switch-to-prev-buffer' properly update buffer list order.
           (cl-loop with move-func =
                    (if (> n 0) #'switch-to-next-buffer #'switch-to-prev-buffer)
                    for i to 20
                    while (not (memq (current-buffer) buffers))
                    do
                    (dotimes (_i (abs n))
                      (funcall move-func)))))
    (when (eq (current-buffer) (nx-fallback-buffer))
      (cd project-dir))
    (current-buffer)))

(defun nx-real-buffer-p (&optional buffer-or-name)
  "Returns t if BUFFER-OR-NAME is a 'real' buffer. The complete criteria for a
real buffer is:
  1. The buffer-local value of `nx-real-buffer-p' (variable) is non-nil OR
  2. Any function in `nx-real-buffer-functions' must return non-nil when
     passed this buffer OR
  3. The current buffer:
     a) has a `buffer-file-name' defined AND
     b) is not in a popup window (see `nx-popup-p') AND
     c) is not a special buffer (its name isn't something like *Help*)
If BUFFER-OR-NAME is omitted or nil, the current buffer is tested."
  (-when-let (buf (ignore-errors (window-normalize-buffer buffer-or-name)))
    (or (buffer-local-value 'nx-real-buffer-p buf)
        (run-hook-with-args-until-success 'nx-real-buffer-functions buf)
        (not (or (nx-popup-p buf)
                 (minibufferp buf)
                 (string-match-p "^\\s-*\\*" (buffer-name buf))
                 (not (buffer-file-name buf)))))))

(defun nx/next-buffer ()
  "Switch to the next real buffer, skipping non-real buffers. See
`nx-real-buffer-p' for what 'real' means."
  (interactive)
  (nx--cycle-real-buffers +1))

(defun nx/previous-buffer ()
  "Switch to the previous real buffer, skipping non-real buffers. See
`nx-real-buffer-p' for what 'real' means."
  (interactive)
  (nx--cycle-real-buffers -1))

(defun nx-kill-buffer (&optional buffer dont-save)
  "Kill BUFFER (falls back to current buffer if omitted) then switch to a real
buffer. If the buffer is present in another window, only bury it.
Will prompt to save unsaved buffers when attempting to kill them, unless
DONT-SAVE is non-nil.
See `nx-real-buffer-p' for what 'real' means."
  (setq buffer (or buffer (current-buffer)))
  (when (and (bufferp buffer) (buffer-live-p buffer))
    (let ((buffer-win (get-buffer-window buffer))
          (only-buffer-window-p (= 1 (length (get-buffer-window-list buffer nil t)))))
      ;; deal with unsaved buffers
      (when (and only-buffer-window-p
                 (buffer-file-name buffer)
                 (buffer-modified-p buffer))
        (with-current-buffer buffer
          (if (and (not dont-save)
                   (yes-or-no-p "Buffer is unsaved, save it?"))
              (save-buffer)
            (set-buffer-modified-p nil))))
      (if buffer-win
          ;; deal with dedicated windows
          (if (window-dedicated-p buffer-win)
              (unless (window--delete buffer-win t t)
                (split-window buffer-win)
                (window--delete buffer-win t t))
            ;; cycle to a real buffer
            (with-selected-window buffer-win
              (nx--cycle-real-buffers -1)
              (when buffer-win
                (unrecord-window-buffer buffer-win buffer))
              (when only-buffer-window-p
                (kill-buffer buffer)))
            (not (eq (current-buffer) buffer)))
        (kill-buffer buffer)))))

(defun nx-force-kill-buffer (&optional buffer dont-save)
  "Kill BUFFER globally and ensure all windows previously showing BUFFER have
switched to a real buffer."
  (interactive)
  (let* ((buffer (or buffer (current-buffer)))
         (windows (get-buffer-window-list buffer nil t)))
    (nx-kill-buffer buffer dont-save)
    (dolist (win windows)
      (with-selected-window win
        (unless (nx-real-buffer-p)
          (nx/previous-buffer))))))

(defun nx-kill-buffer-and-windows (buffer)
  "Kill the buffer and delete all the windows it's displayed in."
  (dolist (window (get-buffer-window-list buffer))
    (unless (one-window-p t)
      (delete-window window)))
  (kill-buffer buffer))

(defun nx-kill-process-buffers ()
  "Kill all processes that have no visible associated buffers. Return number of
processes killed."
  (interactive)
  (let ((n 0))
    (dolist (p (process-list))
      (let ((process-buffer (process-buffer p)))
        (when (and (process-live-p p)
                   (not (string= (process-name p) "server"))
                   (or (not process-buffer)
                       (and (bufferp process-buffer)
                            (not (buffer-live-p process-buffer)))))
          (delete-process p)
          (cl-incf n))))
    n))

(defun nx-kill-matching-buffers (pattern &optional buffer-list)
  "Kill all buffers (in current workspace OR in BUFFER-LIST) that match the
regex PATTERN. Returns the number of killed buffers."
  (let ((buffers (nx-matching-buffers pattern buffer-list)))
    (dolist (buf buffers (length buffers))
      (nx-kill-buffer buf t))))

(defun nx/kill-this-buffer ()
  "Use `nx-kill-buffer' on the current buffer."
  (interactive)
  (when (and (not (nx-kill-buffer)) (called-interactively-p 'interactive))
    (message "Nowhere left to go!")))

(defun nx/kill-all-buffers (&optional project-p)
  "Kill all buffers and closes their windows.
If PROJECT-P (universal argument), kill only buffers that belong to the current
project."
  (interactive "P")
  (nx/popup-kill-all)
  (let ((buffers (if project-p (nx-project-buffer-list) (nx-buffer-list))))
    (mapc #'nx-kill-buffer-and-windows buffers)
    (unless (nx-real-buffer-p)
      (switch-to-buffer (nx-fallback-buffer)))
    (message "Killed %s buffers" (length buffers))))

(defun nx/kill-other-buffers (&optional project-p)
  "Kill all other buffers (besides the current one).
If PROJECT-P (universal argument), kill only buffers that belong to the current
project."
  (interactive "P")
  (let ((buffers (if project-p (nx-project-buffer-list) (nx-buffer-list)))
        (current-buffer (current-buffer)))
    (dolist (buf buffers)
      (unless (eq buf current-buffer)
        (nx-kill-buffer-and-windows buf)))
    (when (called-interactively-p 'interactive)
      (message "Killed %s buffers" (length buffers)))))

(defun nx/kill-matching-buffers (pattern &optional project-p)
  "Kill buffers that match PATTERN in BUFFER-LIST.
If PROJECT-P (universal argument), only kill matching buffers in the current
project."
  (interactive
   (list (read-regexp "Buffer pattern: ")
         current-prefix-arg))
  (let* ((buffers (if project-p (nx-project-buffer-list) (nx-buffer-list)))
         (n (nx-kill-matching-buffers pattern buffers)))
    (when (called-interactively-p 'interactive)
      (message "Killed %s buffers" n))))

(defun nx/cleanup-buffers (&optional all-p)
  "Clean up buried and inactive process buffers in the current workspace."
  (interactive "P")
  (let ((buffers (nx-buried-buffers (if all-p (buffer-list))))
        (n 0))
    (mapc #'kill-buffer buffers)
    (setq n (+ n (length buffers) (nx-kill-process-buffers)))
    (when (called-interactively-p 'interactive)
      (message "Cleaned up %s buffers" n))))

(defun nx-set-buffer-real (buffer flag)
  "Forcibly mark a buffer's real property, no matter what."
  (with-current-buffer buffer
    (setq nx-real-buffer-p flag)))

(provide 'autoloads-buffers)

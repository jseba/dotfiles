;;; config-buffers.el

(defvar exit-buffer-hook nil)
(defvar enter-buffer-hook nil)
(defvar inhibit-switch-buffer-hooks nil)

(defvar exit-window-hook nil)
(defvar enter-window-hook nil)
(defvar inhibit-switch-window-hooks nil)

(defun switch-buffer-hooks (orig-fn buffer-or-name &rest args)
  (if (or inhibit-switch-buffer-hooks
          (eq (get-buffer buffer-or-name) (current-buffer)))
      (apply orig-fn buffer-or-name args)
    (let ((inhibit-switch-buffer-hooks t))
      (run-hooks 'exit-buffer-hook)
      (prog1 (apply orig-fn buffer-or-name args)
        (with-current-buffer buffer-or-name
          (run-hooks 'enter-buffer-hook))))))

(defun switch-window-hooks (orig-fn window &optional norecord)
  (if (or inhibit-switch-window-hooks
          (null window)
          (eq window (selected-window))
          (window-minibuffer-p)
          (window-minibuffer-p window))
      (funcall orig-fn window norecord)
    (let ((inhibit-switch-window-hooks t))
      (run-hooks 'exit-window-hook)
      (prog1 (funcall orig-fn window norecord)
        (with-selected-window window
            (run-hooks 'enter-window-hook))))))

(defun setup-switch-buffer-hooks (&optional disable)
  (dolist (fn '(switch-to-buffer display-buffer pop-to-buffer))
    (if disable
        (advice-remove fn #'switch-buffer-hooks)
      (advice-add fn :around #'switch-buffer-hooks))))

(defun setup-switch-window-hooks (&optional disable)
  (if disable
      (advice-remove 'select-window #'switch-window-hooks)
    (advice-add 'select-window :around #'switch-window-hooks)))

(add-hook! '%-init-hook #'(setup-switch-buffer-hooks
                           setup-switch-window-hooks))

(defvar real-buffer-functions #'(dired-buffer-p))

(defvar unreal-buffer-functions
  #'(minibufferp special-buffer-p non-file-visiting-buffer-p))

(defvar-local real-buffer-p nil)

(defvar fallback-buffer "*scratch*")

(defun buffer-frame-predicate (buf)
  (or (real-buffer-p buf)
      (eq buf (fallback-buffer))))

(defun fallback-buffer ()
  (get-buffer-create fallback-buffer))

(defun dired-buffer-p (buf)
  (with-current-buffer buf (derived-mode-p 'dired-mode)))

(defun special-buffer-p (buf)
  (and (equal (substring (buffer-name buf) 0 1) "*")
       (equal (substring (buffer-name buf) -1)  "*")))

(defun temp-buffer-p (buf)
  (equal (substring (buffer-name buf) 0 1) " "))

(defun non-file-visiting-buffer-p (buf)
  (not (buffer-file-name buf)))

(defun real-buffer-list (&optional buffer-list)
  (cl-remove-if-not #'real-buffer-p (or buffer-list (buffer-list))))

(defun real-buffer-p (buffer-or-name)
  (or (bufferp buffer-or-name)
      (stringp buffer-or-name)
      (signal 'wrong-type-argument (list '(bufferp stringp) buffer-or-name)))
  (when-let* ((buf (get-buffer buffer-or-name)))
    (and (not (temp-buffer-p buf))
         (or (buffer-local-value 'real-buffer-p buf)
             (run-hook-with-args-until-success 'real-buffer-functions buf)
             (not (run-hook-with-args-until-success 'unreal-buffer-functions buf))))))

(defun unreal-buffer-p (buffer-or-name)
  (not (real-buffer-p buffer-or-name)))

(defun set-buffer-real-p (buffer flag)
  (with-current-buffer buffer
    (setq real-buffer-p flag)))

(defun mark-buffer-real ()
  (set-buffer-real-p (current-buffer) t))

(add-to-list 'default-frame-alist (cons 'buffer-predicate #'buffer-frame-predicate))

(defun visible-buffer-list (&optional buffer-list)
  (cl-loop for buf in (or buffer-list (buffer-list))
           when (get-buffer-window buf)
           collect buf))

(defun buried-buffers (&optional buffer-list)
  (cl-remove-if #'get-buffer-window (or buffer-list (buffer-list))))

(defun protect-fallback-buffer ()
  (not (eq (current-buffer) (fallback-buffer))))

(defun protect-visible-buffers ()
  (not (and (delq (selected-window) (get-buffer-window-list nil nil t))
            (not (member (substring (buffer-name) 0 1) '(" " "*"))))))

(defun switch-to-fallback-buffer-maybe (orig-fn)
  (let ((buf (current-buffer)))
    (cond ((window-dedicated-p)
           (delete-window))
          ((eq buf (fallback-buffer))
           (message "Not killing the fallback buffer"))
          ((real-buffer-p buf)
           (if (and buffer-file-name
                    (buffer-modified-p buf)
                    (not (y-or-n-p
                          (format "Buffer %s is modified; kill anyway?" buf))))
               (message "Aborted")
             (set-buffer-modified-p nil)
             (when (or (not (cl-set-difference (real-buffer-list)
                                               (visible-buffer-list)))
                       (memq (previous-buffer) (list buf 'nil)))
               (switch-to-buffer (fallback-buffer)))
             (kill-buffer buf)))
          ((funcall orig-fn)))))

(add-hook! 'kill-buffer-query-functions #'(protect-fallback-buffer
                                           protect-visible-buffers))
(advice-add #'kill-this-buffer :around #'switch-to-fallback-buffer-maybe)

(defun visible-window-list (&optional window-list)
  (cl-loop for window in (or window-list (window-list))
           when (or (window-parameter window 'visible)
                    (not (window-dedicated-p window)))
           collect window))

(defun kill-buffer-and-windows (buffer)
  "Kill BUFFER and delete all windows it is displayed in."
  (dolist (window (get-buffer-window-list))
    (unless (one-window-p t)
      (delete-window window)))
  (kill-buffer buffer))

(defun kill-all-buffers (&optional projectp)
  "Kill all buffers and closes their windows.

If PROJECTP, only kill buffers that belong to the current project."
  (interactive "P")
  (unless projectp
    (delete-other-windows))
  (switch-to-buffer (fallback-buffer)))

(defun kill-other-buffers (&optional projectp)
  "Kill all other buffers besides the current one.

If PROJECTP, only kill buffers that belong to the current project."
  (interactive "P")
  (let ((buffers (if projectp (+projectile-buffer-list) (buffer-list)))
        (current-buffer (current-buffer)))
    (dolist (buf buffers)
      (unless (eq buf current-buffer)
        (kill-buffer-and-windows buf)))
    (when (called-interactively-p 'interactive)
      (message "Killed %s buffers" (length buffers)))))

(provide 'config-buffers)
;;; config-buffers.el ends here

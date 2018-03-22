;;; config-files.el -- File management

;;; Code:
;;; Commentary:

(defun nx/delete-this-file ()
  "Delete the current file in this buffer and kill the buffer."
  (interactive)
  (let ((filename (buffer-file-name)))
    (when (yes-or-no-p (format "Really delete '%s'?"
			                   (file-name-nondirectory buffer-file-name)))
	  (cond
	   ((not filename) (kill-this-buffer))
	   ((vc-backend filename) (vc-delete-file filename))
	   (t
	    (delete-file filename)
	    (kill-this-buffer))))))

(defun nx/rename-this-file-and-buffer ()
  "Rename the current file and buffer."
  (interactive)
  (let* ((filename (buffer-file-name))
         (old-name (if filename
                       (file-name-nondirectory filename)
                     (buffer-name)))
         (new-name (read-file-name "New name: " nil nil nil old-name)))
    (cond
     ((not (and filename (file-exists-p filename))) (rename-buffer new-name))
     ((vc-backend filename) (vc-rename-file filename new-name))
     (t
      (rename-file filename new-name 'force-overwrite)
      (set-visited-file-name new-name 'no-query 'along-with-file)))))

(defun nx/copy-filename-as-kill (&optional arg)
  "Copy the name of the currently visited file to kill ring.

With a zero prefix ARG, copy the absolute file name.  With
\\[universal-argument], copy the file name relative to the
current Projectile project, or to the current buffer's
`default-directory', if the file is not part of any project.
Otherwise copy the non-directory part only."
  (interactive "P")
  (if-let* ((file-name (current-file))
            (name-to-copy
             (cond
              ((zerop (prefix-numeric-value arg)) file-name)
              ((consp arg)
               (let* ((projectile-require-project-root nil)
                      (directory (and (fboundp 'projectile-project-root)
                                      (projectile-project-root))))
                 (file-relative-name file-name directory)))
              (t (file-name-nondirectory file-name)))))
	   (progn
             (kill-new name-to-copy)
             (message "%s" name-to-copy))
	   (user-error "This buffer is not visiting a file")))

(defun nx/launch-dwim ()
  "Open the current file externally."
  (interactive)
  (if (derived-mode-p 'dired-mode)
      (let ((marked-files (dired-get-marked-files)))
        (if marked-files
            (launch-files marked-files 'confirm)
          (launch-directory (dired-current-directory))))
    (if (buffer-file-name)
        (launch-file (buffer-file-name))
      (user-error "The current buffer is not visiting a file"))))

(use-package dired
  :init
  (setq dired-auto-revert-buffer t
        dired-listing-switches "-alhF --group-directories-first -v"
        dired-ls-F-marks-symlinks t
        dired-recursive-copies 'always
        dired-recursive-deletes 'top    ;; Only ask for top dir when deleting
        dired-dwim-target t))           ;; Use the dired buffer in other window if it exists

(use-package dired-x
  :after dired
  :init
  (setq dired-dwim-target t)
  (add-hook 'dired-mode-hook #'dired-omit-mode)
  :config
  (put 'dired-find-alternate-file 'disabled nil)  ;; Re-use current buffer when pressing 'a'
  ;; this is a hack to correctly diminish `dired-omit-mode'
  (add-function :after (symbol-function 'dired-omit-startup)
                (lambda () (diminish 'dired-omit-mode))
                '((name . dired-omit-mode-diminish))))

;; Ignore uninteresting files
(use-package ignoramus
  :ensure t
  :init
  (add-hook 'nx-post-init-hook #'ignoramus-setup))

(provide 'config-files)
;;; config-files.el ends here

(if (fboundp 'with-eval-after-load)
    (defalias 'after-load 'with-eval-after-load)
  (defmacro after-load (feature &rest body)
    "After FEATURE is loaded, evaluate BODY."
    (declare (indent defun))
    `(eval-after-load ,feature
       '(progn ,@body))))

;; Handier way to add modes to auto-mode-alist
(defun add-auto-mode (mode &rest patterns)
  "Add entries to `auto-mode-alist' to use `MODE' for all given file `PATTERNS'."
  (dolist (pattern patterns)
    (add-to-list 'auto-mode-alist (cons pattern mode))))

;; String utilities missing from core Emacs
(defun sanityinc/string-all-matches (regex str &optional group)
  "Find all matches for `REGEX' within `STR', returning the full match string or group `GROUP'."
  (let ((result nil)
	(pos 0)
	(group (or group 0)))
    (while (string-match regex str pos)
      (push (match-string group str) result)
      (setq pos (match-end group)))
    result))

;; Delete current file
(defun delete-this-file ()
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

;; Rename current file
(defun rename-this-file-and-buffer ()
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

(defun copy-filename-as-kill (&optional arg)
  "Copy the name of the currently visited file to kill ring.
With a zero prefix arg, copy the absolute file name.  With
\\[universal-argument], copy the file name relative to the
current Projectile project, or to the current buffer's
`default-directory', if the file is not part of any project.
Otherwise copy the non-directory part only."
  (interactive "P")
  (if-let ((file-name (current-file))
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

(defun launch-dwim ()
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

(defun find-user-init-file-other-window ()
  "Edit the `user-init-file', in another window."
  (interactive)
  (find-file-other-window user-init-file))

(defun recompile-packages ()
  "Recompile all packages."
  (interactive)
  (byte-recompile-directory package-user-dir nil 'force))

;; Browse current HTML file
(defun browse-current-file ()
  "Open the current file as a URL using `browse-url'."
  (interactive)
  (let ((file-name (buffer-file-name)))
    (if (and (fboundp 'tramp-tramp-file-p)
             (tramp-tramp-file-p file-name))
        (error "Cannot open tramp file")
      (browse-url (concat "file://" file-name)))))

;; Clear terminal background
(defun clear-background-term ()
  (unless (display-graphic-p (selected-frame))
    (set-face-background 'default "unspecified-bg" (selected-frame))))

(provide 'init-utils)

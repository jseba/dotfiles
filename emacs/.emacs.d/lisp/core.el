;;; core.el -- Core settings

;;; Commentary:
;;; Code:

;; Better hooks
(defvar nx-init-hook nil
  "A list of hooks to run when Emacs is initialized.")
(defvar nx-post-init-hook nil
  "A list of hooks to run after Emacs is initialized (after `nx-init-hook').")

(defvar nx-init-p nil
  "If non-nil, Emacs has been fully initialized.")

(defconst nx--no-ssl-p (and (memq system-type '(windows-nt ms-dos))
                            (not (gnutls-available-p)))
  "Whether or not SSL/TLS is available.")

(defun nx--try-run-hook (fn hook)
  "Run FN from HOOK wrapped in a `condition-case-unless-debug' block.

This provides more information in the error message and still provides
for invoking the debugger in debug mode."
  (condition-case-unless-debug ex
      (funcall fn)
    ('error
     (lwarn hook :error
          "%s in '%s' -> %s"
          (car ex) fn (error-message-string ex))))
  nil)

(defun nx|finalize ()
  "Finalize initialization."
  (unless nx-init-p
    (dolist (hook '(nx-init-hook nx-post-init-hook))
      (run-hook-wrapped hook #'nx--try-run-hook hook))
    (setq nx-init-p t))
  (setq gc-cons-threshold (* 16 1024 1024)
      gc-cons-percentage 0.1
        file-name-handler-alist nx--file-name-handler-alist)
  (message "init completed in %.2fms"
         (* 1000.0 (float-time (time-subtract after-init-time before-init-time)))))
(add-hook 'emacs-startup-hook #'nx|finalize)

(provide 'core)
;;; core.el ends here

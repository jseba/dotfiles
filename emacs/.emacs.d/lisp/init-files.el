(require 'init-utils)

;; Ignore uninteresting files
(use-package ignoramus
  :ensure t
  :config
  (ignoramus-setup))

;; Protect delicate files
(use-package hardhat
  :ensure t
  :diminish global-hardhat-mode
  :config
  (global-hardhat-mode))

;; Auto-revert buffers of changed files
(use-package autorevert
  :init
  (setq auto-revert-verbose nil
	global-auto-revert-non-file-buffers t)
  (global-auto-revert-mode)
  :diminish
  auto-revert-mode)

;; File tools from lunaryorn
(use-package lunaryorn-files
  :init
  (defun delete-file-and-buffer ()
    "Delete the current file and kill the buffer.")
  :bind (("C-c f D" . delete-file-and-buffer)
	 ("C-c f o" . launch-dwim)
	 ("C-c f R" . rename-file-and-buffer)
	 ("C-c f w" . copy-filename-as-kill)
         ("C-c f u" . find-user-init-file-other-window)
         ("C-c f ." . browse-feature-url)))

(use-package midnight)

(setq view-read-only t)

(provide 'init-files)

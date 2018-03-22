;;; config-grep.el -- Grep

;;; Commentary:
;;; Code:

(defun +grep|run-grep-from-here (&rest _)
  "Run grep recursively from the directory of the current buffer or the default directory."
  (interactive)
  (let* ((dir (file-name-directory (or load-file-name buffer-file-name default-directory)))
         (command (read-from-minibuffer "Run grep (like this): "
                                        (cons (concat "grep -nH -r  " dir) 13))))
    (grep command)))

(use-package grep
  :if (executable-find "grep")
  :init
  (setq grep-highlight-matches t
	    grep-scroll-output t))

(provide 'config-grep)
;;; config-grep.el ends here

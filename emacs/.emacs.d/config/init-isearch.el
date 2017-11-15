(use-package anzu
  :ensure t
  :diminish anzu-mode
  :init
  (defun isearch-yank-symbol ()
    "*Put symbol at current point into search string."
    (interactive)
    (let ((sym (symbol-at-point)))
      (if sym
	  (progn
	    (setq isearch-regexp t
		  isearch-string (concat "\\_<" (regexp-quote (symbol-name sym)) "\\_>")
		  isearch-message (mapconcat 'isearch-text-char-description isearch-string "")
		  isearch-yank-flag t))
	(ding)))
    (isearch-search-and-update))
  (defun isearch-exit-other-end (rbeg rend)
    "Exit isearch, but at the other end of the search string.
This is useful when followed by an immediate kill."
    (interactive "r")
    (isearch-exit)
    (goto-char isearch-other-end))
  (global-anzu-mode)
  :bind
  (([remap query-replace] . anzu-query-replace)
   ([remap query-replace-regexp] . anzu-query-replace-regexp)
   :map isearch-mode-map
   ([remap isearch-query-replace] . anzu-isearch-query-replace)
   ([remap isearch-query-replace-regexp] . anzu-isearch-query-replace-regexp)
   ([remap isearch-delete-char] . isearch-del-char)
   ("C-M-w" . isearch-yank-symbol)
   ("C-<return>" . isearch-exit-other-end))
  :config
  (setq anzu-cons-mode-line-p nil
	anzu-mode-lighter ""))

(provide 'init-isearch)

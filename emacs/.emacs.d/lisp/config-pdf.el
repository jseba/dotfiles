;;; config-pdf.el

(use-package pdf-tools
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :config
  (pdf-tools-install)

  (define-key! pdf-view-mode-map
    "q" #'kill-this-buffer)

  (after! evil
    (evil-define-key* 'normal pdf-view-mode-map
                      "q" #'kill-this-buffer))

  (defun +pdf-cleanup-windows ()
    "Kill leftover annotation buffers when the document buffer is killed."
    (when (buffer-live-p pdf-annot-list-document-buffer)
      (pdf-info-close pdf-annot-list-document-buffer))
    (when (buffer-live-p pdf-annot-list-buffer)
      (kill-buffer pdf-annot-list-buffer))
    (let ((contents-buffer (get-buffer "*Contents*")))
      (when (and contents-buffer (buffer-live-p contents-buffer))
        (kill-buffer contents-buffer))))
  (add-hook! 'pdf-view-mode-hook
    (add-hook 'kill-buffer-hook #'+pdf-cleanup-windows nil t))

  (setq-default pdf-view-display-size 'fit-page)
  (add-hook! 'pdf-view-mode-hook (cua-mode 0))

  (+popup-set-rule "^\\*Outline*" :side 'right :size 40 :select nil)

  (+modeline-define-segment
   +pdf-pages
   "Current and total page indicator for PDF documents."
   (format "P %d/%d" (pdf-view-current-page) (pdf-cache-number-of-pages)))

  (+modeline-define-format
   '+pdf
   '(+modeline-bar
     " "
     +modeline-buffer-id
     "  "
     +pdf-pages)
   '(+modeline-major-mode
     +modeline-vcs))

  (defun +pdf-set-modeline ()
    (+modeline-set-format '+pdf))
  (add-hook 'pdf-tools-enabled-hook #'+pdf-set-modeline))

(provide 'config-pdf)

(use-package markdown-mode
  :ensure t
  ;; Just no, dear Markdown Mode.  Don't force that bastard Github dialect me!
  :mode ("\\.md\\'" . markdown-mode)
  :init
  (defun lunaryorn-whitespace-style-no-long-lines ()
    "Disable the highlighting of overlong lines."
  (setq-local whitespace-style (-difference whitespace-style
                                            '(lines lines-tail))))

  ;; Process Markdown with Pandoc, using a custom stylesheet for nice output
  (let ((stylesheet (expand-file-name
    (locate-user-emacs-file "etc/pandoc.css"))))
    (setq markdown-command
    (mapconcat #'shell-quote-argument
               `("pandoc" "--toc" "--section-divs"
                 "--css" ,(concat "file://" stylesheet)
                 "--standalone" "-f" "markdown" "-t" "html5")
                 " ")))

  ;; No filling in GFM, because line breaks are significant.
  (add-hook 'gfm-mode-hook #'turn-off-auto-fill)

  ;; Use visual lines instead
  (add-hook 'gfm-mode-hook #'visual-line-mode)
  (add-hook 'gfm-mode-hook #'lunaryorn-whitespace-style-no-long-lines)

  :bind
  (:map markdown-mode-map
	("C-c C-s C" . markdown-insert-gfm-code-block)
	("C-c C-s P" . markdown-insert-gfm-code-block)
   :map gfm-mode-map
   ("M-q" . ignore)))

; YAML
(use-package yaml-mode
  :ensure t
  :init
  (add-hook 'yaml-mode-hook (lambda () (run-hooks 'prog-mode-hook))))

(provide 'init-markdown)

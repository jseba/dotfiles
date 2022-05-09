;;; tools/tramp/+lsp.el -*- lexical-binding: t; -*-

(when (featurep! :lang go)
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-tramp-connection
                                     (lambda () (cons lsp-go-gopls-server-path lsp-go-gopls-server-args)))
                    :major-modes '(go-mode go-dot-mod-mode)
                    :language-id "go"
                    :priority 0
                    :server-id 'gopls
                    :completion-in-comments? t
                    :remote? t
                    :library-folders-fn #'lsp-go--library-default-directories
                    :after-open-fn (lambda () (setq-local lsp-completion-filter-on-incomplete nil)))))

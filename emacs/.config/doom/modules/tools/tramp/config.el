;;; tools/tramp/config.el -*- lexical-binding: t; -*-

(after! tramp
  (setq
   ;; force PATH on remote to something akin to personal standard
   tramp-remote-path '("/usr/local/bin"
                       "/usr/local/sbin"
                       "/usr/bin"
                       "/usr/sbin"
                       "/bin"
                       "/sbin"
                       "/usr/local/go/bin"
                       "~/go/bin"
                       "~/.cargo/bin")))

;; support LSP over TRAMP
(when (featurep! lsp)
  (load! "+lsp"))

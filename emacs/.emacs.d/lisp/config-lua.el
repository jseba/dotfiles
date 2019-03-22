;;; config-lua.el -*- lexical-binding: t -*-

;; smartparen's default rules are obnoxious, so disable them
(provide 'smartparens-lua)

(use-package lua-mode
  :init
  (setq lua-indent-level tab-width)
  :config
  (+xref-set-lookup-handlers 'lua-mode
    :documentation 'lua-search-documentation)
  (+electric-set 'lua-mode '("else" "end"))
  (+company-set-backends 'lua-mode
    '(company-lua)))

(use-package moonscript)

(use-package company-lua)

(provide 'config-lua)

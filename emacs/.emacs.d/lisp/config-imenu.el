;;; config-imenu.el

(use-package imenu-anywhere
  :init
  (setq imenu-anywhere-delimter ": "))

(use-package imenu-list
  :config
  (setq imenu-list-idle-update-delay 0.5)
  (+popup-set-rule
   "^\\*Ilist"
   :side 'right
   :size 35
   :quit nil
   :select nil
   :ttl 0))

(provide 'config-imenu)
;;; config-imenu.el ends here

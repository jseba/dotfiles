;;; early-init.el

(setq ;; gc-cons-threshold (* 256 1024 1024)
 package-enable-at-startup nil
 inhibit-startup-screen t
 inhibit-startup-message t
 inhibit-default-init t
 inhibit-startup-echo-area-message user-login-name
 initial-major-mode 'fundamental-mode
 initial-scratch-message nil
 mode-line-format nil
 visible-bell nil
 byte-compile-warnings '(not free-vars
                             unresolved
                             noruntime
                             lexical
                             make-local))

(advice-add #'display-startup-echo-area-message :override #'ignore)

;;
;; 'Y' or 'N' should suffice
(fset #'yes-or-no-p #'y-or-n-p)

(add-to-list 'default-frame-alist '(tool-bar-lines 0))
(add-to-list 'default-frame-alist '(menu-bar-lines  0))
(add-to-list 'default-frame-alist '(vertical-scroll-bars))
(add-to-list 'default-frame-alist '(horizontal-scroll-bars))

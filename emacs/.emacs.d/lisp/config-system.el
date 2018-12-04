;;; config-system.el

(setq x-select-request-type '(UTF8_STRING COMPOUNT_TEXT TEXT STRING)
      select-enable-clipboard t
      select-enable-primary t)

(after!
  (advice-add #'evil-visual-update-x-selection
              :override #'ignore))

(defmacro set-environment-variable (&rest _vars)
  "Inject VARS from your shell environment into Emacs.")

(cond
 (%IS-MACOS
  (setq mac-command-modifier 'meta
        mac-option-modifier 'alt
        mouse-wheel-scroll-amount '(5 ((shift) . 2))
        mouse-wheel-progressive-speed nil
        ns-use-native-fullscreen nil
        ns-pop-up-frames nil)

  (use-package exec-path-from-shell
    :when (or (daemonp) (display-graphic-p))
    :init
    (exec-path-from-shell-initialize)
    :config
    (defun set-environment-variable (&rest vars)
      (exec-path-from-shell-copy-envs vars))

    (setq exec-path-from-shell-check-startup-files nil
          exec-path-from-shell-arguments
          (delete "-i" exec-path-from-shell-arguments)
          exec-path-from-shell-debug %debug-mode
          exec-path-from-shell-variables
          (nconc exec-path-from-shell-variables '("LC_TYPE" "LC_ALL" "LANG"))))

  (use-package osx-clipboard
    :when (or (daemonp) (not (display-graphic-p)))
    :init
    (add-hook 'init-hook #'osx-clipboard-mode))
  
  (defun +macos-open (&optional app-name path)
    "Send PATH to APP-NAME on macOS."
    (interactive)
    (let* ((path (expand-file-name
                  (replace-regexp-in-string
                   "'" "\\'"
                   (or path
                       (if (eq major-mode 'dired-mode)
                           (dired-get-file-for-visit)
                         (buffer-file-name)))
                   nil t)))
           (command (format "open %s"
                            (if app-name
                                (format "-a %s '%s'"
                                        (shell-quote-argument app-name)
                                        path)
                              (format "'%s'" path)))))
      (message "Running: %s" command)
      (shell-command command)))

  (defmacro +macos--open (id &optional app dir)
    `(defun ,(intern (format "+macos-%s" id)) ()
       (interactive)
       (+macos-open-with ,app ,dir)))

  (+macos--open open-with-default-program)
  (+macos--open reveal-in-finder "Finder" default-directory)
  (+macos--open reveal-project-in-finder "Finder"
                (or (+projectile-project-root)
                    default-directory)))
 (%IS-WIN32
  (setq w32-get-true-file-attributes nil
        vc-handled-backends (delq 'Git vc-handled-backends))))

(provide 'config-system)

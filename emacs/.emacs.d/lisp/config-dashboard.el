;;; config-dashboard.el -- -*- lexical-binding: t; -*-

;;; Shamelessly lifted from Doom Emacs

(defvar +dashboard-name "*dashboard*"
  "The name for the dashboard buffer.")

(defvar +dashboard-functions
  '(+dashboard-widget-banner
    +dashboard-widget-shortmenu
    +dashboard-widget-loaded
    +dashboard-widget-footer)
  "The list of widgets to show in the dashboard buffer to construct the
dashboard. These functions take no arguments and the dashboard is current
while they run.")

(defvar +dashboard-banner-padding '(4 . 4)
  "Number of newlines to bad the banner with, before and after.")

(defvar +dashboard-inhibit-refresh nil
  "If non-nil, the dashboard buffer won't be refreshed.")

(defvar +dashboard-inhibit-refresh-functions ()
  "A list of functions to inhibit refreshing of the dashboard buffer.
These functions take no arguments and if any return non-nil, the dashboard
won't be refreshed.")

(defvar +dashboard-pwd-policy 'last-project
  "The policy to use when setting the `default-directory' in the dashboard.

Possible values:
  'last-project  - the project root of the last open buffer
  'last          - the default directory of the last open buffer
  a FUNCTION     - a function run with the `default-directory' of the last
                   open buffer that returns a directory path
  a STRING       - a fixed path
  nil            - don't change `default-directory'")

(defvar +dashboard-menu-sections
  '(("Recently opened files"
     :icon (all-the-icons-octicon "file-text" :face 'font-lock-keyword-face)
     :action recentf-open-files)
    ("Open project"
     :icon (all-the-icons-octicon "briefcase" :face 'font-lock-keyword-face)
     :action projectile-switch-project)
    ;; TODO: Finish getting Org-mode working
    ;; ("Open agenda"
    ;;  :icon (all-the-icons-octicon "calendar" :face 'font-lock-keyword-face)
    ;;  :when (fboundp 'org-agenda)
    ;;  :action org-agenda)
    ("Jump to bookmark"
     :icon (all-the-icons-octicon "bookmark" :face 'font-lock-keyword-face)
     :action bookmark-jump)
    ;; FIXME: need to properly define the action for this
    ("Reload last session"
     :icon (all-the-icons-octicon "history" :face 'font-lock-keyword-face)
     :when (if (require 'desktop nil t) (file-exists-p (desktop-full-file-name)))
     :face (:inherit (font-lock-keyword-face bold))
     :action desktop-revert))
  "An alist of menu buttons used by `+dashboard-widget-shortmenu'. Each element
is a cons cell (LABEL . PLIST). LABEL is a string to display after the icon and
before the keybind string.

PLIST can have the following properties:
  :icon FORM    - the icon to display; FORM can be a literal string
  :key STRING   - the keybind displayed next to the button
  :when FORM    - if FORM returns nil, don't display this button
  :face FACE    - the face for the icon and text
  :action FORM  - run FORM when the button is pushed")

(defvar +dashboard--last-cwd nil)
(defvar +dashboard--width 80)
(defvar +dashboard--old-fringe-indicator fringe-indicator-alist)
(defvar +dashboard--pwd-alist ())

;; Appease the byte-compiler. TODO: needed?
(defvar all-the-icons-scale-factor)
(defvar all-the-icons-default-adjust)

;;
;; Bootstrap

(setq fallback-buffer +dashboard-name
      ;; Don't open two windows if a file is passed in on the command line
      initial-buffer-choice
      (when (or (daemonp)
                (not (cl-loop for arg in (cdr command-line-args)
                              if (or (equal arg "--restore")
                                     (and (string-match-p "^[^-]" arg)
                                          (file-exists-p arg)))
                              return t)))
        #'+dashboard-initial-buffer))
(add-hook 'window-setup-hook #'+dashboard-init)

;;
;; Dashboard major mode

(define-derived-mode +dashboard-mode special-mode
  (format "Emacs v%s" emacs-version)
  :syntax-table nil
  :abbrev-table nil
  "Major mode for the dashboard buffer."
  (setq truncate-lines t)
  (setq-local whitespace-style nil)
  (setq-local show-trailing-whitespace nil)
  (setq-local hscroll-margin 0)
  (setq-local tab-width 2)
  (cl-loop for (car . _cdr) in fringe-indicator-alist
           collect (cons car nil) into alist
           finally do (setq fringe-indicator-alist alist))
  (add-hook 'post-command-hook #'+dashboard-reposition-point nil t))

(define-key! +dashboard-mode-map
  [remap forward-button]    #'+dashboard-forward-button
  [remap backward-button]   #'+dashboard-backward-button
  "n"                       #'forward-button
  "p"                       #'backward-button
  "C-n"                     #'forward-button
  "C-p"                     #'backward-button
  [down]                    #'forward-button
  [up]                      #'backward-button
  [tab]                     #'forward-button
  [backtab]                 #'backward-button)

(map! :keymap +dashboard-mode-map
      :n "j"                #'forward-button
      :n "k"                #'backward-button
      :n "n"                #'forward-button
      :n "p"                #'backward-button
      :n "C-n"              #'forward-button
      :n "C-p"              #'backward-button
      :n [down]             #'forward-button
      :n [up]               #'backward-button
      :n [tab]              #'forward-button
      :n [backtab]          #'backward-button

      [remap evil-next-visual-line]     #'forward-button
      [remap evil-previous-visual-line] #'backward-button

      [left-margin mouse-1]         #'ignore
      [remap evil-delete]           #'ignore
      [remap evil-delete-line]      #'ignore
      [remap evil-insert]           #'ignore
      [remap evil-append]           #'ignore
      [remap evil-replace]          #'ignore
      [remap evil-replace-state]    #'ignore
      [remap evil-change]           #'ignore
      [remap evil-change-line]      #'ignore
      [remap evil-visual-char]      #'ignore
      [remap evil-visual-line]      #'ignore)

;;
;; Hooks

(defun +dashboard-reposition-point ()
  "Trap the point in the buttons."
  (when (region-active-p)
    (deactivate-mark t)
    (when (bound-and-true-p evil-mode)
      (evil-change-to-previous-state)))
  (or (ignore-errors
        (if (button-at (point))
            (forward-button 0)
          (backward-button 1)))
      (progn (goto-char (point-min))
             (forward-button 1))))

(defun +dashboard-init ()
  "Initializes the dashboard."
  (unless noninteractive
    (add-hook 'window-configuration-change-hook #'+dashboard-resize)
    (add-hook 'window-size-change-functions #'+dashboard-resize)
    (add-hook 'kill-buffer-query-functions #'+dashboard-reload-on-kill)
    (add-hook 'switch-buffer-hooks #'+dashboard-reload-on-kill))
  (if (daemonp)
      (add-hook 'after-make-frame-functions #'+dashboard-reload-frame)
    (+dashboard-reload t)))

(defun +dashboard-reload-on-kill ()
  "A `kill-buffer-query-functions' hook. If this isn't a dashboard buffer,
move along, but record its `default-directory' if the buffer is real.

If this is a dashboard buffer, reload the dashboard."
  (or (let ((buf (current-buffer)))
        (unless (+dashboardp buf)
          (when (real-buffer-p buf)
            (setq +dashboard--last-cwd default-directory)
            (+dashboard-update-pwd))
          t))
      (ignore
       (let (+dashboard-inhibit-refresh)
         (ignore-errors (+dashboard-reload))))))

(defun +dashboard-reload-frame (_frame)
  "Reload the dashboard after a brief pause. This is necessary for new frames,
whose dimensions may not yet be fully initialized when this is run."
  (run-with-timer 0.1 nil #'+dashboard-reload t))

(defun +dashboard-resize (&rest _)
  "Recenter the dashboard and reset its margins and fringes."
  (let ((windows (get-buffer-window-list (fallback-buffer) nil t))
        buffer-list-update-hook)
    (dolist (win windows)
      (set-window-start win 0)
      (set-window-fringes win 0 0)
      (set-window-margins
       win
       (max 0 (/ (- (window-total-width win) +dashboard--width) 2))))
    (when windows
      (with-current-buffer (fallback-buffer)
        (save-excursion
          (with-silent-modifications
            (goto-char (point-min))
            (delete-region (line-beginning-position)
                           (save-excursion (skip-chars-forward "\n")
                                           (point)))
            (insert (make-string
                     (max 0 (- (/ (window-height (get-buffer-window)) 2)
                               (round (/ (+ (count-lines (point-min) (point-max))
                                            (car +dashboard-banner-padding))
                                         2))))
                     ?\n))))))))

;;
;; Library

(defun +dashboard--help-echo ()
  (when-let* ((btn (button-at (point)))
              (msg (button-get btn 'help-echo)))
    (message "%s" msg)))

(defun +dashboard-open (frame)
  "Switch to the dashboard in the current window of FRAME."
  (interactive (list (selected-frame)))
  (with-selected-frame frame
    (switch-to-buffer (+dashboard-initial-buffer))
    (+dashboard-reload)))

(defun +dashboard-forward-button (n)
  "Like `forward-button', but don't wrap."
  (interactive "p")
  (forward-button n nil)
  (+dashboard--help-echo))

(defun +dashboard-backward-button (n)
  "Like `backward-button', but don't wrap."
  (interactive "p")
  (backward-button n nil)
  (+dashboard--help-echo))

(defun +dashboard-initial-buffer ()
  "Returns the buffer to display on startup. Designed for `initial-buffer-choice'."
  (let (buffer-list-update-hook)
    (get-buffer-create +dashboard-name)))

(defun +dashboardp (buffer)
  "Returns t if BUFFER is the dashboard buffer."
  (eq buffer (get-buffer +dashboard-name)))

(defun +dashboard-update-pwd (&optional pwd)
  "Update `default-directory' in the dashboard buffer. What it is set to is
determined by `+dashboard-pwd-policy'."
  (if pwd
      (with-current-buffer (fallback-buffer)
        (setq-local default-directory pwd))
    (let ((new-pwd (+dashboard--get-pwd)))
      (when (and new-pwd (file-directory-p new-pwd))
        (unless (string-suffix-p "/" new-pwd)
          (setq new-pwd (concat new-pwd "/")))
        (+dashboard-update-pwd new-pwd)))))

(defun +dashboard-reload (&optional force)
  "Update the dashboard buffer (or create it if it doesn't exist)."
  (when (or (and (not +dashboard-inhibit-refresh)
                 (get-buffer-window (fallback-buffer))
                 (not (window-minibuffer-p (frame-selected-window)))
                 (not (run-hook-with-args-until-success '+dashboard-inhibit-refresh-functions)))
            force)
    (with-current-buffer (fallback-buffer)
      (with-silent-modifications
        (save-excursion
          (unless (eq major-mode '+dashboard-mode)
            (+dashboard-mode))
          (erase-buffer)
          (run-hooks '+dashboard-functions)))
      (+dashboard-resize)
      (+dashboard-update-pwd)
      (current-buffer))))

;;
;; Helpers

(defun +dashboard--center (len s)
  (concat (make-string (ceiling (max 0 (- len (length s))) 2) ? )
          s))

(defun +dashboard--get-pwd ()
  (let ((lastcwd +dashboard--last-cwd)
        (policy +dashboard-pwd-policy))
    (cond ((null policy)
           default-directory)
          ((stringp policy)
           (expand-file-name policy lastcwd))
          ((functionp policy)
           (funcall policy lastcwd))
          ((null lastcwd)
           default-directory)
          ((eq policy 'last-project)
           (let ((cwd default-directory)
                 (default-directory lastcwd))
             (if (+projectile-project-p)
                 (+projectile-project-root)
               cwd)))
          ((eq policy 'last)
           lastcwd)
          (t
           (warn "`+dashboard-policy' is set to an invalid value: %s"
                 policy)))))

;;
;; Widgets

(defun +dashboard-widget-banner ()
  (let ((point (point)))
    (mapc (lambda (line)
            (insert (propertize (+dashboard--center +dashboard--width line)
                                'face 'font-lock-comment-face) " ")
            (insert "\n"))
          '("      ___           ___           ___           ___           ___      "
            "     /  /\\         /__/\\         /  /\\         /  /\\         /  /\\     "
            "    /  /:/_       |  |::\\       /  /::\\       /  /:/        /  /:/_    "
            "   /  /:/ /\\      |  |:|:\\     /  /:/\\:\\     /  /:/        /  /:/ /\\   "
            "  /  /:/ /:/_   __|__|:|\\:\\   /  /:/~/::\\   /  /:/  ___   /  /:/ /::\\  "
            " /__/:/ /:/ /\\ /__/::::| \\:\\ /__/:/ /:/\\:\\ /__/:/  /  /\\ /__/:/ /:/\\:\\ "
            " \\  \\:\\/:/ /:/ \\  \\:\\~~\\__\\/ \\  \\:\\/:/__\\/ \\  \\:\\ /  /:/ \\  \\:\\/:/~/:/ "
            "  \\  \\::/ /:/   \\  \\:\\        \\  \\::/       \\  \\:\\  /:/   \\  \\::/ /:/  "
            "   \\  \\:\\/:/     \\  \\:\\        \\  \\:\\        \\  \\:\\/:/     \\__\\/ /:/   "
            "    \\  \\::/       \\  \\:\\        \\  \\:\\        \\  \\::/        /__/:/    "
            "     \\__\\/         \\__\\/         \\__\\/         \\__\\/         \\__\\/     "))))

(defun +dashboard-widget-loaded ()
  (insert
   "\n\n"
   (propertize
    (+dashboard--center
     +dashboard--width
     (display-benchmark 'return))
    'face 'font-lock-comment-face)
   "\n"))

(defun +dashboard-widget-shortmenu ()
  (let ((all-the-icons-scale-factor 1.45)
        (all-the-icons-default-adjust -0.02))
    (insert "\n\n\n")
    (dolist (section +dashboard-menu-sections)
      (cl-destructuring-bind (label &key icon action when face) section
        (when (and (fboundp action)
                   (or (null when)
                       (eval when t)))
          (insert
           (+dashboard--center
            (- +dashboard--width 1)
            (let ((icon (if (stringp icon) icon (eval icon t))))
              (format (format "%s%%s%%-10s" (if icon "%3s\t" "%3s"))
                      (or icon "")
                      (with-temp-buffer
                        (insert-text-button
                         label
                         'action
                         `(lambda (_)
                            (call-interactively (or (command-remapping #',action)
                                                    #',action)))
                         'face (or face 'font-lock-keyword-face)
                         'follow-link t
                         'help-echo
                         (format "%s (%s)" label
                                 (propertize (symbol-name action)
                                             'face 'font-lock-constant-face)))
                        (format "%-37s" (buffer-string)))
                      ;; Lookup command keys dynamically
                      (or (when-let* ((key (where-is-internal action nil t)))
                            (with-temp-buffer
                              (save-excursion (insert (key-description key)))
                              (while (re-search-forward "<\\([^>]+\\)>" nil t)
                                (replace-match (upcase (substring (match-string 1) 0 3))))
                              (propertize (buffer-string) 'face 'font-lock-constant-face)))
                          ""))))
           (if (display-graphic-p)
               "\n\n"
             "\n")))))))

(defun +dashboard-widget-footer ()
  (insert
   "\n"
   (+dashboard--center
    (- +dashboard--width 2)
    (with-temp-buffer
      (insert-text-button (or (all-the-icons-octicon "octoface" :face 'all-the-icons-green :height 1.3 :v-adjust -0.15)
                              (propertize "github" 'face 'font-lock-keyword-face))
                          'action (lambda (_) (browse-url "https://github.com/jseba/dotfiles"))
                          'follow-link t
                          'help-echo "Open Dotfiles on Github")
      (buffer-string)))
   "\n"))

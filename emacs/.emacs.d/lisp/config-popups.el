;;; config-popups.el

(defvar +popup-window-parameters '(ttl quit select modeline popup)
  "A list of custom parameters to be added to `window-persistent-parameters'.")

(defvar +popup-default-display-buffer-actions
  '(display-buffer-reuse-window +popup-display-buffer-stacked-side-window)
  "The functions to use to display the popup buffer.")

(defvar +popup-default-display-buffer-alist
  '((window-height . 0.16)
    (reusable-frames . visible))
  "The default alist for `display-buffer-alist' rules.")

(defvar +popup-default-window-parameters
  '((transient . t)
    (quit . t)
    (select . ignore)
    (no-other-window . t))
  "The default window parameters for popups.")

(defvar +popup-margin-width 1
  "The size of the margins to give popup windows.

  Set this to nil to disable margin adjustments.")

(defvar +popup-defaults
  (list :side   'bottom
        :height 0.16
        :width  40
        :quit   t
        :select #'ignore
        :ttl    5)
  "Default setup for `+popup-set-popup-rule'.")

(defvar +popup--inhibit-transient nil)
(defvar +popup--inhibit-select nil)
(defvar +popup--old-display-buffer-alist nil)
(defvar +popup--last nil)
(defvar +popup--remember-last nil)
(defvar +popup--display-buffer-alist nil)
(defvar-local +popup--timer nil)

(defun +popup--remember (windows)
  "Remember a list of WINDOWS for later restoration."
  (cl-assert (cl-every #'windowp windows) t)
  (setq +popup--last
        (cl-loop for w in windows
                 collect (cons (window-buffer w)
                               (window-state-get w)))))

(defun +popup--kill-buffer (buffer ttl)
  "Tries to kill BUFFER was requested by a transient timer.

  If it fails, eg. the buffer is visible, then set another timer and try again
  later."
  (when (buffer-live-p buffer)
    (let ((inhibit-quit t)
          (kill-buffer-hook (remq '+popup-kill-buffer kill-buffer-hook)))
      (cond ((get-buffer-window buffer)
             (with-current-buffer buffer
               (setq +popup--timer
                     (run-at-time ttl nil #'+popup--kill-buffer buffer ttl))))
            ((eq ttl 0)
             (kill-buffer buffer))
            ((with-demoted-errors "Error killing transient buffer %s"
               (with-current-buffer buffer
                 (let (confirm-kill-processes)
                   (when-let* ((process (get-buffer-process buffer)))
                     (kill-process process))
                   (let (kill-buffer-hook kill-buffer-query-functions)
                     (kill-buffer buffer))))))))))

(defun +popup--init (window &optional alist)
  "Initializes a popup in WINDOW.

  This runs any time a popup is opened. It sets the default window parameters
  for popup windows, clears leftover transient timers and enables
  `+popup-buffer-mode'."
  (with-selected-window window
    (set-window-parameter window 'popup t)
    (set-window-parameter window 'delete-window #'+popup--delete-window)
    (set-window-parameter window 'delete-other-windows #'+popup-close-all)
    (set-window-dedicated-p window 'popup)
    (window-preserve-size window
                          (memq (window-parameter window 'window-side)
                                '(left right))
                          t)
    (+popup-buffer-mode +1)
    (run-hooks '+popup-create-window-hook)))

(defun +popup--delete-window (window)
  "Deletes a popup WINDOW.

  Disables `+popup-buffer-mode' to allow any attached hooks to run, either kills
  the buffer or sets a transient timer and then deletes the window."
  (let ((buffer (window-buffer window))
        (inhibit-quit t)
        ttl)
    (when (and (buffer-file-name buffer)
               (buffer-modified-p buffer)
               (or (+popup-parameter-fn 'autosave window buffer)
                   (y-or-n-p "Popup buffer is modified; save it?")))
      (with-current-buffer buffer (save-buffer)))
    (set-buffer-modified-p nil)
    (let ((ignore-window-parameters t))
      (delete-window window))
    (unless (window-live-p window)
      (with-current-buffer buffer
        (+popup-buffer-mode -1)
        (unless +popup--inhibit-transient
          (setq ttl (+popup-parameter-fn 'ttl window buffer))
          (when ttl
            (when (eq ttl t)
              (setq ttl (or (plist-get +popup-defaults :ttl)
                            0)))
            (cl-assert (integerp ttl) t)
            (if (= ttl 0)
                (+popup--kill-buffer buffer 0)
              (add-hook! :local 'kill-buffer-hook #'+popup-kill-buffer)
              (setq +popup--timer
                    (run-at-time ttl nil #'+popup--kill-buffer buffer ttl)))))))))

(defun +popup--switch-from-popup (location)
  (let (origin enable-local-variables)
    (+popup-ignore-popups!
     (switch-to-buffer (car location) nil t)
     (if (not (cdr location))
         (message "Unable to find location in file")
       (goto-char (cdr location))
       (recenter)
       (setq origin (selected-window))))
    (select-window origin)))

(defun +popup--normalize-alist (alist)
  "Merge `+popup-default-display-buffer-alist' and
  `+popup-default-window-parameters' with ALIST."
  (let ((alist (cl-remove-duplicates (append alist
                                             +popup-default-display-buffer-alist)
                                     :key #'car
                                     :from-end t))
        (parameters (cl-remove-duplicates (append (cdr (assq 'window-parameters alist))
                                                  +popup-default-window-parameters)
                                          :key #'car
                                          :from-end t)))
    (when-let* ((size  (cdr (assq 'size alist)))
                (side  (or (cdr (assq 'side alist)) 'bottom))
                (param (if (memq side '(left right))
                           'window-width
                         'window-height)))
      (setq alist (assq-delete-all 'size alist))
      (setcdr (assq param alist) size)
      (setcdr (assq 'window-parameters alist)
              parameters)
      alist)))

(defun +popup-buffer-p (&optional buffer)
  "Return non-nil if BUFFER (or current buffer) is a popup buffer."
  (when +popup-mode
    (let ((buffer (or buffer (current-buffer))))
      (cl-assert (bufferp buffer) t)
      (and buffer
           (buffer-live-p buffer)
           (buffer-local-value '+popup-buffer-mode buffer)))))

(defun +popup-window-p (&optional window)
  "Return non-nil if WINDOW (or current window) is a popup window."
  (when +popup-mode
    (let ((window (or window (selected-window))))
      (cl-assert (windowp window) t)
      (and window
           (window-live-p window)
           (window-parameter window 'popup)))))

(defun +popup-buffer (buffer &optional alist)
  (let* ((origin (selected-window))
         (window-min-height 3)
         (norm-alist (+popup--normalize-alist alist))
         (actions (or (cdr (assq 'actions norm-alist))
                      +popup-default-display-buffer-actions)))
    (when-let* ((popup (cl-loop for func in actions
                                if (funcall func buffer norm-alist)
                                return it)))
      (+popup--init popup norm-alist)
      (unless +popup--inhibit-select
        (let ((select (+popup-parameter 'select popup)))
          (if (functionp select)
              (funcall select popup origin)
            (select-window (if select popup origin)))))
      popup)))

(defun +popup-parameter (parameter &optional window)
  "Fetch the window PARAMETER of WINDOW (or current window)."
  (window-parameter (or window (selected-window)) parameter))

(defun +popup-parameter-fn (parameter &optional window &rest args)
  "Fetch the window PARAMETER of WINDOW (or current window).

  If it is a function, return the result of running it."
  (let ((val (+popup-parameter parameter window)))
    (if (functionp val)
        (apply val args)
      val)))

(defun +popup-windows ()
  "Returns a list of all popup windows."
  (cl-remove-if-not #'+popup-window-p (window-list)))

(defun +popup-shrink-to-fit (&optional window)
  "Shrinks WINDOW to fit the buffer contents if the buffer isn't empty.

  Uses `shrink-window-if-larger-than-buffer'."
  (let ((window (or window (selected-window))))
    (unless (= (- (point-max) (point-min)) 0)
      (shrink-window-if-larger-than-buffer window))))

(defun +popup-adjust-fringes ()
  "Hides the fringe in popup windows."
  (let ((f (if (bound-and-true-p +popup-buffer-mode) 0)))
    (set-window-fringes nil f f fringes-outside-margins)))

(defun +popup-adjust-margins ()
  "Creates padding for the popup window determined by `+popup-margin-width'."
  (when +popup-margin-width
    (let ((m (if (bound-and-true-p +popup-buffer-mode) +popup-margin-width)))
      (set-window-margins nil m m))))

(defun +popup-set-modeline-on-enable ()
  "Don't show a modeline in popup windows without a `modeline' window parameter.

  - If one exists and it's a symbol, use `+modeline' to grab the format.
  - If non-nil, show the modeline as normal.
  - If nil (or omitted), then hide the modeline entirely (the default).
  - If a function, it takes the current buffer as its argument and must return one
    of the above values."
  (when (bound-and-true-p +popup-buffer-mode)
    (let ((modeline (+popup-parameter-fn 'modeline nil (current-buffer))))
      (cond ((eq modeline 't))
            ((or (eq modeline 'nil)
                 (null modeline))
             (hide-mode-line-mode +1))
            ((symbolp modeline)
             (when-let* ((hide-mode-line-format (doom-modeline modeline)))
               (hide-mode-line-mode +1)))))))
(put '+popup-set-modeline-on-enable 'permanent-local-hook t)

(defun +popup-unset-modeline-on-disable ()
  "Restore the modeline when `+popup-buffer-mode' is deactivated."
  (when (and (not (bound-and-true-p +popup-buffer-mode))
             (bound-and-true-p hide-mode-line-mode))
    (hide-mode-line-mode -1)))

(defun +popup-close-on-escape ()
  "If called inside a popup, try to close that popup window (see
  `+popup-close-popup'). If called outside, try to close all popup windows (see
  `+popup-close-all')."
  (if (+popup-window-p)
      (+popup-close-popup)
    (+popup-close-all)))

(defun +popup-kill-buffer ()
  "Cleans up a popup window when its buffer is killed."
  (when-let* ((window (get-buffer-window)))
    (when (+popup-window-p window)
      (let ((+popup--inibit-transient t))
        (+popup--delete-window window)))))

(defalias 'other-popup #'+popup-other)

(defun +popup-other ()
  "Cycle through popup windows, like `other-window'. Ignores regular windows."
  (interactive)
  (let ((popups (+popup-windows))
        (window (selected-window)))
    (unless popups
      (user-error "No popups are open"))
    (select-window (if (+popup-window-p)
                       (or (car-safe (cdr (memq window popups)))
                           (car (delq window popups))
                           (car popups))
                     (car popups)))))

(defun +popup-close-popup (&optional window force-p)
  "Close WINDOW if it's a popup window.

  This will do nothing if the popup's `quit' window parameter is either nil or
  `\'other'. This window parameter is ignored if FORCE-P is non-nil."
  (interactive (list (selected-window)
                     current-prefix-arg))
  (unless window
    (setq window (selected-window)))
  (when (and (+popup-window-p window)
             (or force-p
                 (memq (+popup-parameter-fn 'quit window window)
                       '(t current))))
    (when +popup--remember-last
      (+popup--remember (list window)))
    (delete-window window)
    t))

(defun +popup-close-all (&optional force-p)
  "Close all open popup windows.

  This will ignore popups with a `quit' parameter that is either nil or
  `\'current'. This window parameter is ignored if FORCE-P is non-nil."
  (interactive "P")
  (let (targets +popup--remember-last)
    (dolist (window (+popup-windows))
      (when (or force-p
                (memq (+popup-parameter-fn 'quit window window)
                      '(t other)))
        (push window targets)))
    (when targets
      (+popup--remember targets)
      (mapc #'delete-window targets)
      t)))

(defun +popup-cleanup-rules ()
  "Cleans up any duplicate popup rules."
  (interactive)
  (cl-delete-duplicates +popup--display-buffer-alist
                        :key #'car
                        :test #'equal
                        :from-end t)
  (when +popup-mode
    (setq display-buffer-alist +popup--display-buffer-alist)))

(defun +popup-toggle ()
  "If popups are open, close them. If they are not, restore the last open one or
  open the message buffer in a popup window."
  (interactive)
  (let ((+popup--inhibit-transient t))
    (cond ((+popup-windows) (+popup-close-all))
          ((ignore-errors (+popup-restore)))
          ((display-buffer (get-buffer "*Messages*"))))))

(defun +popup-restore ()
  "Restore the last popups that were closed, if any."
  (interactive)
  (unless +popup--last
    (error "No popups to restore"))
  (cl-loop for (buffer . state) in +popup--last
           if (and (buffer-live-p buffer)
                   (display-buffer buffer))
           do (window-state-put state it))
  (setq +popup--last nil)
  t)

(defun +popup-raise ()
  "Raise the current popup window into a regular window."
  (interactive)
  (unless (+popup-window-p)
    (user-error "Cannot raise a non-popup window"))
  (let ((window (selected-window))
        (buffer (current-buffer))
        +popup--remember-last)
    (set-window-parameter window 'ttl nil)
    (+popup-close-window window 'force)
    (display-buffer-pop-up-window buffer nil)))

(defun +popup-close (&rest _)
  "TODO"
  (+popup-close nil t))

(defun +popup-ignore-popups (orig-fn &rest args)
  "Sets aside all popups before executing the original function, usually to
  prevent the popup(s) from messing up the window layout (or vice versa)."
  (+popup-ignore-popups! (apply orig-fn args)))

(defun +popup-dont-select-me (orig-fn &rest args)
  (let ((+popup--inhibit-select t))
    (apply orig-fn args)))

(defun +popup-helpful--navigate (button)
  (let ((path (substring-no-properties (button-get button 'path)))
        enable-local-variables origin)
    (+popup-ignore-popups!
     (find-file path)
     (-when-let (pos (get-text-property button 'position
                                        (marker-buffer button)))
       (goto-char pos))
     (setq origin (selected-window))
     (recenter))
    (select-window origin)))

(defun +popup-suppress-delete-other-windows (orig-fn &rest args)
  (if +popup-mode
      (cl-letf (((symbol-function 'delete-other-windows)
                 (symbol-function 'ignore)))
        (apply orign-fn args))
    (apply orig-fn args)))

(defun +popup-org-src-pop-to-buffer (orig-fn buffer context)
  "Hand off the src-block window to the popup system by using `display-buffer'
  instead of `switch-to-buffer-*'."
  (if +popup-mode
      (if (eq org-src-window-setup 'switch-invisibly)
          (set-buffer buffer)
        (display-buffer buffer))
    (funcall orig-fn buffer context)))

(defun +popup-org-pop-to-buffer (orig-fn buf &optional norecord)
  "Use `pop-to-buffer' instead of `switch-to-buffer' to open a buffer."
  (if +popup-mode
      (pop-to-buffer
       (cond ((stringp buf) (get-buffer-create buf))
             ((bufferp buf) buf)
             (t (error "Invalid buffer %s" buf))))
    (funcall orig-fn buf norecord)))

(defun +popup-ignore-window-parameters (orig-fn &rest args)
  "Allow *interactive* window moving commands to traverse popups."
  (cl-letf (((symbol-function #'windmove-find-other-window)
             (lambda (dir &optional arg window)
               (window-in-direction
                (pcase dir (`up 'above) (`down 'below) (_ dir))
                window (bound-and-true-p +popup-mode) arg
                windmove-wrap-around t))))
    (apply orig-fn args)))

(defun +popup-switch-to-info-window (&rest _)
  (when-let* ((win (get-buffer-window "*info*")))
    (when (+popup-window-p win)
      (select-window win))))

(defun +popup-display-buffer-stacked-side-window (buffer alist)
  "A `display-buffer' action that serves as an alternative to
  `display-buffer-in-side-window', but allows for stacking popups with the `vslot'
  alist entry.

  Accepts the same arguments as `display-buffer-in-side-window'. You must set
  `window--sides-inhibit-check' to non-nil for this to work properly."
  (let* ((side (or (cdr (assq 'side alist)) 'bottom))
         (slot (or (cdr (assq 'slot alist)) 0))
         (vslot (or (cdr (assq 'vslot alist)) 0))
         (left-or-right (memq side '(left right)))
         (dedicated (or display-buffer-mark-dedicated 'popup)))
    (cond ((not (memq side '(top bottom left right)))
           (error "Invalid side %s specified" side))
          ((not (numberp slot))
           (error "Invalid slot %s specified" slot))
          ((not (numberp vslot))
           (error "Invalid vslot %s specified" vslot)))
    (let* ((major (get-window-with-predicate
                   (lambda (window)
                     (and (eq (window-parameter window 'window-side) side)
                          (eq (window-parameter window 'window-vslot) vslot)))
                   nil t))
           (reversed (window--sides-reverse-on-frame-p (selected-frame)))
           (windows
            (cond ((window-live-p major)
                   (list major))
                  ((window-valid-p major)
                   (let* ((first (window-child major))
                          (next (window-next-sibling first))
                          (windows (list next first)))
                     (setq reversed (> (window-parameter first 'window-slot)
                                       (window-parameter next  'window-slot)))
                     (while (setq next (window-next-sibling next))
                       (setq windows (cons next windows)))
                     (if reversed windows (nreverse windows))))))
           (slots (if major (max 1 (window-child-count major))))
           (max-slots (nth (plist-get '(left 0 top 1 right 2 bottom 3) side)
                           window-sides-slots))
           (window--sides-inhibit-check t)
           window this-window this-slot prev-window next-window
           best-window best-slot abs-slot)
      (cond ((and (numberp max-slots) (<= max-slots 0))
             nil)
            ((not windows)
             (cl-letf (((symbol-function
                         'window--make-major-side-window-next-to)
                        (lambda (_side) (frame-root-window (selected-frame)))))
               (when-let* ((window (window--make-major-side-window buffer
                                                                   side
                                                                   slot
                                                                   alist)))
                 (set-window-parameter window 'window-vslot vslot)
                 (map-put window-persistent-parameters 'window-vslot 'writable)
                 window)))
            (t
             ;; Scan windows on SIDE.
             (catch 'found
               (dolist (window windows)
                 (setq this-slot (window-parameter window 'window-slot))
                 (cond ((not (numberp this-slot))
                        (setq this-window window)
                        (throw 'found t))
                       (t
                        (setq abs-slot
                              (if (or (and (> this-slot 0) (> slot 0))
                                      (and (< this-slot 0) (< slot 0)))
                                  (abs (- slot this-slot))
                                (+ (abs slot) (abs this-slot))))
                        (unless (and best-slot (<= best-slot abs-slot))
                          (setq best-window window)
                          (setq best-slot abs-slot))
                        (if reversed
                            (cond
                             ((<= this-slot slot)
                              (setq next-window window))
                             ((not prev-window)
                              (setq prev-window window)))
                          (cond
                           ((<= this-slot slot)
                            (setq prev-window window))
                           ((not next-window)
                            (setq next-window window))))))))
             (or (and this-window
                      ;; Reuse `this-window'
                      (with-current-buffer buffer
                        (setq window--sides-shown t))
                      (window--display-buffer buffer
                                              this-window
                                              'reuse
                                              alist
                                              dedicated))
                 (and (or (not max-slots) (< slots max-slots))
                      (or (and next-window
                               ;; Make new window before `next-window'
                               (let ((next-side (if left-or-right 'above 'left))
                                     (window-combination-resize 'side))
                                 (setq window
                                       (ignore-errors (split-window next-window
                                                                    nil
                                                                    next-side)))))
                          (and prev-window
                               ;; Make new window after `prev-window'
                               (let ((prev-side (if left-or-right 'below
                                                  'right))
                                     (window-combination-resize 'side))
                                 (setq window
                                       (ignore-errors (split-window prev-window
                                                                    nil
                                                                    prev-side))))))
                      (set-window-parameter window 'window-slot slot)
                      (with-current-buffer buffer
                        (setq window--sides-shown t))
                      (window--display-buffer buffer
                                              window
                                              'window
                                              alist
                                              dedicated))
                 (and best-window
                      ;; Reuse `best-window'
                      (progn
                        ;; Give `best-window' the new slot value
                        (set-window-parameter best-window 'window-slot slot)
                        (with-current-buffer buffer
                          (setq window--sides-shown t))
                        (window--display-buffer buffer
                                                best-window
                                                'reuse
                                                alist
                                                dedicated)))))))))

(defun +popup--make (predicate plist)
  (cond ((plist-get plist :ignore)
         (list predicate nil))
        ((let* ((plist (append plist +popup-defaults))
                (alist `((actions       . ,(plist-get plist :actons))
                         (side          . ,(plist-get plist :side))
                         (size          . ,(plist-get plist :size))
                         (window-width  . ,(plist-get plist :width))
                         (window-height . ,(plist-get plist :height))
                         (slot          . ,(plist-get plist :slot))
                         (vslot         . ,(plist-get plist :vslot))))
                (params `((ttl      . ,(plist-get plist :ttl))
                          (quit     . ,(plist-get plist :quit))
                          (select   . ,(plist-get plist :select))
                          (modeline . ,(plist-get plist :modeline))
                          (autosave . ,(plist-get plist :autosave))
                          ,@(plist-get plist :parameters))))
           `(,predicate (+popup-buffer)
                        ,@alist
                        (window-parameters ,@params))))))

(defun +popup-set-rule (predicate &rest plist)
  "Defines a popup rule.

  Buffers displayed by `pop-to-buffer' and `display-buffer' (or their siblings)
  will be tested against PREDICATE, which is either a) a regexp string (which is
  matched against the buffer's name) or b) a function that takes no arguments and
  returns a boolean.

  Buffers dsplayed with `switch-to-buffer' and its variants will not be affected
  by these rules (as they are unaffected by `display-buffer-alist', which powers
  this system).

  PLIST can be made up of any of the followng properties:

   :actions ACTIONS
    ACTIONS is a list of functions or an alist containing (FUNCTION . ALIST). See
    `display-buffer''s second arguemtn for more information on its format and what
    it accepts. If omitted, `+popup-default-display-buffer-actions' is used.

   :side 'bottom|'top|'left|'right
    Which side of the frame to open the popup on. This is only respected if
    `+popup-display-buffer-stacked-side-window' or `display-buffer-in-side-window'
    is in :actions or `+popup-default-display-buffer-actions'.

   :size/:width/:height FLOAT|INT
    Determines the size of the popup. If opened at the top or bottom, the width is
    irrelevant unless it is opened in an adjacent slot. Same deal with the left
    and right side.

    If given a FLOAT (0 < x < 1), the number represents how much of the window
    will be consumed by the popup (a percentage). If given an INT, the number
    determines the size in lines (height) or units of character width (width).

   :slot/:vslot INT
    This only applies to the popups with a :side. For popups opened at the top or
    bottom, slot designates the horizontal positioning of a popup. If two popups
    are assigned the same slot (and same vslot), the later popup will replace the
    earlier one. If the later popup has a lower slot, it will open to the older
    popup's left. A higher slot opens to the older popup's right.

    On the other hand, vslot operates the same way, but controls how popups are
    stacked.

    When a popup is opened on the left and right, slot determines vertical
    position and vslot horizontal.

   :ttl INT|BOOL|FN
    Stands for time-to-live. CDR can be t, an integer or a function that returns
    one of these. It represents the number of seconds before the buffer belonging
    to a closed popup window is killed.

    If t, CDR will default to `+popup-ttl'.
    If 0, the buffer is immediately killed.
    If nil, the buffer won't be killed.
    If a function, it must return one of the other possible values listed above.
    It takes the popup buffer as its sole argument.

   :quit BOOL|FN
    CDR can be t, 'other, 'current, nil or a function that returns one of these.
    This determines the behavior of <C-g> in or outside of popup windows.

    If t, close the popup if <C-g> is pressed inside or outside of popups.
    If 'other, close this popup if <C-g> is pressed outside of any popup. This is
    great for popups you just want to peek at and discard, but might also want to
    poke around in, without the risk of closing it from the inside.
    If 'current, close this popup if <C-g> is pressed from inside of the popup.
    If nil, pressing <C-g> will not close this popup.
    If a function, it is checked each time C-g is pressed to determine the fate of
    this popup window. This function must return one of the values listed above
    and takes the popup buffer as its sole argument.

   :select BOOL|FN
    CDR can be a boolean or a function. The boolean determines whether to focus
    the popup window after it opens (non-nil) or focus the origin window (nil).

    If a function it takes two arguments: the popup window and the origin window
    (where you were before the popup was opened). The return value is ignored.

   :modeline BOOL|SYMBOL|FN
    CDR can be t (show the default modeline), a symbol representing the name of a
    modeline defined with `def-modeline!', nil (show no modeline) or a function
    that returns one of these. The function takes one argument: the popup buffer.

   :autosave BOOL|FN
    This parameter determines what to do with modified buffers in closing popup
    windows. CDR can be a t, 'ignore, a function or nil.

    If t, no prompts. Just save them automatically (if they're file-visiting
      buffers).
    If 'ignore, no prompts, no saving. Just silently kill it.
    If nil (the default), prompt the user what to do if the buffer is
      file-visiting and modified.
    If a function, the return value must return one of the other values. It takes
      two arguments: the popup window and buffer.

   :parameters ALIST
    An alist of custom window parameters. See \(info window-parameters)

  If any of these are omitted, defaults derived from `+popup-defaults' will be
  used."
  (push (+popup--make predicate plist) +popup--display-buffer-alist)
  (when (bound-and-true-p +popup-mode)
    (setq display-buffer-alist +popup--display-buffer-alist))
  +popup--display-buffer-alist)

(defun +popup-set-rules (&rest rulesets)
  "Like `+popup-set-rule' but defines multiple popup rules.

  Every entry in RULESETS should be a list of lists, where each sublist is a popup
  rule that could be passed to `+popup-set-rule'."
  (dolist (rules rulesets)
    (dolist (rule rules)
      (push (+popup--make (car rule) (cdr rule))
            +popup--display-buffer-alist)))
  (when (bound-and-true-p +popup-mode)
    (setq display-buffer-alist +popup--display-buffer-alist))
  +popup--display-buffer-alist)

(defvar +popup-mode-map (make-sparse-keymap)
  "Active keymap in a session with the popup system enabled.")

(defvar +popup-buffer-mode-map (make-sparse-keymap)
  "Active keymap in a popup window.")

(define-minor-mode +popup-mode
  "Global minor mode representing the popup management system."
  :init-value nil
  :global t
  :keymap +popup-mode-map
  (cond (+popup-mode
         (add-hook 'global-escape-hook #'+popup-close-on-escape t)
         (add-hook 'cleanup-hook #'+popup-cleanup-rules)
         (setq +popup--old-display-buffer-alist display-buffer-alist
               display-buffer-alist +popup--display-buffer-alist
               window--sides-inhibit-check t))
        (t
         (remove-hook 'global-escape-hook #'+popup-close-on-escape)
         (remove-hook 'cleanup-hook #'+popup-cleanup-rules)
         (setq display-buffer-alist +popup--old-display-buffer-alist
               window--sides-inhibit-check nil)
         (+popup-cleanup-rules)
         (dolist (prop +popup-window-parameters)
           (setq window-persistent-parameters
                 (map-delete window-persistent-parameters prop))))))

(define-minor-mode +popup-buffer-mode
  "Minor mode for individual popup windows.

  It is enabled when a buffer is displayed in a popup window and disabled when
  that window has been changed or closed."
  :init-value nil
  :keymap +popup-buffer-mode-map
  (if (not +popup-buffer-mode)
      (remove-hook! :local 'after-change-major-mode-hook
                    #'+popup-set-modeline-on-enable)
    (add-hook! :local 'after-change-major-mode-hook
      #'popup|set-modeline-on-enable)
    (when (timerp +popup--timer)
      (remove-hook! :local 'kill-buffer-hook #'+popup-kill-buffer)
      (cancel-timer +popup--timer)
      (setq +popup--timer nil))))

(put '+popup-buffer-mode 'permanent-local t)
(put '+popup-buffer-mode 'permanent-local-hook t)
(put '+popup-set-modelne-on-enable 'permanent-local-hook t)

(add-hook! '+popup-buffer-mode-hook #'(+popup-adjust-fringes
                                       +popup-adjust-margins
                                       +popup-set-modeline-on-enable
                                       +popup-unset-modeline-on-disable))

(define-key! +popup-buffer-mode-map
  [escape] #'global-escape)

(add-hook 'init-ui-hook #'+popup-mode)

(defmacro +popup-with-rules (rules &rest body)
  "Evaluate BODY with popup RULES.

  RULES is a list of rules. Each rule should match the arguments of `+popup-set-popup-rules'."
  (declare (indent defun))
  `(let ((+popup--display-buffer-alist +popup--old-display-buffer-alist)
         display-buffer-alist)
     (+popup-set-rules ,@rules)
     (when (bound-and-true-p +popup-mode)
       (setq display-buffer-alist +popup--display-buffer-alist))
     ,@body))

(defmacro +popup-ignore-popups (&rest body)
  `(let* ((in-popup-p (+popup-buffer-p))
          (popups (+popup-windows))
          (+popup--inhibit-transient t)
          +popup--last)
     (dolist (p popups)
       (+popup-close-popup p 'force))
     (unwind-protect
         (progn ,@body)
       (when popups
         (let ((origin (selected-window)))
           (+popup-restore)
           (unless in-popup-p
             (select-window origin)))))))

(defun +popup-init-rules ()
  (+popup-set-rules
   '(("^\\*"  :slot 1 :vslot -1 :select t)
     ("^ \\*" :slot 1 :vslot -1 :size +popup-shrink-to-fit))
   '(("^\\*Completions"
      :slot -1 :vslot -2 :ttl 0)
     ("^\\*Compil\\(?:ation\\-e-Log\\)"
      :size 0.3 :ttl 0 :quit t)
     ("^\\*\\(?:scratch\\-Messages\\)"
      :autosave t :ttl nil)
     ("^\\*Man "
      :size 0.45 :vslot -6 :ttl 0 :quit t :select t)
     ("^\\*doom \\(?:term\\-eshell\\)"
      :size 0.25 :vslot -10 :select t :quit nil :ttl 0)
     ("^\\*doom:"
      :size 0.35 :size bottom :autosave t :select t :modeline t :quit nil)
     ("^\\*\\(?:\\(?:Pp E\\-doom e\\)val\\)"
      :size +popup-shrink-to-fit :ttl 0 :select ignore)
     ("^\\*Customize"
      :slot 2 :side right :select t :quit t)
     ("^ \\*undo-tree\\*"
      :slot 2 :side left :size 20 :select t :quit t)
     ;; `help-mode', `helpful-mode'
     ("^\\*[Hh]elp"
      :slot 2 :vslot 2 :size 0.35 :select t)
     ;; `Info-mode'
     ("^\\*info\\*$"
      :slot 2 :vslot 2 :size 0.45 :select t))
   '(("^\\*Backtrace" :ignore t))))
(add-hook 'init-ui-hook #'+popup-init-rules)

(provide 'config-popups)
;;; config-popups.el ends here

;;; lisp/init-keybinds.el -*- lexical-binding: t; -*-

(use-package which-key
  :demand t
  :config
  (setq which-key-sort-order #'which-key-prefix-then-key-order
        which-key-sort-uppercase-first nil
        which-key-add-column-padding 1
        which-key-max-display-columns nil
        which-key-min-display-lines 5)
  (set-face-attribute 'which-key-local-map-description-face nil :weight 'bold)
  (which-key-setup-side-window-bottom)
  (add-hook 'nx-init-hook #'which-key-mode))

(use-package hydra
  :demand t
  :config
  (setq lv-use-separator t)
  (defhydra doom@window-nav (:hint nil)
    "
          Split: _v_ert  _s_:horz
         Delete: _c_lose  _o_nly
  Switch Window: _h_:left  _j_:down  _k_:up  _l_:right
        Buffers: _p_revious  _n_ext  _b_:select  _f_ind-file
         Resize: _H_:splitter left  _J_:splitter down  _K_:splitter up  _L_:splitter right
           Move: _a_:up  _z_:down  _i_menu
"
    ("z" scroll-up-line)
    ("a" scroll-down-line)
    ("i" idomenu)

    ("h" windmove-left)
    ("j" windmove-down)
    ("k" windmove-up)
    ("l" windmove-right)

    ("p" nx/previous-buffer)
    ("n" nx/next-buffer)
    ("b" switch-to-buffer)
    ("f" find-file)

    ("s" split-window-below)
    ("v" split-window-right)

    ("c" delete-window)
    ("o" delete-other-windows)

    ("H" hydra-move-splitter-left)
    ("J" hydra-move-splitter-down)
    ("K" hydra-move-splitter-up)
    ("L" hydra-move-splitter-right)

    ("q" nil)))

(defun nx--keybind-register (key desc &optional modes)
  "Register a description for KEY with `which-key' in MODES.

KEY should be a string in KBD format.
DESC should be a docstring.
MODES shoud be a list of major mode symbols."
  (if modes
      (dolist (mode modes)
        (which-key-add-major-mode-key-based-replacements mode key desc))
    (which-key-add-key-based-replacements key desc)))

(put ':after  'lisp-indent-function 'defun)
(put ':desc   'lisp-indent-function 'defun)
(put ':local  'lisp-indent-function 'defun)
(put ':map    'lisp-indent-function 'defun)
(put ':map*   'lisp-indent-function 'defun)
(put ':mode   'lisp-indent-function 'defun)
(put ':prefix 'lisp-indent-function 'defun)
(put ':unless 'lisp-indent-function 'defun)
(put ':when   'lisp-indent-function 'defun)

(defvar nx--keymaps nil)
(defvar nx--prefix  nil)
(defvar nx--defer   nil)
(defvar nx--local   nil)

(defmacro map! (&rest rest)
  "A nightmare of a key-binding macro that will use `define-key', `local-set-key'
and `global-set-key' depending on context and plist key flags. It was designed to
make binding multiple keys more concise, like in vim.

Flags
    (:mode [MODE(s)] [...])    inner keybinds are applied to major MODE(s)
    (:map [KEYMAP(s)] [...])   inner keybinds are applied to KEYMAP(S)
    (:map* [KEYMAP(s)] [...])  same as :map, but deferred
    (:prefix [PREFIX] [...])   assign prefix to all inner keybindings
    (:after [FEATURE] [...])   apply keybinds when [FEATURE] loads
    (:local [...])             make bindings buffer local; incompatible with keymaps!

Conditional keybinds
    (:when [CONDITION] [...])
    (:unless [CONDITION] [...])"
  (let ((nx--keymaps nx--keymaps)
        (nx--prefix  nx--prefix)
        (nx--defer   nx--defer)
        (nx--local   nx--local)
        key def forms desc modes)
    (while rest
      (setq key (pop rest))
      (cond
       ;; it's a sub expr
       ((listp key)
        (push (macroexpand `(map! ,@key)) forms))

       ;; it's a flag
       ((keywordp key)
        (pcase key
          (:when    (push `(if ,(pop rest)       ,(macroexpand `(map! ,@rest))) forms) (setq rest '()))
          (:unless  (push `(if (not ,(pop rest)) ,(macroexpand `(map! ,@rest))) forms) (setq rest '()))
          (:after   (push `(after! ,(pop rest)   ,(macroexpand `(map! ,@rest))) forms) (setq rest '()))
          (:desc    (setq desc (pop rest)))
          (:map*    (setq nx--defer t) (push :map rest))
          (:map
           (setq nx--keymaps (nx-enlist (pop rest))))
          (:mode
           (setq modes (nx-enlist (pop rest)))
           (unless nx--keymaps
             (setq nx--keymaps
                   (cl-loop for m in modes
                            collect (intern (format "%s-map" (symbol-name m)))))))
          (:prefix
           (let ((def (pop rest)))
             (setq nx--prefix `(vconcat ,nx--prefix (kbd ,def)))
             (when desc
               (push `(nx--keybind-register ,(key-description (eval nx--prefix))
                                              ,desc ',modes)
                     forms)
               (setq desc nil))))
          (:local
           (setq nx--local t))
          (_ (user-error "%s is not a valid flag for map!" key))))

       ;; It's a key-def pair
       ((or (stringp key)
            (characterp key)
            (vectorp key)
            (symbolp key))
        (unwind-protect
            (catch 'skip
              (when (symbolp key)
                (setq key `(kbd ,key)))
              (when (stringp key)
                (setq key (kbd key)))
              (when nx--prefix
                (setq key (append nx--prefix (list key))))
              (unless (> (length rest) 0)
                (user-error "map! has no definition for %s key" key))
              (setq def (pop rest))
              (when desc
                (push `(nx--keybind-register ,(key-description (eval key))
                                               ,desc ',modes)
                      forms))
              (cond ((and nx--local nx--keymaps)
                     (push `(lwarn 'nx-map :warning
                                   "Can't local bind '%s' key to a keymap; skipped"
                                   ,key)
                           forms)
                     (throw 'skip 'local))
                    (nx--keymaps
                     (dolist (keymap nx--keymaps)
                       (push `(define-key ,keymap ,key ,def) forms)))
                    (t
                     (push `(,(if nx--local 'local-set-key 'global-set-key) ,key ,def)
                           forms))))
          (setq nx--local nil
                desc nil)))

       (t (user-error "Invalid key %s" key))))
       `(progn ,@(nreverse forms))))

(provide 'init-keybinds)

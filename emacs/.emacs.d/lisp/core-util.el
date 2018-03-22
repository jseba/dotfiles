;;; core-util.el -- Core utility functions/macros

;;; Commentary:
;;; Code:

(defmacro after! (feature &rest forms)
  "A smart wrapper around `with-eval-after-load'

Suppresses warnings during compilation."
  (declare (indent defun) (debug t))
  `(,(if (or (not (bound-and-true-p byte-compile-current-file))
             (if (symbolp feature)
                 (require feature nil :no-error)
               (load feature :no-message :no-error)))
         #'progn
       #'with-no-warnings)
    (with-eval-after-load ',feature ,@forms)))

(eval-and-compile
  (defun nx-enlist (exp)
    "Returns EXP as a list or as-is if already a list."
    (if (listp exp) exp (list exp)))

  (defun nx-unquote (exp)
    "Return EXP unquoted."
    (while (memq (car-safe exp) '(quote function))
      (setq exp (cadr exp)))
    exp)
  
  (defun nx--resolve-hook-forms (hooks)
    (cl-loop with quoted-p = (eq (car-safe hooks) 'quote)
             for hook in (nx-enlist (nx-unquote hooks))
             if (eq (car-safe hook) 'quote)
             collect (cadr hook)
             else if quoted-p
             collect hook
             else
             collect (intern (format "%s-hook" (symbol-name hook)))))

  (defvar nx--transient-counter 0)
  (defmacro add-transient-hook! (hook &rest forms)
    "Attaches transient forms to a HOOK.

HOOK can be a quoted hook or a sharp-quoted function (which will be advised).

These forms will be evaluated once when that function/hook is first invoked,
then it detaches itself."
    (declare (indent 1))
    (let ((append (eq (car forms) :after))
    (fn (intern (format "nx--transient-hook-%s" (cl-incf nx--transient-counter)))))
      `(when ,hook
   (fset ',fn
         (lambda (&rest _)
     ,@forms
     (cond ((functionp ,hook) (advice-remove ,hook #',fn))
           ((symbolp ,hook)   (remove-hook ,hook #',fn)))
     (unintern ',fn nil)))
   (cond ((functionp ,hook)
    (advice-add ,hook ,(if append :after :before) #',fn))
         ((symbolp ,hook)
    (add-hook ,hook #',fn ,append)))))))

(defmacro add-hook! (&rest args)
  "A convenience macro for `add-hook'.

Takes, in order:
1. Optional properties :local and/or :append, which will make the hook
   buffer-local or append to the list of hooks (respectively).
2. The hooks: either an unquoted major mode, an unquoted list of major-modes,
   a quoted hook variable or a quoted list of hook variables. If unqutoed, the
   hooks will be resolved by appending -hook to each symbol.
3. A function, list of functions, or body forms to be wrapped in a lambda.

Body forms can access the hook's arguments through the let-bound variable
`ARGS'."
  (declare (indent defun) (debug t))
  (let ((hook-fn 'add-hook)
  append-p local-p)
    (while (keywordp (car args))
      (pcase (pop args)
  (:append (setq append-p t))
  (:local (setq local-p t))
  (:remove (setq hook-fn 'remove-hook))))
    (let ((hooks (nx--resolve-hook-forms (pop args)))
    (funcs
     (let ((val (car args)))
       (if (memq (car-safe val) '(quote function))
     (if (cdr-safe (cadr val))
         (cadr val)
       (list (cadr val)))
         (list args))))
    forms)
      (dolist (fn funcs)
  (setq fn (if (symbolp fn)
         `(function ,fn)
       `(lambda (&rest _) ,@args)))
  (dolist (hook hooks)
    (push (cond ((eq hook-fn 'remove-hook)
           `(remove-hook ',hook ,fn ,local-p))
          (t
           `(add-hook ',hook ,fn ,append-p ,local-p)))
    forms)))
      `(progn ,@(nreverse forms)))))

(defmacro remove-hook! (&rest args)
  "Convenience macro for `remove-hook'."
  `(add-hook! :remove ,@args))

(defmacro quiet! (&rest forms)
  "Run FORMS without making any noise."
  `(if nil
       (progn ,@forms)
     (fset 'nx--old-write-region-fn (symbol-function 'write-region))
     (cl-letf ((stdout (lambda (&rest _)))
         ((symbol-function 'load-file) (lambda (file) (load file nil t)))
         ((symbol-function 'message) (lambda (&rest _)))
         ((symbol-function 'write-region)
    (lambda (start end filename &optional append visit lockname mustbenew)
      (unless visit (setq visit 'no-message))
      (nx--old-write-region-fn
       start end filename append visit lockname mustbenew)))
         (inhibit-message t)
         (save-silently t))
       ,@forms)))

(defmacro defadvice! (advice-name commands class &rest body)
  "Apply advice named ADVICE-NAME to multiple COMMANDS.

The body of the advice is in BODY."
  `(progn
     ,@(mapcar (lambda (command)
     `(defadvice ,command
          (,class ,(intern (format "%S-%s" command advice-name))
            activate)
        ,@body))
               commands)))

(provide 'core-util)
;;; core-util.el ends here

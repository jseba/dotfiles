;;; config-util.el

(defun unquote (exp)
  "Return EXP unqouted."
  (while (memq (car-safe exp) '(quote function))
	(setq exp (cadr exp)))
  exp)

(defun enlist (exp)
  "Return EXP wrapped in a list or as-is if already a list."
  (if (listp exp) exp (list exp)))

(defun resolve-hook-forms (hooks)
  (cl-loop with quoted-p = (eq (car-safe hooks) 'quote)
		   for hook in (enlist (unquote hooks))
		   if (eq (car-safe hook) 'quote)
		   collect (cadr hook)
		   else if quoted-p
		   collect hook
		   else
		   collect (intern (format "%s-hook" (symbol-name hook)))))

(defmacro add-hook! (&rest args)
  "A convenience macro for `add-hook'. Takes, in order:
  1. Optional properties :local and/or :append, which will make the hook
	 buffer-local or append to the list of hooks (respectively),
  2. The hooks: either an unquoted major mode, an unquoted list of major-modes,
	 a quoted hook variable or a quoted list of hook variables. If unquoted, the
	 hooks will be resolved by appending -hook to each symbol.
  3. A function, list of functions, or body forms to be wrapped in a lambda.
Examples:
	(add-hook! 'some-mode-hook 'enable-something)   (same as `add-hook')
	(add-hook! some-mode '(enable-something and-another))
	(add-hook! '(one-mode-hook second-mode-hook) 'enable-something)
	(add-hook! (one-mode second-mode) 'enable-something)
	(add-hook! :append (one-mode second-mode) 'enable-something)
	(add-hook! :local (one-mode second-mode) 'enable-something)
	(add-hook! (one-mode second-mode) (setq v 5) (setq a 2))
	(add-hook! :append :local (one-mode second-mode) (setq v 5) (setq a 2))
Body forms can access the hook's arguments through the let-bound variable
`args'."
  (declare (indent defun) (debug t))
  (let ((hook-fn 'add-hook)
		append-p local-p)
	(while (keywordp (car args))
	  (pcase (pop args)
		(:append (setq append-p t))
		(:local  (setq local-p t))
		(:remove (setq hook-fn 'remove-hook))))
	(let ((hooks (resolve-hook-forms (pop args)))
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
		  (push (if (eq hook-fn 'remove-hook)
					`(remove-hook ',hook ,fn ,local-p)
				  `(add-hook ',hook ,fn ,append-p ,local-p))
				forms)))
	  `(progn ,@(if append-p (nreverse forms) forms)))))

(defmacro remove-hook! (&rest args)
  "Convenience macro for `remove-hook'. Takes the same arguments as
`add-hook!'."
  (declare (indent defun) (debug t))
  `(add-hook! :remove ,@args))

(defmacro add-transient-hook! (hook &rest forms)
  "Attaches transient FORMS to a HOOK.

This means that FORMS will be evaluated once when first invoked and then
deleted.

HOOK can be a quoted hook or a sharp-quoted function, which will be advised."
  (declare (indent 1))
  (let ((append (if (eq (car forms) :after) (pop forms)))
		(fn (if (symbolp (car forms))
				(intern (format "transient-hook--%s" (pop forms)))
			  (gensym "transient-hook--"))))
	`(progn
	   (fset ',fn
			 (lambda (&rest _)
			   ,@forms
			   (cond ((functionp ,hook) (advice-remove ,hook #',fn))
					 ((symbolp   ,hook) (remove-hook   ,hook #',fn)))
			   (fmakunbound ',fn)
			   (unintern ',fn nil)))
	   (cond ((functionp ,hook)
			  (advice-add ,hook ,(if append :after :before) #',fn))
			 ((symbolp ,hook)
			  (put ',fn 'permanent-local-hook t)
			  (add-hook ,hook #',fn ,append))))))

(defmacro setq-hook! (hooks &rest rest)
  "Convenience macro for setting buffer-local variables in a hook."
  (declare (indent defun) (debug t))
  (unless (= 0 (% (length rest) 2))
	(signal 'wrong-number-of-arguments (length rest)))
  `(add-hook! ,hooks
	 ,@(let (forms)
		 (while rest
		   (let ((var (pop rest))
				 (val (pop rest)))
			 (push `(setq-local ,var ,val) forms)))
		 (nreverse forms))))

(defmacro lambda! (&rest body)
  "Convenience macro for inline interactive lambdas."
  (declare (doc-string 1))
  `(lambda () (interactive) ,@body))

(defmacro quiet! (&rest forms)
  "Run FORMS without any output."
  `(if %debug-mode
	   (progn ,@forms)
	 (let ((old-fn (symbol-function 'write-region)))
	   (cl-letf* ((standard-output (lambda (&rest _)))
				  ((symbol-function 'load-file) (lambda (file) (load file nil t)))
				  ((symbol-function 'message) (lambda (&rest _)))
				  ((symbol-function 'write-region)
				   (lambda (start end filename &optional append visit lockname mustbenew)
					 (unless visit (setq visit 'no-message))
					 (funcall old-fn start end filename append visit lockname mustbenew)))
				  (inhibit-message t)
				  (save-silently t))
		 ,@forms))))

(defmacro after! (targets &rest body)
  "A smart wrapper around `with-eval-after-load'. Supresses warnings during
compilation."
  (declare (indent defun) (debug t))
  (unless (symbolp targets)
	(list (if (or (not (bound-and-true-p byte-compile-current-file))
				  (dolist (next (%-enlist targets))
					(if (symbolp next)
						(require next nil :no-error)
					  (load next :no-message :no-error))))
			  #'progn
			#'with-no-warnings)
		  (if (symbolp targets)
			  `(with-eval-after-load ',targets ,@body)
			(pcase (car-safe targets)
			  ((or :or :any)
			   (macroexp-progn
				(cl-loop for next in (cdr targets)
						 collect `(after! ,next ,@body))))
			  ((or :and :all)
			   (dolist (next (cdr targets))
				 (setq body `((after! ,next ,@body))))
			   (car body))
			  (_ `(after! (:and ,@targets) ,@body)))))))

(defmacro defer-until! (condition &rest body)
  "Execute BODY when CONDTION becomes true."
  (declare (indent defun) (debug t))
  `(if ,condition
	   (progn ,@body)
	 ,(let ((fun (gensym "%|delay-")))
		`(progn
		   (fset ',fun (lambda (&rest args)
						 (when ,(or condition t)
						   (remove-hook 'after-load-functions #',fun)
						   (unintern ',fun nil)
						   (ignore args)
						   ,@body)))
		   (put ',fun 'permanent-local-hook t)
		   (add-hook 'after-load-functions #',fun)))))

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

(defmacro define-key! (keymaps key def &rest rest)
  "Like `define-key', but accepts a variable number of KEYMAPS and/or KEY+DEFs.

KEYMAPS can also be (or contain) `\'global' or `\'local' to make this equivalent
to using `global-set-key' or `local-set-key'.

KEY is a key string or vector. It is *not* piped through `kbd'."
  (declare (indent defun))
  (or (cl-evenp (length rest))
	  (signal 'wrong-number-of-arguments (list 'evenp (length rest))))
  (if (and (listp keymaps)
		   (not (eq (car-safe keymaps) 'quote)))
	  `(dolist (map (list ,@keymaps))
		 ,(macroexpand `(define-key! map ,key ,def ,@rest)))
	(when (eq (car-safe keymaps) 'quote)
	  (pcase (cadr keymaps)
		(`global (setq keymaps '(current-global-map)))
		(`local  (setq keymaps '(current-local-map)))
		(x (error "%s is not a valid keymap" x))))
	`(let ((map ,keymaps))
	   (define-key map ,key ,def)
	   ,@(let (forms)
		   (while rest
			 (let ((key (pop rest))
				   (def (pop rest)))
			   (push `(define-key map ,key ,def) forms)))
		   (nreverse forms)))))

(provide 'config-util.el)
;;; config-util.el ends here

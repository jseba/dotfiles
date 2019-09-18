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
  "Converts a list of modes into a list of hook symbols."
  (declare (pure t) (side-effect-free t))
  (let ((hook-list (enlist (unquote hooks))))
    (if (eq (car-safe hooks) 'quote)
	hook-list
      (cl-loop for hook in hook-list
	       if (eq (car-safe hook) 'quote)
	       collect (cadr hook)
	       else collect (intern (format "%s-hook" (symbol-name hook)))))))

(defun resolve-setq-hook-forms (hooks rest &optional singles)
  (unless (or singles (= 0 (% (length rest) 2)))
    (signal 'wrong-number-of-arguments (listp #'evenp (length rest))))
  (cl-loop with vars = (let ((args rest)
			     vars)
			 (while args
			   (push (if singles
				     (list (pop args))
				   (cons (pop args) (pop args)))
				 vars))
			 (nreverse vars))
	   for hook in (resolve-hook-forms hooks)
	   for mode = (string-remove-suffix "-hook" (symbol-name hook))
	   append
	   (cl-loop for (var . val) in vars
		    collect (list var val hook
				  (intern (format "setq-hook--%s-for-%s"
						  var mode))))))

(defmacro add-hook! (hooks &rest rest)
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
  (declare (indent (lambda (indent-point state)
		     (goto-char indent-point)
		     (when (looking-at-p "\\s-*(")
		       (lisp-indent-defform state indent-point))))
	   (debug t))
  (let* ((hook-forms (resolve-hook-forms hooks))
	 (func-forms ())
	 (defn-forms ())
	 append-p
	 local-p
	 remove-p
	 forms)
    (while (keywordp (car rest))
      (pcase (pop rest)
	(:append (setq append-p t))
	(:local (setq local-p t))
	(:remove (setq remove-p t))))
    (let ((first (car-safe (car rest))))
      (cond ((null first)
	     (setq func-forms rest))

	    ((eq first 'defun)
	     (setq func-forms (mapcar #'cadr rest)
		   defn-forms rest))

	    ((memq first '(quote function))
	     (setq func-forms
		   (if (cdr rest)
		       (mapcar #'unquote rest)
		     (enlist (unquote (car rest))))))
	    
	    ((setq func-forms (list `(lambda () ,@rest)))))
      (dolist (hook hook-forms)
	(dolist (func func-forms)
	  (push (if remove-p
		    `(remove-hook ',hook #',func ,local-p)
		  `(add-hook ',hook #',func ,append-p ,local-p))
		forms)))
      (macroexp-progn
       (append defn-forms
	       (if append-p
		   (nreverse forms)
		 forms))))))

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

(defmacro setq-hook! (hooks &rest var-vals)
  "Convenience macro for setting buffer-local variables in a hook."
  (declare (indent 1))
  (macroexp-progn
   (cl-loop for (var val hook fn) in (resolve-setq-hook-forms hooks var-vals)
	    collect `(defun ,fn (&rest _)
		       ,(format "%s = %s" var (pp-to-string val))
		       (setq-local ,var ,val))
	    collect `(remove-hook ',hook #',fn) ; ensure set order
	    collect `(add-hook ',hook #',fn))))

(defmacro unsetq-hook! (hooks &rest vars)
  "Unbind `setq' hooks on HOOKS for VARS."
  (declare (indent 1))
  (macroexp-progn
   (cl-loop for (_var _val hook fn) in (resolve-setq-hook-forms hooks vars 'singles)
	    collect `(remove-hook ',hook #',fn))))

(defmacro lambda! (&rest body)
  "Convenience macro for inline interactive lambdas."
  (declare (doc-string 1))
  `(lambda () (interactive) ,@body))

(defmacro lambda!! (command &optional arg)
  "Expands to a command that interactively calls COMMAND with prefix ARG."
  (declare (doc-string 1))
  (lambda ()
    (interactive)
    (let ((current-prefix-arg arg))
      (call-interactively command))))

(defmacro setq! (&rest settings)
  "A stripped down `customize-set-variable' with the syntax of `setq'."
  (macroexp-progn
   (cl-loop for (val var)
	    on settings
	    by 'cddr
	    collect `(funcall (or (get ',var 'custom-set) #'set)
			      ',var ,val))))

(defmacro pushnew! (place &rest values)
  "Push VALUES sequentially into PLACE if they aren't already present.

This is essentially a variadic `cl-pushnew'."
  (let ((var (make-symbol "result")))
    `(dolist (,var (list ,@values) (with-no-warnings ,place))
       (cl-pushnew ,var ,place :test #'equal))))

(defmacro prependq! (sym &rest lists)
  "Prepend LISTS to SYM in place."
  `(setq ,sym (append ,@lists ,sym)))

(defmacro appendq! (sym &rest lists)
  "Append LISTS to SYM in place."
  `(setq ,sym (append ,sym ,@lists)))

(defmacro nconcq! (sym &rest lists)
  "Append LISTS to SYM by altering them in place."
  `(setq ,sym (nconc ,sym ,@lists)))

(defmacro delq! (elt list)
  "`delq' ELT from LIST in place."
  `(setq ,list (delq ,elt ,list)))

(defmacro quiet! (&rest forms)
  "Run FORMS without any output."
  `(if debug-on-error
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

(defmacro defadvice! (symbol arglist &optional docstring &rest body)
  "Define an advice called SYMBOL and add it to PLACES.

ARGLIST is as in `defun'. WHERE is a keyword passed to `advice-add'.
PLACE is the function to which to add the advice, like in `advice-add'.
DOCSTRING and BODY are as in `defun'."
  (declare (doc-string 3) (indent defun))
  (unless (stringp docstring)
    (push docstring body)
    (setq docstring nil))
  (let (where-alist)
    (while (keywordp (car body))
      (push `(cons ,(pop body) (enlist ,(pop body)))
	    where-alist))
    `(progn
       (defun ,symbol ,arglist ,docstring ,@body)
       ,(when where-alist
	  `(dolist (targets (list ,@(nreverse where-alist)))
	     (dolist (target (cdr targets))
	       (advice-add target (car targets) #',symbol)))))))

(defmacro defer-feature! (feature &optional fn)
  "Pretend FEATURE hasn't been loaded yet, until FEATURE-hook or FN runs.

Some packages (like `emacs-lisp-mode') are loaded immediately at startup,
which will prematurely trigger `after!' (and `with-eval-after-load') blocks.
To get arounds, we Emacs believe FEATURE hasn't been loaded yet, then wait
until FEATURE-hook (or MODE-hook, if FN is provided) is triggered to
reverse this and trigger `after!' blocks at a more reasonable time."
  (let ((advice-fn (intern (format "defer-feature--%s" feature)))
	(fn (or fn feature)))
    `(progn
       (setq features (delq ',feature features))
       (advice-add #',fn :before #',advice-fn)
       (defun ,advice-fn (&rest _)
	 (when (and ,(intern (format "%s-hook" fn))
		    (not delay-mode-hooks))
	   (provide ',feature)
	   (advice-remove #',fn #',advice-fn))))))

(defmacro file! ()
  "Evaluates to the file this macro is called from."
  (cond ((bound-and-true-p byte-compile-current-file))
	(load-file-name)
	((stringp (car-safe current-load-list))
	 (car current-load-list))
	(buffer-file-name)
	((error "Cannot get this file path"))))

(defmacro dir! ()
  "Evaluates to the directory of the file this macro is called from."
  (when-let (path (file!))
    (directory-file-name (file-name-directory path))))

(defmacro load! (filename &optional path noerror)
  "Load a file relative to the current executing file (`load-file-name').

FILENAME should be a file path string. PATH is where to look for the file;
if omitted, the lookup is relative to either `load-file-name', `byte-compile-current-file'
or `buffer-file-name', in that order.

If NOERROR is non-nil, don't throw an error if the file does not exist."
  (unless path
    (setq path (or (dir!)
		   (error "Could not detect path to look for '%s'"
			  filename))))
  (let ((file (if path
		  `(let (file-name-handler-alist)
		     (expand-file-name ,filename ,path))
		filename)))
    `(condition-case e
	 (load ,file ,noerror ,(not debug-on-error))
       ((debug error)
	(let* ((source (file-name-sans-extension ,file)))
	  (signal 'file-missing
		  (list (file-relative-name
			 (concat source ".el")
			 path))
		  e))))))

(provide 'config-util.el)
;;; config-util.el ends here

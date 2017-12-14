;;; core/init-lib.el -*- lexical-binding: t; -*-

(defvar nx-settings nil)

(defmacro def-setting! (keyword arglist &optional docstring &rest forms)
  "Define a setting. FORMS are not evaluated until `set!' is called."
  (declare (indent defun) (doc-string 3))
  (unless (keywordp keyword)
    (error "Not a valid property name: %s" keyword))
  (let ((fn (intern (format "nx--set%s" keyword))))
    `(progn
       (defun ,fn ,arglist
	 ,docstring
	 ,@forms)
       (cl-pushnew ',(cons keyword fn) nx-settings :test #'eq :key #'car))))

(defmacro set! (keyword &rest values)
  "Set an option defined by `def-setting!'. Skip if it doesn't exit."
  (declare (indent defun))
  (unless values
    (error "Empty set! for %s" keyword))
  (let ((fn (cdr (assq keyword nx-settings))))
    (if fn
	(apply fn values)
      (when nx-debug-mode
	(message "No setting found for %s" keyword)
	nil))))

(use-package dash
  :demand t
  :ensure t)

(provide 'init-lib)

;;;  private/jseba/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun nx|dont-kill-scratch-buffer ()
  "Don't kill the scratch buffer."
  (or (not (string= (buffer-file-name) "*scratch*"))
      (ignore (bury-buffer))))

;;;###autoload
(defun nx/smart-newline-and-indent ()
  "Insert a newline and possibly indent it.

Also continues comments if executed from a commented line and handles
special cases for certain languages with weak native support."
  (interactive)
  (cond ((sp-point-in-string)
         (newline))
        ((sp-point-in-comment)
         (pcase major-mode
           ((or 'js2-mode 'rjsx-mode)
            (call-interactively #'js2-line-break))
           ((or 'java-mode 'php-mode)
            (c-indent-new-comment-line))
           ((or 'c-mode 'c++-mode 'objc-mode 'css-mode 'scss-mode 'js2-mode)
            (newline-and-indent)
            (insert "* ")
            (indent-according-to-mode))
           (_
            ;; Fix an off-by-one cursor-positioning issue
            ;; with `indent-new-comment-line'
            (let ((col (save-excursion (comment-beginning) (current-column))))
              (indent-new-comment-line)
              (unless (= col (current-column))
                (insert " "))))))
        (t
         (newline nil t)
         (indent-according-to-mode))))

;;;###autoload
(defun nx/newline-at-end-of-line ()
  "Move to end of line, enter a newline and reindent."
  (interactive)
  (move-end-of-line 1)
  (nx/smart-newline-and-indent))

;;;###autoload
(defun nx/smart-kill-whole-line (&optional arg)
  "Kill whole line and move back to indentation.

With ARG."
  (interactive "p")
  (kill-whole-line arg)
  (back-to-indentation))

;;;###autoload
(defun nx/smart-move-beginning-of-line ()
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between first non-whitespace character and
the beginning of the line."
  (interactive)
  (if (bound-and-true-p visual-line-mode)
      (beginning-of-visual-line)
    (let ((ci (current-indentation))
          (cc (current-column)))
      (cond ((or (> cc ci) (= cc 0))
             (back-to-indentation))
            ((<= cc ci)
             (beginning-of-visual-line))))))

;;;###autoload
(defun nx/smart-move-end-of-line ()
  "Move forward to the last non-blank character in the line.

Ignores comments and trailing whitespace. If already there, move
to the real end of the line. If already there, do nothing."
  (interactive)
  (let* ((point (point))
         (eol (save-excursion (end-of-visible-line) (point)))
         (bol (save-excursion (beginning-of-visual-line) (point)))
         (eoc (or (if (not comment-use-syntax)
                      (when (re-search-forward comment-start-skip eol t)
                        (or (match-end 1) (match-beginning 0)))
                    (save-excursion
                      (goto-char eol)
                      (while (and (sp-point-in-comment)
                                  (> (point) point))
                        (backward-char))
                      (when (> (point) point)
                        (skip-chars-backward " " bol)
                        (point))))
                  eol)))
    (if (= eoc point)
	    (goto-char eol)
      (unless (= eol point)
	    (goto-char eoc)))))

;;;###autoload
(defun nx/smart-back-to-indentation (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line. If point is
already there, move to the beginning of the line. Effectively toggle between
the first non-whitespace character and the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first. If point reaches the
beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

;;;###autoload
(defun nx/kill-back-to-indentation ()
  "Kill from point back to the first non-whitespace character on the current line."
  (interactive)
  (let ((prev-pos (point)))
    (back-to-indentation)
    (kill-region (point) prev-pos)))

;;;###autoload
(defun nx/smart-backward-sexp (arg)
  "Jump up to the start of the ARG'th enclosing sexp."
  (interactive "p")
  (let ((ppss (syntax-ppss)))
    (cond
     ((elt ppss 3)
      (goto-char (elt ppss 8))
      (backward-up-sexp (1- arg)))
     (backward-up-list arg))))

;;;###autoload
(defun nx/smart-backward-kill-line ()
  "Kill line to the first non-blank character.

If invoked again afterwards, kill line to column 1."
  (interactive)
  (let ((empty-line-p (save-excursion (beginning-of-line)
                                      (looking-at-p "[ \t]*$"))))
    (delete-region (point-at-bol) (point))
    (unless empty-line-p
      (indent-according-to-mode))))

;;;###autoload
(defun nx/backward-delete-whitespace-to-column ()
  "Delete back to the previous column of whitespace.

Fallback to either as much whitespace as possible or just one char."
  (interactive)
  (let* ((delete-backward-char (if (derived-mode-p 'org-mode)
                                   #'org-delete-backward-char
                                 #'delete-backward-char))
         (context (sp--get-pair-list-context 'navigate))
         (open-pair-re (sp--get-opening-regexp context))
         (close-pair-re (sp--get-closing-regexp context))
         open-len close-len)
    (cond ;; When in strings (sp acts weird with quotes; this is the fix)
     ;; Also, skip closing delimiters
     ((and (and (sp--looking-back open-pair-re)
		        (setq open-len (- (match-beginning 0) (match-end 0))))
	       (and (looking-at close-pair-re)
		        (setq close-len (- (match-beginning 0) (match-end 0))))
	       (string= (plist-get (sp-get-thing t) :op)
		            (plist-get (sp-get-thing) :cl)))
      (delete-char (- 0 open-len))
      (delete-char close-len))

     ;; Delete up to the nearest tab column IF only whitespace between
     ;; point and bol.
     ((save-match-data (looking-back "^[\\t ]*" (line-beginning-position)))
      (let ((movement (% (current-column) tab-width))
	        (p (point)))
	    (when (= movement 0)
	      (setq movement tab-width))
	    (save-match-data
	      (if (string-match "\\w*\\(\\s-+\\)$"
			                (buffer-substring-no-properties (max (point-min) (- p movement)) p))
	          (sp-delete-char
	           (- 0 (- (match-end 1)
		               (match-beginning 1))))
	        (call-interactively delete-backward-char)))))

     ;; Otherwise do a regular delete
     (t (call-interactively delete-backward-char)))))

;;;###autoload
(defun nx/inflate-space-maybe ()
  "Add a space if point is surrounded by {} [] () delimiters."
  (interactive)
  (let ((command (or (command-remapping #'self-insert-command)
                     #'self-insert-command)))
    (cond ((nx--surrounded-p)
           (call-interactively command)
           (save-excursion (call-interactively command)))
          (t
           (call-interactively command)))))

;;;###autoload
(defun nx/deflate-space-maybe ()
  "Add a space if point is surrounded by {} [] () delimiters.

Resorts to `nx/backward-delete-whitespace-to-column' otherwise."
  (interactive)
  (save-match-data
    (if (nx--surrounded-p)
        (let ((whitespace-match (match-string 1)))
          (cond ((not whitespace-match)
                 (call-interactively #'delete-backward-char))
                ((string-match "\n" whitespace-match)
		         (delete-region (point-at-bol) (point))
                 (call-interactively #'delete-backward-char)
                 (save-excursion (call-interactively #'delete-char)))
                (t (just-one-space 0))))
      (nx/backward-delete-whitespace-to-column))))

;;;###autoload
(defun nx/isearch-yank-symbol ()
  "Put symbol at current point into search string."
  (interactive)
  (let ((sym (symbol-at-point)))
    (if sym
	    (progn
	      (setq isearch-regexp t
		        isearch-string (concat "\\_<" (regexp-quote (symbol-name sym)) "\\_>")
		        isearch-message (mapconcat 'isearch-text-char-description isearch-string "")
		        isearch-yank-flag t))
	  (ding)))
  (isearch-search-and-update))

;;;###autoload
(defun nx/isearch-exit-other-end (&optional _rbeg _rend)
  "Exit isearch, but at the other end of the search string.
This is useful when followed by an immediate kill.

RBEG and REND denote the `isearch' region."
  (interactive "r")
  (isearch-exit)
  (goto-char isearch-other-end))


(use-package buffers
  :init
  (defconst lunaryorn/do-not-kill-buffer-names '("*scratch*" "*Messages*")
    "Names of buffers that should not be killed.")
  
  (defun lunaryorn/do-not-kill-important-buffers ()
    "Inhibit killing of important buffers."
    (if (not (member (buffer-name) lunaryorn/do-not-kill-buffer-names))
	t
      (message "Not allowed to kill %s, burying instead" (buffer-name))
      (bury-buffer)
      nil))

  (defun lunaryorn/kill-this-buffer ()
    "Kill the current buffer."
    (interactive)
    (kill-buffer (current-buffer)))
  
  (add-hook 'kill-buffer-query-functions #'lunaryorn/do-not-kill-important-buffers)

  :bind
  (("C-c b k" . lunaryorn/kill-this-buffer)))

;; Better buffer list
(use-package ibuffer
  :bind
  (([remap list-buffers] . ibuffer))
  :init
  (defun ibuffer-set-up-preferred-filters ()
    (ibuffer-vc-set-filter-groups-by-vc-root)
    (unless (eq ibuffer-sorting-mode 'filename/process)
      (ibuffer-do-sort-by-filename/process)))
  
  ;; show version control status in ibuffer
  (setq ibuffer-formats
        '((mark modified read-only vc-status-mini " "
                (name 18 18 :left :elide)
                " "
                (size 9 -1 :right)
                " "
                (mode 16 16 :left :elide)
                " "
                (vc-status 16 16 :left)
                " "
                filename-and-process)
          (mark modified read-only " "
                (name 18 18 :left :elide)
                " "
                (size 9 -1 :right)
                " "
                (mode 16 16 :left :elide)
                " "
                filename-and-process)
          (mark " " (name 16 -1) " " filename))
	ibuffer-filter-group-face 'font-lock-doc-face)
  (setq-default ibuffer-show-empty-filter-groups nil)
  
  :config
  (define-ibuffer-column size-h
    (:name "Size" :inline t)
    (cond
     ((> (buffer-size) 1000000) (format "%7.1fM" (/ (buffer-size) 1000000.0)))
     ((> (buffer-size) 1000) (format "%7.1fk" (/ (buffer-size) 1000.0)))
     (t (format ("%8d" (buffer-size))))))
  (add-hook 'ibuffer-hook 'ibuffer-set-up-preferred-filters))

;; Group buffers by project and status
(use-package ibuffer-vc
  :ensure t
  :defer t
  :init
  (add-hook  'ibuffer-hook (lambda ()
                             (ibuffer-vc-set-filter-groups-by-vc-root)
                             (unless (eq ibuffer-sorting-mode 'alphabetic)
                               (ibuffer-do-sort-by-alphabetic)))))

(use-package uniquify
  :init
  (setq uniquify-buffer-name-style 'forward))

(setq display-buffer-alist
      ;; magit status window in a fullscreen window
      `((,(rx "*magit: ")
          (display-buffer-fullframe)
          (reusable-frames . nil))
        ;; Put REPLs and error lists into the bottom side window
        (,(rx bos
              (or "*Help"
                  "*Warnings*"
                  "*Compile-Log*"
                  "*compilation*"
                  "*Flycheck errors*"
                  "*shell"
                  "*sbt"
                  "*ensime-update*"
                  "*SQL"
                  "*Cargo"
                  (and (1+ nonl) " output*")
                  ))
          (display-buffer-reuse-window
           display-buffer-in-side-window)
          (side . bottom)
          (reusable-frames . visible)
          (window-height . 0.33))
        ("." nil (reusable-frames . visible))))

(provide 'init-ibuffer)

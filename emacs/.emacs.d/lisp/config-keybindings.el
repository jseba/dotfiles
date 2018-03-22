;;; config-keybindings.el -- Keybindings

;;; Commentary:
;;; Code:

(bind-keys ("C-j" . nx/smart-newline-and-indent)
           ([remap kill-whole-line] . nx/smart-kill-whole-line)
           ([remap move-beginning-of-line] . nx/smart-move-beginning-of-line)
           ([remap move-end-of-line] . nx/smart-move-end-of-line)
           ([remap back-to-indentation] . nx/smart-back-to-indentation)
           ([remap backward-sexp] . nx/smart-backward-sexp)
           ([backtab] . nx/dumb-dedent)
           ("M-DEL" . nx/smart-backward-kill-line)
           ("C-c e DEL" . nx/backward-delete-whitespace-to-column)
           ("C-c e i" . nx/inflate-space-maybe)
           ("C-c e x" . nx/deflate-space-maybe)
           ("C-c e r" . nx/retab)
           ([remap split-window-vertically] . nx/split-window-vertically-with-other-buffer)
           ([remap split-window-horizontally] . nx/split-window-horizontally-with-other-buffer)
           ([remap delete-other-windows] . nx/toggle-delete-other-windows)
           ("C-c w |" . nx/split-window-horizontally-instead)
           ("C-c w _" . nx/split-window-vertically-instead)
           ("C-c w S" . nx/split-window)
           ("C-c w d" . nx/toggle-current-window-dedication))

(after! align
  (bind-keys ("C-c x a a" . align)
             ("C-c x a c" . align-current)))

(after! browse-kill-ring
  (bind-keys ("M-Y" . browse-kill-ring)
              :map browse-kill-ring-mode-map
                ("C-g" . browse-kill-ring-quit)
                ("M-n" . browse-kill-ring-forward)
                ("M-p" . browse-kill-ring-previous)))

(after! zop-to-char
  (bind-keys ("M-z" . zop-to-char)
             ("M-Z" . zop-up-to-char)))

(after! avy
  (bind-keys ("C-;" . avy-goto-char-timer)
             ("C-c j j" . avy-goto-word-or-subword-1)
             ("C-c j w" . avy-goto-word-1)
             ("C-c j l" . avy-goto-line)
             ("C-c j m" . avy-pop-mark)
             ("C-c j c" . avy-goto-char-2)))

(after! anzu
  (bind-keys ([remap query-replace] . anzu-query-replace)
             ([remap query-replace-regexp] . anzu-query-replace-regexp)
             :map isearch-mode-map
               ([remap isearch-query-replace] . anzu-isearch-query-replace)
               ([remap isearch-query-replace-regexp] . anzu-isearch-query-replace-regexp)
               ([remap isearch-delete-char] . isearch-del-char)
               ("C-M-w" . isearch-yank-symbol)
               ("C-<return>" . isearch-exit-other-end)))

(after! ivy
  (bind-keys
   :map ivy-mode-map
     ([remap switch-to-buffer] . ivy-switch-buffer)
     ([remap imenu-anywhere]   . ivy-imenu-everywhere)
   :map ivy-minibuffer-map
     ("<return>" . ivy-alt-done)
     ("C-j" . ivy-immediate-done)
     ("C-<return>" . ivy-immediate-done)))

(after! counsel
  (bind-keys
   :map ivy-mode-map
     ([remap apropos] . counsel-apropos)
     ([remap describe-face] . counsel-describe-face)
     ([remap find-file] . counsel-find-file)
     ([remap recentf-open-files] . counsel-recentf)
     ([remap imenu] . counsel-imenu)
     ([remap bookmark-jump] . counsel-bookmark)
     ([remap execute-extended-command] . counsel-M-x)
     ([remap describe-function] . counsel-describe-function)
     ([remap describe-variable] . counsel-describe-variable)))

(after! swiper
  (bind-keys
   :map ivy-mode-map
     ("M-s /" . nx/swiper-at-point)))

(after! smex
  (bind-keys
   ([remap execute-extended-command] . smex)))

(after! prettify-symbols
  (bind-keys
   ("C-c t p" . prettify-symbols-mode)))

(after! symbol-overlay
  (bind-keys
   :map symbol-overlay-mode-map
     ("M-n" . symbol-overlay-jump-next)
     ("M-p" . symbol-overlay-jump-prev)))

(after! vc-git
  (bind-keys
   :map vc-prefix-map
     ("f" . vc-git-grep)))

(after! magit
  (bind-keys
   ("C-c g l" . magit-log)
   ("C-c g f" . magit-file-log)
   ("C-c g b" . magit-blame-mode)
   ("C-c g B" . magit-branch)
   ("C-c g c" . magit-checkout)
   ("C-c g d" . magit-ediff-show-working-tree)
   ("C-c g s" . magit-status)
   ("C-c g S" . magit-stage-file)
   ("C-c g r" . magit-rebase)
   ("C-c g U" . magit-unstage-file)))

(after! git-timemachine
  (bind-keys
   ("C-c g t" . git-timemachine)))

(after! ace-window
  (bind-keys
   ([remap other-window] . ace-window)))

(after! company
  (bind-keys
   ([remap dabbrev-expand] . company-dabbrev)
   :map company-active-map
     ("C-n" . company-select-next)
     ("C-p" . company-select-prev)))

(after! cc-mode
  (bind-keys
   :map c-mode-map
     ("C-c m a" . projectile-find-other-file)
     ("C-c m A" . projectile-find-other-file-other-window)
   :map c++-mode-map
     ("C-c m a" . projectile-find-other-file)
     ("C-c m A" . projectile-find-other-file-other-window)))

(provide 'config-keybindings)
;;; config-keybindings.el ends here

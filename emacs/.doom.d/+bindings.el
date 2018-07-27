;;; +bindings.el -*- lexical-binding: t; -*-

;; This file defines the standard bindings for Emacs

;; TODO: move the common `remap's to the main config file
;; TODO: investigate a hyper key (like AltGr or something)

(map!
 ;; Popups
 "C-`"       #'+popup/toggle
 "C-~"       #'+popup/raise
 "C-x p"     #'+popup/other

 ;; --- Package keybindings --------------------------
 (:map company-active-map
   "C-n"     #'company-select-next
   "C-p"     #'company-select-prev
   "C-h"     #'company-show-doc-buffer
   "C-u"     #'company-previous-page
   "C-d"     #'company-next-page
   "C-s"     #'company-filter-candidates
   "C-SPC"   #'company-complete-common
   "TAB"     #'company-complete-common-or-cycle
   [backtab] #'company-select-previous
   "C-M-s"   #'counsel-company)

 (:map company-search-map
   "C-n"     #'company-select-next-or-abort
   "C-p"     #'company-select-previous-or-abort
   "C-s"     (λ! (company-search-abort) (company-filter-candidates))
   [escape]  #'company-search-abort)

 (:map (ivy-minibuffer-map
        minibuffer-local-map
        minibuffer-local-ns-map
        minibuffer-local-completion-map
        minibuffer-local-must-match-map
        minibuffer-local-isearch-map
        read-expression-map)
   "C-s" #'counsel-minibuffer-history
   "C-a" #'move-beginning-of-line
   "C-n" #'next-line
   "C-p" #'previous-line
   "C-_" (lambda! (ignore-errors (call-interactively #'undo))))

 (:map counsel-ag-map
   [backtab] #'+ivy/wgrep-occur
   "C-l"     #'ivy-call-and-recenter
   "C-RET"   (+ivy-do-action! #'+ivy-git-grep-other-window-action))

 (:map swiper-map
   [backtab] #'+ivy/wgrep-occur)

 (:map smartparens-mode-map
   [remap forward-sexp]   #'sp-forward-sexp
   [remap backward-sexp]  #'sp-backward-sexp
   [remap forward-list]   #'sp-forward-hybrid-sexp
   [remap backward-list]  #'sp-backward-hybrid-sexp
   "C-\""                 #'sp-change-inner)

 (:map yas-keymap
   "C-e"         #'+snippets/goto-end-of-field
   "C-a"         #'+snippets/goto-start-of-field
   [C-backspace] #'+snippets/delete-to-start-of-field
   [backspace]   #'+snippets/delete-backward-char
   [delete]      #'+snippets/delete-forward-char-or-field)
 (:map yas-minor-mode-map
   [C-tab]       #'yas-maybe-expand)


 (:map Man-mode-map
   "q" #'kill-this-buffer)

 (:map comint-mode-map
   [tab] #'company-complete)

 (:map pdf-view-mode-map
   "q" #'kill-this-buffer)

;; --- Leader keybindings --------------------------
 (:desc "leader" :prefix "C-c"
   "b"   #'persp-switch-to-buffer
   ","   #'switch-to-prev-buffer
   "."   #'switch-to-next-buffer
   "RET" #'bookmark-jump

   (:desc "workspace" :prefix [tab]
     :desc "Next workspace"            "]"   #'+workspace/switch-right
     :desc "Previous workspace"        "["   #'+workspace/switch-left
     :desc "New workspace"             "n"   #'+workspace/new
     :desc "Rename workspace"          "r"   #'+workspace/rename
     :desc "Save workspace"            "s"   #'+workspace/save
     :desc "Save session"              "S"   #'+workspace/save-session
     :desc "Load workspace"            "l"   #'+workspace/load
     :desc "Load session"              "L"   #'+workspace/load-session
     :desc "Load last session"         "C-l" (lambda! (+workspace/load-session))
     :desc "Kill all buffers"          "k"   #'doom/kill-all-buffers
     :desc "Delete workspace"          "x"   #'+workspace/delete
     :desc "Kill session"              "X"   #'+workspace/kill-session
     :desc "Kill session and quit"     "C-x" #'+workspace/kill-session-and-quit
     :desc "Switch workspace"          "."   #'+workspace/switch-to
     :desc "Switch to workspace #1"    "1"   #'(λ! (+workspace/switch-to 0))
     :desc "Switch to workspace #2"    "2"   #'(λ! (+workspace/switch-to 1))
     :desc "Switch to workspace #3"    "3"   #'(λ! (+workspace/switch-to 2))
     :desc "Switch to workspace #4"    "4"   #'(λ! (+workspace/switch-to 3))
     :desc "Switch to workspace #5"    "5"   #'(λ! (+workspace/switch-to 4))
     :desc "Switch to workspace #6"    "6"   #'(λ! (+workspace/switch-to 5))
     :desc "Switch to workspace #7"    "7"   #'(λ! (+workspace/switch-to 6))
     :desc "Switch to workspace #8"    "8"   #'(λ! (+workspace/switch-to 7))
     :desc "Switch to workspace #9"    "9"   #'(λ! (+workspace/switch-to 8))
     :desc "Switch to last workspace"  "0" #'+workspace/switch-to-last
     :desc "Display tab bar"           [tab] #'+workspace/display)

   (:desc "code" :prefix "c"
     :desc "List errors"               "x"   #'flycheck-list-errors
     :desc "Build tasks"               "b"   #'+eval/build
     :desc "Evaluate buffer"           "e"   #'+eval/buffer
     :desc "Evaluate region"           "r"   #'+eval/region
     :desc "Jump to definition"        "d"   #'+lookup/definition
     :desc "Jump to references"        "u"   #'+lookup/references
     :desc "Open REPL"                 "o"   #'+eval/open-repl)

   (:desc "file" :prefix "f"
     :desc "Find file from here"       "f"   #'counsel-file-jump
     :desc "Find other file"           "a"   #'projectile-find-other-file
     :desc "Recent project files"      "R"   #'projectile-recentf
     :desc "Find file in doom.d"       "e"   #'+default/find-in-config
     :desc "Find file in emacs.d"      "E"   #'+default/find-in-emacsd
     :desc "Recent files"              "r"   #'counsel-recentf
     :desc "Find project editorconfig" "C"   #'editorconfig-find-current-editorconfig
     :desc "Yank this filename"        "y"   #'+default/yank-buffer-filename
     :desc "Delete this file"          "X"   #'doom/delete-this-file
     (:when (not IS-WINDOWS)
       :desc "Sudo find file"          "s"   #'doom/sudo-find-file
       :desc "Sudo edit this file"     "S"   #'doom/sudo-this-file))

   (:desc "git" :prefix "g"
     :desc "Blame"                     "b"   #'magit-blame
     :desc "Commit"                    "c"   #'magit-commit
     :desc "Clone"                     "C"   #'+magit/clone
     :desc "Dispatch"                  "d"   #'magit-dispatch-popup
     :desc "Find file"                 "f"   #'magit-find-file
     :desc "Status"                    "g"   #'magit-status
     :desc "Buffer log"                "l"   #'magit-log-buffer-file
     :desc "Push"                      "p"   #'magit-push-popup
     :desc "Pull"                      "P"   #'magit-pull-popup
     :desc "Revert hunk"               "r"   #'git-gutter:revert-hunk
     :desc "Revert file"               "R"   #'vc-revert
     :desc "Stage hunk"                "s"   #'git-gutter:stage-hunk
     :desc "Stage file"                "S"   #'magit-stage-file
     :desc "Time machine"              "t"   #'git-timemachine-toggle
     :desc "Unstage file"              "U"   #'magit-unstage-file
     :desc "List repos"                "L"   #'magit-list-repositories
     :desc "Delete file"               "X"   #'magit-file-delete
     :desc "Initialize repo"           "I"   #'magit-init
     :desc "Next hunk"                 "]"   #'git-gutter:next-hunk
     :desc "Previous hunk"             "["   #'git-gutter:previous-hunk)

   (:desc "help" :prefix "h"
     :desc "Apropos"                   "a"   #'apropos
     :desc "Open Bug Report"           "b"   #'doom/open-bug-report
     :desc "Describe char"             "c"   #'describe-char
     :desc "Describe Doom module"      "d"   #'doom/describe-module
     :desc "Open vanilla sandbox"      "e"   #'doom/open-vanilla-sandbox
     :desc "Print Emacs version"       "E"   #'emacs-verson
     :desc "Describe function"         "f"   #'describe-function
     :desc "Describe face"             "F"   #'describe-face
     :desc "Info"                      "i"   #'info-lookup-symbol
     :desc "Describe key"              "k"   #'describe-key
     :desc "Find documentation"        "K"   #'+lookup/documentation
     :desc "Find library"              "l"   #'find-library
     :desc "Command log"               "L"   #'global-command-log-mode
     :desc "Describe mode"             "m"   #'describe-mode
     :desc "View *Messages*"           "M"   #'view-echo-area-messages
     :desc "Toggle profiler"           "p"   #'doom/toggle-profiler
     :desc "Describe variable"         "v"   #'describe-variable
     :desc "Print Doom version"        "V"   #'doom/version
     :desc "Man pages"                 "w"   #'+default/man-or-woman
     :desc "Describe at point"         "."   #'helpful-at-point
     :desc "What minor modes"          ","   #'doom/describe-active-minor-mode
     :desc "What face"                 ";"   #'doom/what-face)

   (:desc "insert" :prefix "i"
     :desc "From kill-ring"            "y"   #'counsel-yank-pop
     :desc "From snippet"              "s"   #'yas-insert-snippet)

   (:desc "notes" :prefix "n"
     :desc "Find file in notes"        "n"   #'+default/find-in-notes
     :desc "Org capture"               "x"   #'org-capture)

   (:desc "open" :prefix "o"
     :desc "Default browser"           "b"   #'browse-url
     :desc "Debugger"                  "d"   #'+debug/open
     :desc "Dired"                     "-"   #'dired-jump
     :desc "Terminal"                  "t"   #'+term/open
     :desc "Terminal in popup"         "T"   #'+term/open-popup-in-project
     :desc "Eshell"                    "e"   #'+eshell/open
     :desc "Eshell in popup"           "E"   #'+eshell/open-popup)

   (:desc "project" :prefix "p"
     :desc "Browse project"            "."   #'+default/browse-project
     :desc "Find file in project"      "f"   #'projectile-find-file
     :desc "Run command in project"    "!"   #'projectile-run-shell-command-in-root
     :desc "Compile project"           "c"   #'projectile-compile-project
     :desc "Find other file"           "a"   #'projectile-find-other-file
     :desc "Switch project"            "p"   #'projectile-switch-project
     :desc "Recent files in project"   "r"  #'projectile-recentf
     :desc "List project tasks"        "t"   #'+ivy/tasks
     :desc "Invalidate project cache"  "x" #'projectile-invalidate-cache)

   (:desc "popups" :prefix "q"
     :desc "Cycle next popup"          [tab] #'+popup/other
     :desc "Close popup"               "x"   #'+popup/close-popup
     :desc "Close all popups"          "X"   #'+popup/close-all
     :desc "Cleanup popup rules"       "R"   #'+popup/cleanup-rules
     :desc "Restore last popup"        "w"   #'+popup/restore
     :desc "Raise popup"               "p"   #'+popup/raise)

   (:desc "snippets" :prefix "s"
     :desc "New snippet"               "n"   #'yas-new-snippet
     :desc "Insert snippet"            "i"   #'yas-insert-snippet
     :desc "Find in snippets"          "s"   #'+default/find-in-snippets
     :desc "Browse snippets"           "S"   #'+default/browse-snippets
     :desc "Find snippet for mode"     "/"   #'yas-visit-snippet-file)

   (:desc "toggle" :prefix "t"
     :desc "Flyspell"                  "s"   #'flyspell-mode
     :desc "Flycheck"                  "f"   #'flycheck-mode
     :desc "Line numbers"              "l"   #'doom/toggle-line-numbers
     :desc "Fullscreen"                "F"   #'toggle-frame-fullscreen
     :desc "Indent guides"             "i"   #'highlight-indentation-mode
     :desc "Indent guides (column)"    "I"   #'highlight-indentation-current-column-mode
     :desc "Big mode"                  "b"   #'doom-big-font-mode
     :desc "Org present"               "p"   #'+org-present/start)

   (:desc "quit" :prefix "x"
     :desc "Quit Emacs"                "x"   #'kill-emacs
     :desc "Quit Emacs & session"      "X"   #'+workspace/kill-session-and-quit
     :desc "Restart & restore Emacs"   "r"   #'+workspace/restart-emacs-then-restore
     :desc "Restart Emacs"             "R"   #'restart-emacs)

   ;;(:desc "vc" :prefix "v")
   (:desc "complete" :prefix [C-tab]
     "C-l"   #'+company/whole-lines
     "C-k"   #'+company/dict-or-keywords
     "C-f"   #'company-files
     "C-d"   #'company-etags
     "C-s"   #'company-ispell
     "C-t"   #'company-yasnippet
     "C-o"   #'company-capf
     "C-n"   #'+company/dabbrev
     "C-p"   #'+company/dabbrev-code-previous)

   (:desc "search" :prefix "/"
     :desc "Buffer"                   "b"  #'swiper
     :desc "Project"                  "p"  #'+ivy/project-search
     :desc "Directory"                "d"  (lambda! (+ivy/project-search t))
     :desc "Symbols"                  "i"  #'imenu
     :desc "Symbols across buffers"   "I"  #'imenu-anywhere)

   (:desc "previous..." :prefix "["
     :desc "Text size"                "]"   #'text-scale-decrease
     :desc "Buffer"                   "b"   #'previos-buffer
     :desc "Diff hunk"                "d"   #'git-gutter:previous-hunk
     :desc "Todo"                     "t"   #'hl-todo-previous
     :desc "Error"                    "e"   #'previous-error)

   (:desc "next..." :prefix "]"
     :desc "Text size"                "]"   #'text-scale-increase
     :desc "Buffer"                   "b"   #'next-buffer
     :desc "Diff hunk"                "d"   #'git-gutter:next-hunk
     :desc "Todo"                     "t"   #'hl-todo-next
     :desc "Error"                    "e"   #'next-error
     :desc "Spelling error"           "s"   #'flyspell-goto-next-error)))

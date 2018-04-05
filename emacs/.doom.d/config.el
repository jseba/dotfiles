;;; private/jseba/config.el  -*- lexical-binding: t; -*-

(pcase system-type
  ('windows-nt)
  ('darwin
   (setq ns-alternate-modifier 'meta
         ns-command-modifier 'super)))

(setq evil-escape-key-sequence "kj"
      expand-region-contract-fast-key "V"
      doom-font (font-spec :family "Hack" :size 12)
      doom-big-font (font-spec :family "Hack" :size 16))

(map!
 ;; --- Remap to smarter functions ---------------------------------------------------------
 [remap electric-newline-and-indent] #'nx/smart-newline-and-indent
 [remap kill-whole-line]             #'nx/smart-kill-whole-line
 [remap move-beginning-of-line]      #'nx/smart-move-beginning-of-line
 [remap move-end-of-line]            #'nx/smart-move-end-of-line
 [remap back-to-indentation]         #'nx/smart-back-to-indentation
 [remap backward-sexp]               #'nx/smart-backward-sexp
 [remap backtab]                     #'nx/dumb-dedent
 [remap split-window-vertically]     #'nx/split-window-vertically-with-other-buffer
 [remap split-window-horizontally]   #'nx/split-window-horizontally-with-other-buffer
 [remap delete-other-windows]        #'nx/toggle-delete-other-windows

 ;; --- Global keybindings -----------------------------------------------------------------
 :gnvmie "M-x"                       #'execute-extended-command
 :gnvmie "M-;"                       #'eval-expression
 :gnvmie "M-:"                       #'doom/open-scratch-buffer

 :ne     "M-+"                       #'text-scale-increase
 :ne     "M--"                       #'text-scale-decrease
 :ne     "M-="                       (lambda! (text-scale-set 0))
 :ne     "C-`"                       #'+popup/toggle
 :ne     "C-~"                       #'+popup/raise
 :ne     "M-t"                       #'+workspace/new
 :ne     "M-T"                       #'+workspace/display
 :ne     "M-w"                       #'delete-window
 :ne     "M-W"                       #'delete-frame
 :ne     "M-T"                       #'+workspace/display
 :ne     "C-M-f"                     #'toggle-frame-fullscreen
 :ne     "M-n"                       #'evil-buffer-new
 :ne     "M-N"                       #'make-frame
 :ne     "M-1"                       (lambda! (+workspace/switch-to 0))
 :ne     "M-2"                       (lambda! (+workspace/switch-to 1))
 :ne     "M-3"                       (lambda! (+workspace/switch-to 2))
 :ne     "M-4"                       (lambda! (+workspace/switch-to 3))
 :ne     "M-5"                       (lambda! (+workspace/switch-to 4))
 :ne     "M-6"                       (lambda! (+workspace/switch-to 5))
 :ne     "M-7"                       (lambda! (+workspace/switch-to 6))
 :ne     "M-8"                       (lambda! (+workspace/switch-to 7))
 :ne     "M-9"                       (lambda! (+workspace/switch-to 8))
 :ne     "M-0"                       #'+workspace/switch-to-last

 :ne     "M-r"                       #'+eval/buffer
 :ne     "M-R"                       #'+eval/region-and-replace
 :ne     "M-b"                       #'+default/compile
 :ne     "M-a"                       #'mark-whole-buffer
 :ne     "M-c"                       #'evil-yank
 :ne     "M-q"                       (if (daemonp) #'delete-frame #'evil-quit-all)
 :ne     "M-f"                       #'swiper
 :n      "M-s"                       #'save-buffer
 :nv     "C-SPC"                     #'+evil:fold-toggle
 :gnvmie "M-v"                       #'clipboard-yank

 :ne     "C-h"                       #'evil-window-left
 :ne     "C-j"                       #'evil-window-down
 :ne     "C-k"                       #'evil-window-up
 :ne     "C-l"                       #'evil-window-right

 ;; --- Leader -----------------------------------------------------------------------------
 (:leader
   :desc "Ex command"                   :nv     ";"     #'evil-ex
   :desc "M-x"                          :nv     ":"     #'execute-extended-command
   :desc "Pop-up scratch buffer"        :nv     "x"     #'doom/open-scratch-buffer
   :desc "Universal argument"           :n      "u"     #'universal-argument
   :desc "Eval expression"              :n      "`"     #'eval-expression

   (:desc "previous..." :prefix "["
     :desc "Text size"                 :nv      "["     #'text-scale-decrease
     :desc "Buffer"                    :nv      "b"     #'previous-buffer
     :desc "Diff Hunk"                 :nv      "d"     #'git-gutter:previous-hunk
     :desc "Todo"                      :nv      "t"     #'hl-todo-previous
     :desc "Error"                     :nv      "e"     #'previous-error
     :desc "Workspace"                 :nv      "w"     #'+workspace/switch-left
     :desc "Smart Jump"                :nv      "j"     #'smart-backward
     :desc "Spelling Error"            :nv      "s"     #'evil-prev-flyspell-error
     :desc "Spelling Correction"       :n       "S"     #'flyspell-correct-previous-word-generic)

   (:desc "next..." :prefix "]"
     :desc "Text size"                 :nv      "]"     #'text-scale-increase
     :desc "Buffer"                    :nv      "b"     #'next-buffer
     :desc "Diff Hunk"                 :nv      "d"     #'git-gutter:next-hunk
     :desc "Todo"                      :nv      "t"     #'hl-todo-next
     :desc "Error"                     :nv      "e"     #'next-error
     :desc "Workspace"                 :nv      "w"     #'+workspace/switch-right
     :desc "Smart Jump"                :nv      "j"     #'smart-forward
     :desc "Spelling Error"            :nv      "s"     #'evil-prev-flyspell-error
     :desc "Spelling Correction"       :n       "S"     #'flyspell-correct-word-generic)

   (:desc "search" :prefix "/"
     :desc "Project"                   :nv      "p"     #'+ivy/project-search
     :desc "Directory"                 :nv      "d"     (lambda! (+ivy/project-search t))
     :desc "Buffer"                    :nv      "b"     #'swiper
     :desc "Symbols"                   :nv      "i"     #'imenu
     :desc "Symbols (all buffers)"     :nv      "I"     #'imenu-anywhere
     :desc "Online"                    :nv      "o"     #'+lookup/online-select)

   (:desc "workspace" :prefix "TAB"
     :desc "Display tab bar"           :n       "TAB"   #'+workspace/display
     :desc "New workspace"             :n       "n"     #'+workspace/new
     :desc "Load workspace"            :n       "l"     #'+workspace/load
     :desc "Load last session"         :n       "L"     (lambda! (+workspace/load-session))
     :desc "Save workspace"            :n       "s"     #'+workspace/save
     :desc "Autosave session"          :n       "S"     #'+workspace/save-session
     :desc "Switch workspace"          :n       "."     #'+workspace/switch-to
     :desc "Kill all buffers"          :n       "x"     #'doom/kill-all-buffers
     :desc "Delete session"            :n       "X"     #'+workspace/kill-session
     :desc "Delete workspace"          :n       "d"     #'+workspace/delete
     :desc "Next workspace"            :n       "n"     #'+workspace/switch-right
     :desc "Previous workspace"        :n       "p"     #'+workspace/switch-left
     :desc "Switch to workspace 1"     :n       "1"     (lambda! (+workspace/switch-to 0))
     :desc "Switch to workspace 2"     :n       "2"     (lambda! (+workspace/switch-to 1))
     :desc "Switch to workspace 3"     :n       "3"     (lambda! (+workspace/switch-to 2))
     :desc "Switch to workspace 4"     :n       "4"     (lambda! (+workspace/switch-to 3))
     :desc "Switch to workspace 5"     :n       "5"     (lambda! (+workspace/switch-to 4))
     :desc "Switch to workspace 6"     :n       "6"     (lambda! (+workspace/switch-to 5))
     :desc "Switch to workspace 7"     :n       "7"     (lambda! (+workspace/switch-to 6))
     :desc "Switch to workspace 8"     :n       "8"     (lambda! (+workspace/switch-to 7))
     :desc "Switch to workspace 9"     :n       "9"     (lambda! (+workspace/switch-to 8))
     :desc "Switch to last workspace"  :n       "0"     #'+workspace/switch-to-last)

   (:desc "buffer" :prefix "b"
     :desc "New empty buffer"          :n       "n"     #'evil-buffer-new
     :desc "Switch workspace buffer"   :n       "B"     #'persp-switch-to-buffer
     :desc "Switch buffer"             :n       "b"     #'switch-to-buffer
     :desc "Kill buffer"               :n       "k"     #'doom/kill-this-buffer
     :desc "Kill other buffers"        :n       "K"     #'doom/kill-other-buffers
     :desc "Save buffer"               :n       "s"     #'save-buffer
     :desc "Bury buffer"               :n       "z"     #'bury-buffer
     :desc "Next buffer"               :n       "]"     #'next-buffer
     :desc "Previous buffer"           :n       "["     #'previous-buffer)

   (:desc "code" :prefix "c"
     :desc "List errors"               :n       "x"     #'flycheck-list-errors
     :desc "Evaluate buffer"           :n       "e"     #'+eval/buffer
     :desc "Evaluate region"           :v       "e"     #'+eval/region
     :desc "Evaluate & replace region" :nv      "E"     #'+eval:replace-region
     :desc "Build tasks"               :nv      "b"     #'+eval/build
     :desc "Jump to definition"        :n       "d"     #'+lookup/definition
     :desc "Jump to references"        :n       "D"     #'+lookup/references
     :desc "Open REPL"                 :n       "r"     #'+eval/open-repl
                                       :v       "r"     #'+eval:repl)

   (:desc "files" :prefix "f"
     :desc "Find file"                 :n       "."     #'find-file
     :desc "Find file in project"      :n       "/"     #'projectile-find-file
     :desc "Find file from here"       :n       "?"     #'counsel-file-jump
     :desc "Find other file"           :n       "a"     #'projectile-find-other-file
     :desc "Open project editorconfig" :n       "C"     #'editorconfig-find-current-editorconfig
     :desc "Find directory"            :n       "d"     #'dired
     :desc "Find file in emacs.d"      :n       "e"     #'+default/find-in-emacsd
     :desc "Browse emacs.d"            :n       "E"     #'+default/browse-emacsd
     :desc "Recent files"              :n       "r"     #'recentf-open-files
     :desc "Recent project files"      :n       "R"     #'projectile-recentf
     :desc "Yank filename"             :n       "y"     #'+default/yank-buffer-filename
     (:when (featurep! :config private)
       :desc "Find file in doom.d"     :n       "p"     #'+private/find-in-config
       :desc "Browse doom.d"           :n       "P"     #'+private/browse-config))

   (:desc "git" :prefix "g"
     :desc "Git status"            :n  "S" #'magit-status
     :desc "Git blame"             :n  "b" #'magit-blame
     :desc "Git time machine"      :n  "t" #'git-timemachine-toggle
     :desc "Git stage hunk"        :n  "s" #'git-gutter:stage-hunk
     :desc "Git revert hunk"       :n  "r" #'git-gutter:revert-hunk
     :desc "Git revert buffer"     :n  "R" #'vc-revert
     :desc "List gists"            :n  "g" #'+gist:list
     :desc "Next hunk"             :nv "]" #'git-gutter:next-hunk
     :desc "Previous hunk"         :nv "[" #'git-gutter:previous-hunk)

   (:desc "help" :prefix "h"
     :n "h" help-map
     :desc "Apropos"               :n  "a" #'apropos
     :desc "Describe char"         :n  "c" #'describe-char
     :desc "Describe DOOM module"  :n  "d" #'doom/describe-module
     :desc "Open Doom manual"      :n  "D" #'doom/help
     :desc "Describe function"     :n  "f" #'describe-function
     :desc "Describe face"         :n  "F" #'describe-face
     :desc "Info"                  :n  "i" #'info-lookup-symbol
     :desc "Describe key"          :n  "k" #'describe-key
     :desc "Find documentation"    :n  "K" #'+lookup/documentation
     :desc "Find library"          :n  "l" #'find-library
     :desc "Command log"           :n  "L" #'global-command-log-mode
     :desc "Toggle Emacs log"      :n  "m" #'view-echo-area-messages
     :desc "Describe mode"         :n  "M" #'describe-mode
     :desc "Toggle profiler"       :n  "p" #'doom/toggle-profiler
     :desc "Reload theme"          :n  "R" #'doom//reload-theme
     :desc "Describe DOOM setting" :n  "s" #'doom/describe-setting
     :desc "Describe variable"     :n  "v" #'describe-variable
     :desc "Describe at point"     :n  "." #'helpful-at-point
     :desc "What face"             :n  "'" #'doom/what-face
     :desc "What minor modes"      :n  ";" #'doom/what-minor-mode)

   (:desc "insert" :prefix "i"
     :desc "From kill-ring"        :nv "y" #'counsel-yank-pop
     :desc "From evil registers"   :nv "r" #'counsel-evil-registers
     :desc "From snippet"          :nv "s" #'yas-insert-snippet)

   (:desc "notes" :prefix "n"
     :desc "Find file in notes"    :n  "n" #'+default/find-in-notes
     :desc "Browse notes"          :n  "N" #'+default/browse-notes
     :desc "Org capture"           :n  "x" #'org-capture
     :desc "Browse mode notes"     :n  "m" #'+org/browse-notes-for-major-mode
     :desc "Browse project notes"  :n  "p" #'+org/browse-notes-for-project)

   (:desc "open" :prefix "o"
     :desc "Default browser"       :n  "b" #'browse-url-of-file
     :desc "Debugger"              :n  "d" #'+debug/open
     :desc "REPL"                  :n  "r" #'+eval/open-repl
     :v  "r" #'+eval:repl
     :desc "Neotree"               :n  "n" #'+neotree/open
     :desc "Neotree: on this file" :n  "N" #'+neotree/find-this-file
     :desc "Imenu sidebar"         :nv "i" #'imenu-list-minor-mode
     :desc "Terminal"              :n  "t" #'+term/open-popup
     :desc "Terminal in project"   :n  "T" #'+term/open-popup-in-project

     ;; applications
     :desc "APP: elfeed"           :n "E" #'=rss
     :desc "APP: email"            :n "M" #'=email
     :desc "APP: twitter"          :n "T" #'=twitter
     :desc "APP: regex"            :n "X" #'=regex

     ;; macos
     (:when IS-MAC
       :desc "Reveal in Finder"          :n "o" #'+macos/reveal-in-finder
       :desc "Reveal project in Finder"  :n "O" #'+macos/reveal-project-in-finder
       :desc "Send to Transmit"          :n "u" #'+macos/send-to-transmit
       :desc "Send project to Transmit"  :n "U" #'+macos/send-project-to-transmit
       :desc "Send to Launchbar"         :n "l" #'+macos/send-to-launchbar
       :desc "Send project to Launchbar" :n "L" #'+macos/send-project-to-launchbar))

   (:desc "project" :prefix "p"
     :desc "Browse project"          :n  "." #'+default/browse-project
     :desc "Find file in project"    :n  "/" #'projectile-find-file
     :desc "Run cmd in project root" :nv "!" #'projectile-run-shell-command-in-root
     :desc "Compile project"         :n  "c" #'projectile-compile-project
     :desc "Find other file"         :n  "o" #'projectile-find-other-file
     :desc "Switch project"          :n  "p" #'projectile-switch-project
     :desc "Recent project files"    :n  "r" #'projectile-recentf
     :desc "List project tasks"      :n  "t" #'+ivy/tasks
     :desc "Invalidate cache"        :n  "x" #'projectile-invalidate-cache)

   (:desc "quit" :prefix "q"
     :desc "Save and quit"          :n "q" #'evil-save-and-quit
     :desc "Quit (forget session)"  :n "Q" #'+workspace/kill-session-and-quit)

   (:desc "snippets" :prefix "s"
     :desc "New snippet"            :n  "n" #'yas-new-snippet
     :desc "Insert snippet"         :nv "i" #'yas-insert-snippet
     :desc "Find snippet for mode"  :n  "s" #'yas-visit-snippet-file
     :desc "Find snippet"           :n  "S" #'+default/find-in-snippets)

   (:desc "toggle" :prefix "t"
     :desc "Flyspell"               :n "s" #'flyspell-mode
     :desc "Flycheck"               :n "f" #'flycheck-mode
     :desc "Line numbers"           :n "l" #'doom/toggle-line-numbers
     :desc "Frame fullscreen"       :n "F" #'toggle-frame-fullscreen
     :desc "Indent guides"          :n "i" #'highlight-indentation-mode
     :desc "Indent guides (column)" :n "I" #'highlight-indentation-current-column-mode
     :desc "Impatient mode"         :n "h" #'+impatient-mode/toggle
     :desc "Big mode"               :n "b" #'doom-big-font-mode
     :desc "Evil goggles"           :n "g" #'+evil-goggles/toggle
     :desc "org-tree-slide mode"    :n "p" #'+org-present/start))

 ;; --- Personal vim-esque bindings ------------------
 :nv "K"  #'+lookup/documentation
 :n  "zx" #'kill-this-buffer
 :n  "ZX" #'bury-buffer
 :n  "]b" #'next-buffer
 :n  "[b" #'previous-buffer
 :n  "]w" #'+workspace/switch-right
 :n  "[w" #'+workspace/switch-left
 :m  "gt" #'+workspace/switch-right
 :m  "gT" #'+workspace/switch-left
 :m  "gd" #'+lookup/definition
 :m  "gD" #'+lookup/references
 :n  "gp" #'+evil/reselect-paste
 :n  "gr" #'+eval:region
 :n  "gR" #'+eval/buffer
 :v  "gR" #'+eval:replace-region
 :m  "gs" #'+default/easymotion  ; lazy-load `evil-easymotion'
 :v  "@"  #'+evil:apply-macro
 :n  "g@" #'+evil:apply-macro

 ;; repeat in visual mode (FIXME buggy)
 :v  "."  #'evil-repeat

 ;; don't leave visual mode after shifting
 :v  "<"  #'+evil/visual-dedent  ; vnoremap < <gv
 :v  ">"  #'+evil/visual-indent  ; vnoremap > >gv

 :nv "C-a"   #'evil-numbers/inc-at-pt
 :nv "C-S-a" #'evil-numbers/dec-at-pt

 ;; --- Plugin bindings ------------------------------

 ;; auto-yasnippet
 :i  [C-tab] #'aya-expand
 :nv [C-tab] #'aya-create

 ;; company-mode (vim-like omnicompletion)
 [remap dabbrev-expand] #'+company/complete
 :i "C-@"    #'+company/complete
 :i "C-SPC"  #'+company/complete
 (:prefix "C-x"
   :i "C-l"   #'+company/whole-lines
   :i "C-k"   #'+company/dict-or-keywords
   :i "C-f"   #'company-files
   :i "C-]"   #'company-etags
   :i "s"     #'company-ispell
   :i "C-s"   #'company-yasnippet
   :i "C-o"   #'company-capf
   :i "C-n"   #'company-dabbrev-code
   :i "C-p"   #'+company/dabbrev-code-previous)

 (:after company
   (:map company-active-map
     ;; Don't interfere with `evil-delete-backward-word' in insert mode
     "C-w"     nil
     "C-o"     #'company-search-kill-others
     "C-n"     #'company-select-next
     "C-p"     #'company-select-previous
     "C-h"     #'company-quickhelp-manual-begin
     "C-S-h"   #'company-show-doc-buffer
     "C-S-s"   #'company-search-candidates
     "C-s"     #'company-filter-candidates
     "C-SPC"   #'company-complete-common
     "C-h"     #'company-quickhelp-manual-begin
     "TAB"     #'company-complete-common-or-cycle
     [tab]     #'company-complete-common-or-cycle
     "S-TAB"   #'company-select-previous
     [backtab] #'company-select-previous)

   ;; Automatically applies to `company-filter-map'
   (:map company-search-map
     "C-n"     #'company-select-next-or-abort
     "C-p"     #'company-select-previous-or-abort
     "C-s"     (lambda! (company-search-abort) (company-filter-candidates))
     [escape]  #'company-search-abort))

 ;; counsel
 (:after counsel
   (:map counsel-ag-map
     [backtab]  #'+ivy/wgrep-occur      ; search/replace on results
     "C-SPC"    #'ivy-call-and-recenter ; preview
     "M-RET"    (+ivy-do-action! #'+ivy-git-grep-other-window-action)))

 ;; evil
 (:after evil
   :textobj "a" #'evil-inner-arg                    #'evil-outer-arg
   :textobj "B" #'evil-textobj-anyblock-inner-block #'evil-textobj-anyblock-a-block
   :textobj "i" #'evil-indent-plus-i-indent         #'evil-indent-plus-a-indent
   :textobj "I" #'evil-indent-plus-i-indent-up      #'evil-indent-plus-a-indent-up
   :textobj "J" #'evil-indent-plus-i-indent-up-down #'evil-indent-plus-a-indent-up-down

   (:map evil-window-map ; prefix "C-w"
     ;; Navigation
     "C-h"     #'evil-window-left
     "C-j"     #'evil-window-down
     "C-k"     #'evil-window-up
     "C-l"     #'evil-window-right
     "C-w"     #'other-window

     ;; Swapping windows
     "H"       #'+evil/window-move-left
     "J"       #'+evil/window-move-down
     "K"       #'+evil/window-move-up
     "L"       #'+evil/window-move-right
     "C-S-w"   #'ace-swap-window

     ;; Window undo/redo
     "u"       #'winner-undo
     "C-u"     #'winner-undo
     "C-r"     #'winner-redo
     "o"       #'doom/window-enlargen

     ;; Delete window
     "c"       #'+workspace/close-window-or-workspace
     "C-C"     #'ace-delete-window))

 ;; evil-commentary
 :n  "gc"  #'evil-commentary

 ;; evil-exchange
 :n  "gx"  #'evil-exchange

 ;; evil-matchit
 :nv [tab] #'+evil/matchit-or-toggle-fold

 ;; evil-magit
 (:after evil-magit
   :map (magit-status-mode-map magit-revision-mode-map)
   :n "C-j" nil
   :n "C-k" nil)

 ;; evil-mc
 (:prefix "gz"
   :nv "m" #'evil-mc-make-all-cursors
   :nv "u" #'evil-mc-undo-all-cursors
   :nv "z" #'+evil/mc-make-cursor-here
   :nv "t" #'+evil/mc-toggle-cursors
   :nv "n" #'evil-mc-make-and-goto-next-cursor
   :nv "p" #'evil-mc-make-and-goto-prev-cursor
   :nv "N" #'evil-mc-make-and-goto-last-cursor
   :nv "P" #'evil-mc-make-and-goto-first-cursor
   :nv "d" #'evil-mc-make-and-goto-next-match
   :nv "D" #'evil-mc-make-and-goto-prev-match)

 (:after evil-mc
   :map evil-mc-key-map
   :nv "C-n" #'evil-mc-make-and-goto-next-cursor
   :nv "C-N" #'evil-mc-make-and-goto-last-cursor
   :nv "C-p" #'evil-mc-make-and-goto-prev-cursor
   :nv "C-P" #'evil-mc-make-and-goto-first-cursor)

 ;; evil-multiedit
 :v  "R"     #'evil-multiedit-match-all
 :n  "M-d"   #'evil-multiedit-match-symbol-and-next
 :n  "M-D"   #'evil-multiedit-match-symbol-and-prev
 :v  "M-d"   #'evil-multiedit-match-and-next
 :v  "M-D"   #'evil-multiedit-match-and-prev
 :nv "C-M-d" #'evil-multiedit-restore

 (:after evil-multiedit
   (:map evil-multiedit-state-map
     "M-d" #'evil-multiedit-match-and-next
     "M-D" #'evil-multiedit-match-and-prev
     "RET" #'evil-multiedit-toggle-or-restrict-region)
   (:map (evil-multiedit-state-map evil-multiedit-insert-state-map)
     "C-n" #'evil-multiedit-next
     "C-p" #'evil-multiedit-prev))

 ;; evil-snipe
 (:after evil-snipe
   :map evil-snipe-parent-transient-map

   ;; switch to evil-easymotion/avy after a snipe
   "C-;" (lambda! (require 'evil-easymotion)
             (call-interactively
              (evilem-create #'evil-snipe-repeat
                             :bind ((evil-snipe-scope 'whole-buffer)
                                    (evil-snipe-enable-highlight)
                                    (evil-snipe-enable-incremental-highlight))))))

 ;; evil-surround
 :v  "S"  #'evil-surround-region
 :o  "s"  #'evil-surround-edit
 :o  "S"  #'evil-Surround-edit

 ;; flycheck
 :m  "]e" #'next-error
 :m  "[e" #'previous-error
 (:after flycheck
   :map flycheck-error-list-mode-map
   :n "C-n" #'flycheck-error-list-next-error
   :n "C-p" #'flycheck-error-list-previous-error
   :n "j"   #'flycheck-error-list-next-error
   :n "k"   #'flycheck-error-list-previous-error
   :n "RET" #'flycheck-error-list-goto-error)

 ;; flyspell
 :m  "]S" #'flyspell-correct-word-generic
 :m  "[S" #'flyspell-correct-previous-word-generic
 (:after flyspell
   ;; Press RET on misspelled words to correct them
   (:map flyspell-mouse-map
     "RET" #'flyspell-correct-word-generic
     "<mouse-1>" #'flyspell-correct-word-generic))

 ;; git-gutter
 :m  "]d" #'git-gutter:next-hunk
 :m  "[d" #'git-gutter:previous-hunk

 ;; git-timemachine
 (:after git-timemachine
   (:map git-timemachine-mode-map
     :n "C-p" #'git-timemachine-show-previous-revision
     :n "C-n" #'git-timemachine-show-next-revision
     :n "[["  #'git-timemachine-show-previous-revision
     :n "]]"  #'git-timemachine-show-next-revision
     :n "q"   #'git-timemachine-quit
     :n "gb"  #'git-timemachine-blame))

 ;; hl-todo
 :m  "]t" #'hl-todo-next
 :m  "[t" #'hl-todo-previous

 ;; ivy
 (:after ivy
   :map ivy-minibuffer-map
   [escape] #'keyboard-escape-quit
   "C-SPC"  #'ivy-call-and-recenter
   "M-z"    #'undo
   "M-v"    #'yank
   "C-v"    #'yank
   "C-r"    #'evil-paste-from-register
   "C-k"    #'ivy-previous-line
   "C-j"    #'ivy-next-line
   "C-A-k"  #'ivy-scroll-down-command
   "C-A-j"  #'ivy-scroll-up-command
   "C-l"    #'ivy-alt-done
   "C-w"    #'ivy-backward-kill-word
   "C-u"    #'ivy-kill-line
   "C-b"    #'backward-word
   "C-f"    #'forward-word)

 ;; neotree
 (:after neotree
   :map neotree-mode-map
   :n "g"         nil
   :n [tab]       #'neotree-quick-look
   :n "RET"       #'neotree-enter
   :n [backspace] #'evil-window-prev
   :n "c"         #'neotree-create-node
   :n "r"         #'neotree-rename-node
   :n "d"         #'neotree-delete-node
   :n "j"         #'neotree-next-line
   :n "k"         #'neotree-previous-line
   :n "n"         #'neotree-next-line
   :n "p"         #'neotree-previous-line
   :n "h"         #'+neotree/collapse-or-up
   :n "l"         #'+neotree/expand-or-open
   :n "J"         #'neotree-select-next-sibling-node
   :n "K"         #'neotree-select-previous-sibling-node
   :n "H"         #'neotree-select-up-node
   :n "L"         #'neotree-select-down-node
   :n "G"         #'evil-goto-line
   :n "gg"        #'evil-goto-first-line
   :n "v"         #'neotree-enter-vertical-split
   :n "s"         #'neotree-enter-horizontal-split
   :n "q"         #'neotree-hide
   :n "R"         #'neotree-refresh)

 ;; realgud
 (:after realgud
   :map realgud:shortkey-mode-map
   :n "j" #'evil-next-line
   :n "k" #'evil-previous-line
   :n "h" #'evil-backward-char
   :n "l" #'evil-forward-char
   :m "n" #'realgud:cmd-next
   :m "b" #'realgud:cmd-break
   :m "B" #'realgud:cmd-clear
   :n "c" #'realgud:cmd-continue)

 ;; rotate-text
 :n  "!"  #'rotate-text

 ;; smart-forward
 :m  "g]" #'smart-forward
 :m  "g[" #'smart-backward

 ;; swiper
 (:after swiper
   (:map swiper-map
     [backtab]  #'+ivy/wgrep-occur))

 ;; undo-tree -- undo/redo for visual regions
 :v "C-u" #'undo-tree-undo
 :v "C-r" #'undo-tree-redo

 ;; yasnippet
 (:after yasnippet
   (:map yas-keymap
     "C-e"           #'+snippets/goto-end-of-field
     "C-a"           #'+snippets/goto-start-of-field
     "<M-right>"     #'+snippets/goto-end-of-field
     "<M-left>"      #'+snippets/goto-start-of-field
     "<M-backspace>" #'+snippets/delete-to-start-of-field
     [backspace]     #'+snippets/delete-backward-char
     [delete]        #'+snippets/delete-forward-char-or-field)
   (:map yas-minor-mode-map
     :ig "<tab>" yas-maybe-expand
     :v  "<tab>" #'yas-insert-snippet))

 ;; --- Major mode bindings --------------------------
 (:after markdown-mode
   (:map markdown-mode-map
     ;; fix conflicts with private bindings
     "<backspace>" nil
     "<M-left>"    nil
     "<M-right>"   nil))

 ;; --- Built-in plugins -----------------------------
 (:after comint
   ;; TAB auto-completion in term buffers
   :map comint-mode-map [tab] #'company-complete)

 (:map* (help-mode-map helpful-mode-map)
   :n "o"  #'ace-link-help
   :n "Q"  #'ivy-resume
   :n "]l" #'forward-button
   :n "[l" #'backward-button)

 (:after vc-annotate
   :map vc-annotate-mode-map
   [remap quit-window] #'kill-this-buffer))

(def-package! cc-mode
  :commands (c-mode c++-mode)
  :init
  (setq-default c-basic-offset tab-width
                c-backspace-function #'delete-backward-char
                c-default-style "nx"
                c-toggle-hungry-state 1)
  :config
  (set! :electric '(c-mode c++-mode)
        :chars '(?\n ?\}))

  (c-toggle-electric-state -1)
  (c-toggle-auto-newline -1)

  ;; personal style (based off Stroustrup)
  (map-put c-style-alist "nx"
           `((c-basic-offset . ,tab-width)
             (c-comment-only-line-offset . 0)
             (c-hanging-braces-alist (brace-list-open)
                                     (brace-entry-open)
                                     (substatement-open . after)
                                     (block-close . c-snug-do-while)
                                     (arglist-cont-nonempty))
             (c-cleanup-list brace-else-brace)
             (c-offsets-alist (statement-block-intro . +)
                              (knr-argdecl-intro . +)
                              (substatement . 0)
                              (substatement-open . 0)
                              (substatement-label . 0)
                              (statement-cont . +)
                              (case-label . 0)
                              (defun-open . 0)
                              (defun-close . 0)
                              (arglist-intro . +)
                              (arglist-cont +cc-lineup-arglist-close 0)
                              (arglist-cont-nonempty +cc-lineup-arglist-close 0)
                              (arglist-close +cc-lineup-arglist-close 0)
                              (template-args-cont +cc-lineup-arglist-close 0)
                              (inline-open . 0)
                              (inlambda . 0)
                              (innamespace . -)
                              (inexpr-class . 0)
                              (access-label . -)
                              (inclass +cc-c++-lineup-inclass +)
                              (inher-cont . c-lineup-multi-inher)
                              (label . 0))))

  (add-hook! prog-mode 'electric-indent-mode)
  (add-hook! c-mode-common-hook #'rainbow-delimiters-mode)
  (add-hook! (c-mode c++-mode) #'highlight-numbers-mode)
  (add-hook! (c-mode c++-mode) #'+cc|fontify-constants)
  (add-hook! (c-mode c++-mode) (lambda! (c-set-style "nx")))

  ;; Disable electric keys because it interferes with smartparens and
  ;; custom bindings. Do it manually instead.
  (setq c-tab-always-indent nil
        c-electric-flag nil)
  (dolist (key '("#" "{" "}" "/" "*" ";" "," ":" "(" ")" "\177"))
    (define-key c-mode-base-map key nil))

  ;; Smartparens and cc-mode both try to autoclose angle-brackets intelligently.
  ;; The result is...less intelligent than they think (redundant characters), so
  ;; do it manually.
  (map! :map c++-mode-map "<" nil ">" nil)

  ;; Employ smartparens for the job
  (sp-with-modes '(c++-mode)
    (sp-local-pair "<" ">"
                   :when '(+cc-sp-point-is-template-p +cc-sp-point-after-include-p)
                   :post-handlers '(("| " "SPC"))))
  (sp-with-modes '(c-mode c++-mode)
    (sp-local-pair "/*" "*/" :post-handlers '(("||\n[i]" "RET") ("| " "SPC")))
    ;; Doxygen blocks
    (sp-local-pair "/**" "*/" :post-handlers '(("||\n[i]" "RET") ("||\n[i]" "SPC")))
    (sp-local-pair "/*!" "*/" :post-handlers '(("||\n[i]" "RET") ("[d-1]< | " "SPC")))))

(def-package! modern-cpp-font-lock
  :hook (c++-mode . modern-c++-font-lock-mode))

;; (def-package! irony
;;   :after cc-mode
;;   :commands (irony-install-server irony-mode)
;;   :preface
;;   (setq irony-server-install-prefix (concat doom-etc-dir "irony-server/"))
;;   :init
;;   (add-hook! (c-mode c++-mode) #'irony-mode)
;;   :config
;;   (setq irony-cdb-search-directory-list '("." "build"))
;;   (add-hook 'irony-mode-hook #'+cc|irony-init-compile-options))
;; (def-package! company-irony :after irony)
;; (def-package! company-irony-c-headers :after company-irony)
;; (def-package! lsp-mode)
;; (def-package! company-lsp)
;; (def-package! cquery)

(def-package! cmake-mode)
(def-package! demangle-mode)
(def-package! disaster)
(def-package! ivy-xref)

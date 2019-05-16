;;; config.el -*- lexical-binding: t; -*-

(setq doom-font (font-spec :family "Fira Code Retina" :size 12)
      doom-theme 'doom-one
      tab-always-indent 'complete
      evil-cross-lines t
      evil-escape-key-sequence "kj"
      evil-want-fine-undo nil
      evil-move-cursor-back nil
      expand-region-contract-fast-key "V")

(when (display-graphic-p)
  (map! :keymap input-decode-map
        [(control ?i)] [control-i]
        [(control ?I)] [(shift control-i)]))

(map!
 :gnvie "C-c" nil
 :n     "C-h"       #'evil-window-left
 :n     "C-j"       #'evil-window-down
 :n     "C-k"       #'evil-window-up
 :n     "C-l"       #'evil-window-right
 :n     "C-x"       #'evil-numbers/dec-at-pt
 :i     [tab]       #'tab-to-tab-stop
 :i     [control-i] #'indent-for-tab-command
 :n     [control-i] #'evil-jump-forward)

(map! :leader
      :desc "Eval Last Sexp" "C-e" #'eval-last-sexp)

(map! :leader
      (:prefix ("l" . "lsp")
        :desc "Format Buffer"       "=" #'lsp-format-buffer
        :desc "Code Action"         "a" #'lsp-execute-code-action
        :desc "Sideline Mode"       "l" #'lsp-ui-sideline-mode
        :desc "Doc Mode"            "d" #'lsp-ui-doc-mode
        :desc "Diagnostics"         "e" #'lsp-ui-flycheck-list
        :desc "Imenu"               "i" #'lsp-ui-imenu
        :desc "Rename"              "r" #'lsp-rename
        :desc "Restart Workspace"   "R" #'lsp-restart-workspace
        :desc "Find Symbol"         "?" #'lsp-ui-peek-find-workspace-symbol))

(map! :localleader
      (:keymap (c-mode-map c++-mode-map)
        :prefix ("x" . "ccls")
        :desc "Variable Addresses"    "A" #'+ccls-references-address
        :desc "Function Addresses"    "F" #'+ccls-references-not-call
        :desc "Macro References"      "P" #'+ccls-references-macro
        :desc "Read References"       "R" #'+ccls-references-read
        :desc "Write References"      "W" #'+ccls-references-write
        :desc "Direct Bases"          "b" (lambda! (+ccls-base 1))
        :desc "All Bases"             "B" (lambda! (+ccls-base 3))
        :desc "Directed Derived"      "d" (lambda! (+ccls-derived 1))
        :desc "All Derived"           "D" (lambda! (+ccls-derived 3))
        :desc "Base Hierarchy"        "i" #'ccls-inheritance-hierarchy
        :desc "Derived Hierarchy"     "I" (lambda! (ccls-inheritance-hierarchy t))
        :desc "Callers"               "c" #'+ccls-caller
        :desc "Callee"                "C" #'+ccls-callee
        :desc "Caller Hierarchy"      "e" #'ccls-call-hierarchy
        :desc "Callee Hierarchy"      "E" (lambda! (ccls-call-hierarchy t))
        :desc "Nested Classes"        "s" (lambda! (+ccls-member 2))
        :desc "Member Functions"      "f" (lambda! (+ccls-member 3))
        :desc "Member Variables"      "m" (lambda! (+ccls-member 0))
        :desc "Member Hierarchy"      "M" #'ccls-member-hierarchy
        :desc "Local Variables"       "v" (lambda! (+ccls-vars 3))
        :desc "All Variables"         "V" (lambda! (+ccls-vars 7))
        :desc "Fields"                "k" (lambda! (+ccls-vars 1))
        :desc "Code Lens Mode"        "l" #'ccls-code-lens-mode
        :desc "Go To Type Definition" "t" #'lsp-goto-type-definition))

(after! smartparens
  (defhydra +smartparens-hydra (:hint nil)
    "
Sexps (quit with _q_)
 ^Nav^            ^Barf/Slurp^                  ^Depth^
 ^---^------------^----------^------------------^-----^-----------------
 _f_: forward     _→_:          slurp forward   _R_: splice
 _b_: backward    _←_:          slurp backward  _r_: raise
 _u_: backward ↑  _C-<right>_:   barf forward    _↑_: raise backward
 _d_: forward ↓   _C-<left>_:    barf backward   _↓_: raise forward
 _p_: backward ↓
 _n_: forward ↑
 ^Kill^           ^Misc^                        ^Wrap^
 ^----^-----------^----^------------------------^----^------------------
 _w_: copy        _j_: join                     _(_: wrap with ( )
 _k_: kill        _s_: split                    _{_: wrap with { }
 ^^               _t_: transpose                _'_: wrap with ' '
 ^^               _c_: convolute                _\"_: wrap with \" \"
 ^^               _i_: indent defun             _/_: unwrap"
    ("q" nil)
    ("(" (lambda (_) (interactive "P") (sp-wrap-with-pair "(")))
    ("{" (lambda (_) (interactive "P") (sp-wrap-with-pair "{")))
    ("[" (lambda (_) (interactive "P") (sp-wrap-with-pair "[")))
    ("'" (lambda (_) (interactive "P") (sp-wrap-with-pair "'")))
    ("\"" (lambda (_) (interactive "P") (sp-wrap-with-pair "\"")))
    ("/" #'sp-unwrap-sexp)
    ("f" #'sp-forward-sexp)
    ("b" #'sp-backward-sexp)
    ("u" #'sp-backward-up-sexp)
    ("d" #'sp-down-sexp)
    ("p" #'sp-backward-down-sexp)
    ("n" #'sp-up-sexp)
    ("w" #'sp-copy-sexp)
    ("k" #'sp-kill-sexp)
    ("t" #'sp-transpose-sexp)
    ("j" #'sp-join-sexp)
    ("s" #'sp-split-sexp)
    ("c" #'sp-convolute-sexp)
    ("i" #'sp-indent-defun)
    ("R" #'sp-splice-sexp)
    ("r" #'sp-splice-sexp-killing-around)
    ("<up>" #'sp-splice-sexp-killing-backward)
    ("<down>" #'sp-splice-sexp-killing-forward)
    ("<right>" #'sp-forward-slurp-sexp)
    ("C-<right>" #'sp-forward-barf-sexp)
    ("<left>" #'sp-backward-slurp-sexp)
    ("C-<left>" #'sp-backward-barf-sexp))

  (map!
   :n "C-;"       #'+smartparens-hydra/body
   :n "C-M-a"     #'sp-beginning-of-sexp
   :n "C-M-e"     #'sp-end-of-sexp
   :n "C-M-f"     #'sp-forward-sexp
   :n "C-M-b"     #'sp-backward-sexp
   :n "C-M-d"     #'sp-splice-sexp
   :n "C-M-k"     #'sp-kill-sexp
   :n "C-M-t"     #'sp-transpose-sexp
   :n "C-<right>" #'sp-forward-slurp-sexp
   :n "M-<right>" #'sp-forward-barf-sexp
   :n "C-<left>"  #'sp-backward-slurp-sexp
   :n "M-<left>"  #'sp-backward-barf-sexp))

(after! company
  (map! (:keymap company-active-map
          "M-s" #'counsel-company)))
